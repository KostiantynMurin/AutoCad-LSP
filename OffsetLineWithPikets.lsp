;; --- ВЕРСІЯ: ВЕРШИНА-ДО-ВЕРШИНИ (XY) + ВИБІР СТОРОНИ + ЗБЕРЕЖЕННЯ Z БЛОКУ + КОЛІР ЗМІЩЕННЯ --- ;; *** ОНОВЛЕНО ОПИС ***
;; Призначення:
;; 1. Зміщує вибрану Лінію (LINE) або Легку Полілінію (LWPOLYLINE) за допомогою команди OFFSET.
;; 2. Дозволяє користувачу вибрати сторону зміщення.
;; 3. Знаходить блоки "PIKET", які розташовані на вершинах ВИХІДНОЇ кривої,
;;    порівнюючи ТІЛЬКИ X та Y координати (з допуском vertexTol).
;; 4. Переміщує знайдені блоки на XY-координати відповідних вершин ЗМІЩЕНОЇ кривої.
;;    При цьому Z-координата блоку ЗБЕРІГАЄТЬСЯ (НЕ змінюється на Z зміщеної кривої). ;; *** ОНОВЛЕНО ОПИС ***
;; 5. Фарбує створену ЗМІЩЕНУ лінію/полілінію в колір RGB(39, 118, 187).
;;
;; Виклик: OFFSETLINEWITHPIKETS або OLWP

(vl-load-com) ; Переконатися, що VLISP функції доступні

;; --- Допоміжна функція для порівняння тільки X та Y координат у межах допуску ---
(defun points-equal-2d (pt1 pt2 tol)
  (if (and pt1 pt2 (= (length pt1) 3) (= (length pt2) 3) (> tol 0.0)) ; Перевірка вхідних даних
      (and
        (< (abs (- (car pt1) (car pt2))) tol)   ; Порівняти X
        (< (abs (- (cadr pt1) (cadr pt2))) tol) ; Порівняти Y
        ;; Z ігнорується
      )
      (progn (princ "\nПомилка у points-equal-2d: невірні аргументи.") nil) ; Повертаємо nil у разі помилки
  )
)


;; ============================================================
;; == ОСНОВНА ФУНКЦІЯ ==
;; ============================================================
(defun c:OffsetLineWithPikets ( / *error* oldEcho oldOsmode oldCmdDia oldOsmodeZ ; Додано oldOsmodeZ
                                vertexTol selEnt entData entType curveObj ssAllPikets i blkEnt blkData blkPtList_orig ssPiketsOnVertices idx vertexPt origCoords offsetDist offsetObjList newCurveObj offsetCoords targetVertexPt moveData vertCountMatch newCurveEnt entTypeExpected foundMatch colorObj acadObj acadDoc errorResult targetR targetG targetB trueColorValue newCurveData finalTargetPt ; Додано змінні
                               )
  ;; --- Локальна функція обробки помилок для відновлення налаштувань ---
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if oldCmdDia (setvar "CMDDIA" oldCmdDia)) ; Відновити діалоги
    (if oldOsmodeZ (setvar "OSNAPZ" oldOsmodeZ)) ; *** ВІДНОВИТИ OSNAPZ ***
    (if ssPiketsOnVertices (sssetfirst nil nil)) ; Зняти виділення
    (princ "\n*** Помилка LISP або скасовано: ")
    (if msg (princ msg))
    (princ " ***")
    (princ)
  )

;; --- Допоміжна функція для парсингу координат з Line та LWPolyline (ВИПРАВЛЕНО) ---
  (defun ParseRawCoords (obj / ptList currentPt n rawCoords coordCount isLWPoly isLine elevation objName objNameResult) ; Додано isLine
    (setq ptList nil
          ; Отримати ім'я об'єкта безпечно
          objNameResult (vl-catch-all-apply 'vla-get-ObjectName (list obj))
    )
    ; Перевірити, чи була помилка при отриманні імені
    (if (vl-catch-all-error-p objNameResult)
        (progn (princ "\nПомилка отримання імені об'єкта в ParseRawCoords.") nil) ; Якщо була помилка
        ; Якщо помилки НЕ було
        (progn
          ; *** ВИПРАВЛЕННЯ: Присвоїти результат напряму ***
          (setq objName objNameResult) ; objNameResult містить рядок типу "AcDbPolyline"

          ; Тепер використовуємо правильне ім'я objName
          (setq isLWPoly (= objName "AcDbPolyline")
                isLine (= objName "AcDbLine") ; *** Додано перевірку на LINE ***
                ; Отримати координати безпечно
                rawCoords (vl-catch-all-apply 'vla-get-Coordinates (list obj))
          )
          ; Перевірити, чи була помилка при отриманні координат
          (if (or (vl-catch-all-error-p rawCoords) (null rawCoords))
              (progn (princ "\nПомилка отримання координат з ParseRawCoords.") nil)
              ; Якщо координати отримано успішно
              (progn
                (setq rawCoords (vlax-safearray->list (vlax-variant-value rawCoords))
                      coordCount (length rawCoords)
                      n 0
                )
                (cond
                  ; Обробка LWPOLYLINE
                  ((and isLWPoly (> coordCount 0))
                   (setq elevation (vla-get-Elevation obj))
                   (while (< n coordCount)
                     (setq currentPt (list (nth n rawCoords) (nth (1+ n) rawCoords) elevation))
                     (setq ptList (cons currentPt ptList))
                     (setq n (+ n 2))
                   )
                  )
                  ; Обробка LINE
                  ((and isLine (> coordCount 0))
                   (while (< n coordCount)
                     (setq currentPt (list (nth n rawCoords) (nth (1+ n) rawCoords) (nth (+ n 2) rawCoords)))
                     (setq ptList (cons currentPt ptList))
                     (setq n (+ n 3))
                   )
                  )
                  ; Інші типи (якщо раптом пропустили перевірку при виборі)
                  (T (princ (strcat "\nУвага: Непідтримуваний тип об'єкта '" objName "' для ParseRawCoords")))
                )
                (reverse ptList) ; Повернути точки у правильному порядку
              )
          )
        )
    )
  ) ; кінець ParseRawCoords

  ;; --- Початок основної логіки ---

  ;; ============================================================
  ;; == НАЛАШТУВАННЯ ПАРАМЕТРІВ СКРИПТА ==
  ;; ============================================================
  ;; -- Цільовий колір для зміщеної лінії (RGB) --
  (setq targetR 39)  ; Червоний компонент (0-255)
  (setq targetG 118) ; Зелений компонент (0-255)
  (setq targetB 187) ; Синій компонент (0-255)

  ;; -- Допуск для порівняння XY координат блоку з вершиною --
  (setq vertexTol 0.01)
  ;; ============================================================

  (princ (strcat "\nЦільовий колір для зміщеної лінії: RGB(" (itoa targetR) "," (itoa targetG) "," (itoa targetB) ")"))
  (princ (strcat "\nВстановлено XY-допуск пошуку блоків на вершинах (vertexTol): " (rtos vertexTol 2 8)))

  ;; Зберегти поточні налаштування
  (setq oldEcho (getvar "CMDECHO"))
  (setq oldOsmode (getvar "OSMODE"))
  (setq oldCmdDia (getvar "CMDDIA"))
  (setq oldOsmodeZ (getvar "OSNAPZ")) ; *** ЗБЕРЕГТИ ПОТОЧНИЙ OSNAPZ ***
  (setvar "CMDECHO" 0) ; Вимикаємо ехо команд
  (setvar "CMDDIA" 0) ; Вимикаємо діалоги (для OFFSET + pause)
  (setvar "OSNAPZ" 0) ; *** Тимчасово встановлюємо OSNAPZ в 0, щоб гарантувати, що команда MOVE спрацює з 3D точками, які ми їй передамо ***
                     ; *** Наша логіка *сама* контролюватиме Z-координату ***

  ;; --- 1. Вибір лінії/полілінії ---
  (setq selEnt nil)
  (while (not selEnt)
    (setq selEnt (entsel "\nІванич! Вибери лінію або полілінію для зміщення: "))
    (if selEnt
      (progn
        (setq entData (entget (car selEnt))
              entType (cdr (assoc 0 entData)))
        (if (or (= entType "LINE") (= entType "LWPOLYLINE"))
          (setq curveObj (vlax-ename->vla-object (car selEnt))) ; Зберігаємо VLA об'єкт
          (progn
            (princ "\nВибраний об'єкт не є лінією або полілінією. Спробуйте ще раз.")
            (setq selEnt nil) ; Скинути вибір, щоб цикл продовжився
          )
        )
      )
      (*error* "Вибір скасовано користувачем") ; Вихід, якщо користувач натиснув Esc
    )
  )
  (princ (strcat "\nВибрано: " entType " <" (vl-princ-to-string (car selEnt)) ">"))

  ;; --- 2. Пошук блоків "PIKET" на ВЕРШИНАХ кривої (порівняння XY) ---
  (setq ssPiketsOnVertices (ssadd)) ; Ініціалізація порожнього набору для знайдених блоків
  (setq moveData nil)              ; Ініціалізація списку для даних переміщення
  (setq ssAllPikets (ssget "_X" '((0 . "INSERT") (2 . "PIKET")))) ; Знайти ВСІ блоки PIKET

  (setq origCoords (ParseRawCoords curveObj)) ; Отримуємо координати вершин ВИХІДНОЇ кривої

  (if (null origCoords)
      (*error* (strcat "Не вдалося отримати координати вершин для об'єкта " entType))
  )
  (princ (strcat "\nЗнайдено " (itoa (length origCoords)) " вершин у вихідній кривій."))

  (if ssAllPikets
    (progn
      (princ (strcat "\nЗнайдено " (itoa (sslength ssAllPikets)) " блоків 'PIKET'. Перевірка положення XY на вершинах (з допуском " (rtos vertexTol 2 8) ")..."))
      (setq i 0)
      (repeat (sslength ssAllPikets)
        (setq blkEnt (ssname ssAllPikets i))
        (setq blkData (entget blkEnt))
        (setq blkPtList_orig (cdr (assoc 10 blkData))) ; Оригінальна 3D точка вставки блоку

        (if blkPtList_orig ; Перевірка, чи вдалося отримати точку вставки
          (progn
            (setq idx 0 foundMatch nil)
            ;; Цикл по всіх вершинах вихідної кривої
            (while (and (< idx (length origCoords)) (not foundMatch))
              (setq vertexPt (nth idx origCoords)) ; 3D координата поточної вершини

              ;; ----> ОСЬ КЛЮЧОВЕ ПОРІВНЯННЯ <----
              ;; Використовуємо points-equal-2d для порівняння тільки X та Y
              (if (points-equal-2d blkPtList_orig vertexPt vertexTol)
                (progn
                  (princ (strcat "\n  -> [ЗНАЙДЕНО XY] Блок <" (vl-princ-to-string blkEnt) "> на вершині #" (itoa idx) " з OrigZ=" (rtos (caddr blkPtList_orig))))
                  (ssadd blkEnt ssPiketsOnVertices) ; Додати блок до набору знайдених
                  ;; Зберегти ім'я блоку, його ОРИГІНАЛЬНУ 3D точку, та індекс вершини
                  (setq moveData (cons (list blkEnt blkPtList_orig idx) moveData))
                  (setq foundMatch T) ; Позначити, що знайдено відповідність, і перейти до наступного блоку
                )
              ) ; кінець if порівняння
              (setq idx (1+ idx)) ; Наступний індекс вершини
            ) ; кінець while по вершинах
          )
          (princ (strcat "\nУвага: Не вдалося отримати точку вставки для блоку <" (vl-princ-to-string blkEnt) ">"))
        )
        (setq i (1+ i)) ; Наступний блок у ssAllPikets
      ) ; кінець repeat по блоках
      (setq moveData (reverse moveData)) ; Відновити порядок додавання
      (princ (strcat "\nЗнайдено " (itoa (sslength ssPiketsOnVertices)) " блоків на/біля XY вершин (з допуском " (rtos vertexTol 2 8) ")."))
    )
    (princ "\nБлоки з іменем 'PIKET' не знайдені у кресленні.")
  )

  ;; --- 3. Виділення знайдених блоків та запит відстані зміщення ---
  (if (> (sslength ssPiketsOnVertices) 0)
    (progn
      (princ (strcat "\nВиділення знайдених на вершинах блоків (" (itoa (sslength ssPiketsOnVertices)) " шт.)."))
      (sssetfirst nil ssPiketsOnVertices) ; Виділити знайдені блоки

      (setq offsetDist (getdist "\nІванич! Введи відстань зміщення: "))

      (if offsetDist
        (progn
          ;; --- 4. Виконання зміщення кривої за допомогою КОМАНДИ OFFSET ---
          (princ "\nВиконання зміщення за допомогою команди OFFSET...")
          (setvar "CMDECHO" 0) ; Переконатися, що ехо вимкнене
          (setq entTypeExpected entType) ; Запам'ятати очікуваний тип об'єкта

          ;; Запускаємо команду OFFSET, pause дозволяє користувачу вказати сторону
          (command "_offset" offsetDist (car selEnt) pause "")

          (setq newCurveEnt (entlast)) ; Отримуємо ОСТАННІЙ створений об'єкт

          ;; --- Перевірка та отримання нового об'єкта ---
          (if (or (null newCurveEnt)
                  ;; Перевіряємо, чи тип останнього об'єкта відповідає очікуваному
                  (/= (cdr (assoc 0 (entget newCurveEnt))) entTypeExpected)
                  ;; Перевіряємо, чи не той самий об'єкт (зміщення не вдалося)
                  (= (car selEnt) newCurveEnt)
              )
            (progn
              (princ "\n*** ПОМИЛКА: Не вдалося отримати коректний зміщений об'єкт після команди OFFSET.")
              (princ "\n    Можливо, команда була перервана, відстань нульова, або 'entlast' вказав не на той об'єкт.")
              (*error* "Помилка отримання результату зміщення")
            )
            ;; Якщо новий об'єкт коректний
            (progn
              ;; Отримуємо VLA-об'єкт нової зміщеної кривої
              (setq newCurveObj (vlax-ename->vla-object newCurveEnt))
              (princ (strcat "\n[OK] Зміщення командою OFFSET виконано. Новий об'єкт: <" (vl-princ-to-string newCurveEnt) ">"))

              ;; ==================================================================
              ;; ---> ПОЧАТОК: КОД ЗМІНИ КОЛЬОРУ ЗМІЩЕНОЇ ЛІНІЇ <---
              ;; ==================================================================
              (princ "\nЗміна властивостей зміщеного об'єкта через entmod (DXF)...")
              (if newCurveEnt
                  (progn
                    (setq newCurveData (entget newCurveEnt)) ; Отримати DXF дані

                    (if newCurveData
                      (progn
                        ;; --- Розрахунок значення TrueColor з RGB змінних ---
                        (setq trueColorValue (+ (* targetR 65536) (* targetG 256) targetB))

                        ;; --- 1. Встановлення True Color RGB ---
                        (princ (strcat "\n  Встановлення TrueColor (DXF 420) на " (itoa trueColorValue) "..."))
                        (if (assoc 420 newCurveData)
                          (setq newCurveData (subst (cons 420 trueColorValue) (assoc 420 newCurveData) newCurveData))
                          (setq newCurveData (append newCurveData (list (cons 420 trueColorValue))))
                        )

                        ;; --- 2. Встановлення основного кольору в ByLayer (DXF 62 = 256) ---
                        (princ "\n  Встановлення Color (DXF 62) на ByLayer (256)...")
                        (if (assoc 62 newCurveData)
                          (setq newCurveData (subst (cons 62 256) (assoc 62 newCurveData) newCurveData))
                          (setq newCurveData (append newCurveData (list (cons 62 256))))
                        )

                        ;; --- 3. Застосування змін ---
                        (if (entmod newCurveData)
                          (princ " [OK]")
                          (princ "\n*** ПОМИЛКА: entmod не вдалося оновити об'єкт.")
                        )
                      )
                      (princ "\n*** ПОМИЛКА: Не вдалося отримати DXF дані (entget) для зміщеного об'єкта.")
                    )
                  )
                  (princ "\n*** Увага: Немає імені об'єкта (newCurveEnt) для зміни властивостей.")
              )
              ;; ==================================================================
              ;; ---> КІНЕЦЬ: КОД ЗМІНИ КОЛЬОРУ ЗМІЩЕНОЇ ЛІНІЇ <---
              ;; ==================================================================

              ;; Отримуємо та парсимо координати вершин ЗМІЩЕНОЇ кривої
              (setq offsetCoords (ParseRawCoords newCurveObj))
              (if (null offsetCoords)
                  (*error* "Не вдалося отримати координати вершин зміщеної кривої!")
              )
              (princ (strcat "\nЗнайдено " (itoa (length offsetCoords)) " вершин у зміщеній кривій."))

              ;; Перевірка відповідності кількості вершин
              (setq vertCountMatch (= (length origCoords) (length offsetCoords)))
              (if (not vertCountMatch)
                  (princ "\n!!! ПОПЕРЕДЖЕННЯ: Кількість вершин у вихідній та зміщеній кривих не співпадає! Результат переміщення блоків може бути неточним.")
              )

              ;; --- 5. Переміщення блоків на відповідні вершини (Z буде ЗБЕРЕЖЕНО) --- ;; *** ОНОВЛЕНО КОМЕНТАР ***
              (princ "\nПереміщення блоків на XY відповідних вершин зміщеної лінії (ЗБЕРІГАЮЧИ Z)...") ; *** ОНОВЛЕНО ПОВІДОМЛЕННЯ ***
              (foreach dataItem moveData
                  (setq blkEnt (nth 0 dataItem))
                  (setq blkPtList_orig (nth 1 dataItem)) ; Оригінальна 3D точка блоку (Xo, Yo, Zo)
                  (setq idx (nth 2 dataItem))           ; Індекс відповідної вершини

                  (princ (strcat "\n  Обробка блоку <" (vl-princ-to-string blkEnt) "> з вершини #" (itoa idx)))
                  (princ (strcat "\n    Оригінальна точка блоку: " (vl-princ-to-string blkPtList_orig)))

                  (if (and blkPtList_orig (< idx (length offsetCoords))) ; Перевірка чи індекс дійсний і чи є оригінальна точка
                    (progn
                      ;; Отримуємо 3D точку відповідної вершини на зміщеній полілінії
                      (setq targetVertexPt (nth idx offsetCoords)) ; Це точка (X_new, Y_new, Z_new_line_elev)
                      (princ (strcat "\n    Вершина зміщ. кривої #" (itoa idx) ": " (vl-princ-to-string targetVertexPt)))

                      ;; ----> КОНСТРУЮВАННЯ ФІНАЛЬНОЇ ЦІЛЬОВОЇ ТОЧКИ ЗІ ЗБЕРЕЖЕНОЮ Z <----
                      (setq finalTargetPt (list
                                            (car targetVertexPt)     ; X з нової вершини
                                            (cadr targetVertexPt)    ; Y з нової вершини
                                            (caddr blkPtList_orig) ; Z з ОРИГІНАЛЬНОЇ точки блоку
                                          )
                      )
                      (princ (strcat "\n    Фінальна цільова точка (Xn, Yn, Zo): " (vl-princ-to-string finalTargetPt)))


                      ;; ----> ВИКОНУЄМО ПЕРЕМІЩЕННЯ НА СКОНСТРУЙОВАНУ ТОЧКУ <----
                      ;; Переміщення з оригінальної точки блоку ПРЯМО на нову точку (Xn, Yn, Zo).
                      ;; Z координата блоку ЗБЕРЕЖЕТЬСЯ, бо ми вказали її у finalTargetPt.
                      (command "_move" blkEnt "" "_none" blkPtList_orig "_none" finalTargetPt)
                      (princ "\n    [INFO] Команду MOVE виконано (ЗБЕРЕЖЕНО Z блоку).") ; *** ОНОВЛЕНО ПОВІДОМЛЕННЯ ***
                    )
                    (princ (strcat "\n    ПОМИЛКА: Індекс вершини #" (itoa idx) " недійсний для зміщеної кривої (макс: " (itoa (1- (length offsetCoords))) ") або відсутня оригінальна точка блоку. Блок не переміщено."))
                  )
              ) ; кінець foreach
              (princ "\n[OK] Спроба переміщення блоків на вершини зі збереженням Z завершена.") ; *** ОНОВЛЕНО ПОВІДОМЛЕННЯ ***
            ) ; кінець progn успішного зміщення
          ) ; кінець if перевірки entlast
        )
        (princ "\nВідстань не введено. Операцію скасовано.")
      ) ; кінець if offsetDist
    )
    (princ "\nНе знайдено блоків 'PIKET' на XY-координатах вершин вихідної кривої (з допуском " (rtos vertexTol 2 8) "). Нічого зміщувати.")
  ) ; кінець if > 0 знайдених блоків

  ;; --- 6. Очищення та відновлення налаштувань ---
  (if oldEcho (setvar "CMDECHO" oldEcho))
  (if oldOsmode (setvar "OSMODE" oldOsmode))
  (if oldCmdDia (setvar "CMDDIA" oldCmdDia)) ; Відновити діалоги
  (if oldOsmodeZ (setvar "OSNAPZ" oldOsmodeZ)) ; *** ВІДНОВИТИ ЗБЕРЕЖЕНИЙ OSNAPZ ***
  (sssetfirst nil nil) ; Зняти виділення
  (princ "\nСкрипт завершено.")
  (princ) ; Тихий вихід
) ; кінець defun c:OffsetLineWithPikets

;; ============================================================
;; == Визначення короткої команди для запуску ==
;; ============================================================
(defun c:OLWP () (c:OffsetLineWithPikets)) ;; OLWP - Offset Line With Pikets

(princ "\nLISP 'OffsetLineWithPikets' (XY Вершина-до-Вершини + Вибір сторони + ЗБЕРЕЖЕННЯ Z блоку + Колір зміщення) завантажено.") ; *** ОНОВЛЕНО ОПИС ***
(princ "\nДля запуску введіть OLWP або OFFSETLINEWITHPIKETS.")
(princ)