;; --- ВЕРСІЯ: ВЕРШИНА-ДО-ВЕРШИНИ (XY) + ВИБІР СТОРОНИ + ЗБЕРЕЖЕННЯ Z + КОЛІР + ЗАЛИШИТИ ВИДІЛЕНИМИ --- ;; *** ОНОВЛЕНО ОПИС ***
;; Призначення:
;; 1. Зміщує вибрану Лінію (LINE) або Легку Полілінію (LWPOLYLINE).
;; 2. Дозволяє користувачу вибрати сторону зміщення.
;; 3. Знаходить блоки (ім'я: *olwp-PiketBlockName*) на вершинах ВИХІДНОЇ кривої (порівняння XY).
;; 4. Переміщує знайдені блоки на XY-координати відповідних вершин ЗМІЩЕНОЇ кривої, зберігаючи Z блоку.
;; 5. Фарбує ЗМІЩЕНУ криву в колір (RGB: *olwp-TargetColorR*, G, B).
;; 6. *** Після успішного завершення залишає ПЕРЕМІЩЕНІ блоки ВИДІЛЕНИМИ. ***
;;
;; Виклик: OFFSETLINEWITHPIKETS або OLWP
;;
;; *** Для зміни імені блоку або кольору змініть значення глобальних змінних нижче ***

(vl-load-com) ; Переконатися, що VLISP функції доступні

;; ============================================================
;; == ГЛОБАЛЬНІ НАЛАШТУВАННЯ СКРИПТА (Редагуйте ці значення) ==
;; ============================================================
;; -- Ім'я блоку, який шукатимемо на вершинах --
(if (not *olwp-PiketBlockName*) ; Якщо змінна ще не визначена
    (setq *olwp-PiketBlockName* "PIKET") ; Встановити ім'я блоку за замовчуванням
)
;; -- Цільовий колір для зміщеної лінії (RGB) --
(if (not *olwp-TargetColorR*) (setq *olwp-TargetColorR* 39))   ; Червоний компонент (0-255)
(if (not *olwp-TargetColorG*) (setq *olwp-TargetColorG* 118))  ; Зелений компонент (0-255)
(if (not *olwp-TargetColorB*) (setq *olwp-TargetColorB* 187))  ; Синій компонент (0-255)
;; -- Глобальна змінна для запам'ятовування останньої відстані зміщення --
(if (or (null (boundp '*olwp-LastOffsetDist*)) (null *olwp-LastOffsetDist*) (not (numberp *olwp-LastOffsetDist*)))
          (setq *olwp-LastOffsetDist* 0.8)
      ) ; Значення за замовчуванням 0,8
;; ============================================================

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
(defun c:OffsetLineWithPikets ( / *error* oldEcho oldOsmode oldCmdDia oldOsmodeZ
                                  vertexTol selEnt entData entType curveObj ssAllPikets i blkEnt blkData blkPtList_orig ssPiketsOnVertices idx vertexPt origCoords offsetDist offsetObjList newCurveObj offsetCoords targetVertexPt moveData vertCountMatch newCurveEnt entTypeExpected foundMatch colorObj acadObj acadDoc errorResult trueColorValue newCurveData finalTargetPt blockNameFilter wasSuccessful inputOffsetDist prompt_offset_str  
                                 )
  (setq wasSuccessful nil) ; Прапорець успішного завершення операцій з блоками

  ;; --- Локальна функція обробки помилок для відновлення налаштувань ---
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if oldCmdDia (setvar "CMDDIA" oldCmdDia)) ; Відновити діалоги
    (if oldOsmodeZ (setvar "OSNAPZ" oldOsmodeZ)) ; *** ВІДНОВИТИ OSNAPZ ***
    (sssetfirst nil nil) ; *** ЗНЯТИ ВИДІЛЕННЯ ПРИ ПОМИЛЦІ АБО СКАСУВАННІ ***
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
  ;; == НАЛАШТУВАННЯ ПАРАМЕТРІВ СКРИПТА (з глобальних змінних) ==
  ;; ============================================================
  ;; -- Допуск для порівняння XY координат блоку з вершиною --
  (setq vertexTol 0.01)
  ;; ============================================================

  ;; Використовуємо глобальні змінні для кольору та імені блоку
  (princ (strcat "\nШукаємо блоки з іменем: '" *olwp-PiketBlockName* "'"))
  (princ (strcat "\nЦільовий колір для зміщеної лінії: RGB(" (itoa *olwp-TargetColorR*) "," (itoa *olwp-TargetColorG*) "," (itoa *olwp-TargetColorB*) ")"))
  (princ (strcat "\nВстановлено XY-допуск пошуку блоків на вершинах (vertexTol): " (rtos vertexTol 2 8)))

  ;; Зберегти поточні налаштування
  (setq oldEcho (getvar "CMDECHO"))
  (setq oldOsmode (getvar "OSMODE"))
  (setq oldCmdDia (getvar "CMDDIA"))
  (setq oldOsmodeZ (getvar "OSNAPZ")) ; *** ЗБЕРЕГТИ ПОТОЧНИЙ OSNAPZ ***
  (setvar "CMDECHO" 0) ; Вимикаємо ехо команд
  (setvar "CMDDIA" 0) ; Вимикаємо діалоги (для OFFSET + pause)
  (setvar "OSNAPZ" 0) ; *** Тимчасово встановлюємо OSNAPZ в 0, щоб гарантувати, що команда MOVE спрацює з 3D точками, які ми їй передамо ***

  ;; --- 1. Вибір лінії/полілінії ---
  (setq selEnt nil)
  (while (not selEnt)
    (setq selEnt (entsel "\nВиберіть лінію або полілінію для зміщення: "))
    (if selEnt
      (progn
        (setq entData (entget (car selEnt))
              entType (cdr (assoc 0 entData)))
        (if (or (= entType "LINE") (= entType "LWPOLYLINE") (= entType "POLYLINE"))
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

  ;; --- 2. Пошук блоків (з іменем з *olwp-PiketBlockName*) на ВЕРШИНАХ кривої (порівняння XY) ---
  (setq ssPiketsOnVertices (ssadd)) ; Ініціалізація порожнього набору для знайдених блоків
  (setq moveData nil)             ; Ініціалізація списку для даних переміщення
  ;; Створюємо фільтр для ssget, використовуючи глобальну змінну *olwp-PiketBlockName*
  (setq blockNameFilter (list '(0 . "INSERT") (cons 2 *olwp-PiketBlockName*)))
  (setq ssAllPikets (ssget "_X" blockNameFilter)) ; Знайти ВСІ блоки з потрібним іменем

  (setq origCoords (ParseRawCoords curveObj)) ; Отримуємо координати вершин ВИХІДНОЇ кривої

  (if (null origCoords)
      (*error* (strcat "Не вдалося отримати координати вершин для об'єкта " entType))
  )
  (princ (strcat "\nЗнайдено " (itoa (length origCoords)) " вершин у вихідній кривій."))

  (if ssAllPikets
    (progn
      (princ (strcat "\nЗнайдено " (itoa (sslength ssAllPikets)) " блоків '" *olwp-PiketBlockName* "'. Перевірка положення XY на вершинах (з допуском " (rtos vertexTol 2 8) ")..."))
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
      (princ (strcat "\nЗнайдено " (itoa (sslength ssPiketsOnVertices)) " блоків '" *olwp-PiketBlockName* "' на/біля XY вершин (з допуском " (rtos vertexTol 2 8) ")."))
    )
    (princ (strcat "\nБлоки з іменем '" *olwp-PiketBlockName* "' не знайдені у кресленні.")) ; Використовуємо глобальну змінну
  )

  ;; --- 3. Виділення знайдених блоків та запит відстані зміщення ---
  (if (> (sslength ssPiketsOnVertices) 0)
    (progn
      (princ (strcat "\nВиділення знайдених на вершинах блоків (" (itoa (sslength ssPiketsOnVertices)) " шт.)."))
      (sssetfirst nil ssPiketsOnVertices) ; Виділити знайдені блоки (попередньо)

      ; Формування підказки зі збереженим значенням
      (setq prompt_offset_str (strcat "\nВведіть відстань зміщення <" (rtos *olwp-LastOffsetDist* 2 4) ">: "))
      ; (initget) ; Можна не використовувати initget тут, якщо дозволяємо будь-яке числове введення
                ; або (initget 6) якщо хочемо заборонити 0 і негативні (але для offset це може бути потрібно)
      ; Отримати відстань - getdist поверне nil, якщо натиснуто Enter або Esc
      (setq inputOffsetDist (getdist prompt_offset_str)) ; ВИПРАВЛЕНО: Тільки один аргумент-рядок

      ; Використання введеного значення або попереднього (якщо натиснуто Enter)
      (if inputOffsetDist
          (setq offsetDist inputOffsetDist) ; Користувач ввів валідне значення
          (progn ; Користувач натиснув Enter (inputOffsetDist = nil)
             (princ (strcat " Використовується останнє значення: " (rtos *olwp-LastOffsetDist* 2 4)))
             (setq offsetDist *olwp-LastOffsetDist*) ; Присвоюємо збережене значення
          )
      )

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
              (*error* "Помилка отримання результату зміщення") ; Виклик *error* зніме виділення
            )
            ;; Якщо новий об'єкт коректний
            (progn
              ;; Отримуємо VLA-об'єкт нової зміщеної кривої
              (setq newCurveObj (vlax-ename->vla-object newCurveEnt))
              (princ (strcat "\n[OK] Зміщення командою OFFSET виконано. Новий об'єкт: <" (vl-princ-to-string newCurveEnt) ">"))

              ;; ==================================================================
              ;; ---> ПОЧАТОК: КОД ЗМІНИ КОЛЬОРУ ЗМІЩЕНОЇ ЛІНІЇ (з глобальних змінних) <---
              ;; ==================================================================
              (princ "\nЗміна властивостей зміщеного об'єкта через entmod (DXF)...")
              (if newCurveEnt
                  (progn
                    (setq newCurveData (entget newCurveEnt)) ; Отримати DXF дані

                    (if newCurveData
                      (progn
                        ;; --- Розрахунок значення TrueColor з ГЛОБАЛЬНИХ RGB змінних ---
                        (setq trueColorValue (+ (* *olwp-TargetColorR* 65536) (* *olwp-TargetColorG* 256) *olwp-TargetColorB*))

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
                  (*error* "Не вдалося отримати координати вершин зміщеної кривої!") ; Виклик *error* зніме виділення
              )
              (princ (strcat "\nЗнайдено " (itoa (length offsetCoords)) " вершин у зміщеній кривій."))

              ;; Перевірка відповідності кількості вершин
              (setq vertCountMatch (= (length origCoords) (length offsetCoords)))
              (if (not vertCountMatch)
                  (princ "\n!!! ПОПЕРЕДЖЕННЯ: Кількість вершин у вихідній та зміщеній кривих не співпадає! Результат переміщення блоків може бути неточним.")
              )

              ;; --- 5. Переміщення блоків на відповідні вершини (Z буде ЗБЕРЕЖЕНО) ---
              (princ "\nПереміщення блоків на XY відповідних вершин зміщеної лінії (ЗБЕРІГАЮЧИ Z)...")
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
                                            (car targetVertexPt)    ; X з нової вершини
                                            (cadr targetVertexPt)   ; Y з нової вершини
                                            (caddr blkPtList_orig) ; Z з ОРИГІНАЛЬНОЇ точки блоку
                                          )
                      )
                      (princ (strcat "\n    Фінальна цільова точка (Xn, Yn, Zo): " (vl-princ-to-string finalTargetPt)))


                      ;; ----> ВИКОНУЄМО ПЕРЕМІЩЕННЯ НА СКОНСТРУЙОВАНУ ТОЧКУ <----
                      (command "_move" blkEnt "" "_none" blkPtList_orig "_none" finalTargetPt)
                      (princ "\n    [INFO] Команду MOVE виконано (ЗБЕРЕЖЕНО Z блоку).")
                    )
                    (princ (strcat "\n    ПОМИЛКА: Індекс вершини #" (itoa idx) " недійсний для зміщеної кривої (макс: " (itoa (1- (length offsetCoords))) ") або відсутня оригінальна точка блоку. Блок не переміщено."))
                  )
              ) ; кінець foreach
              (princ "\n[OK] Спроба переміщення блоків на вершини зі збереженням Z завершена.")
              (setq wasSuccessful T) ; *** Позначити, що операції з блоками пройшли успішно ***

              ;; *** ВСТАНОВИТИ ФІНАЛЬНЕ ВИДІЛЕННЯ ТІЛЬКИ ПЕРЕМІЩЕНИХ БЛОКІВ ***
              (if wasSuccessful
                  (progn
                     (princ (strcat "\nЗалишення виділеними переміщених блоків (" (itoa (sslength ssPiketsOnVertices)) " шт.)..."))
                     (sssetfirst nil ssPiketsOnVertices)
                   )
              )
            ) ; кінець progn успішного зміщення
          ) ; кінець if перевірки entlast
        )
        (princ "\nВідстань не введено. Операцію скасовано.") ; Тут *error* не викликається, виділення зніметься в кінці
      ) ; кінець if offsetDist
    )
    (princ (strcat "\nНе знайдено блоків '" *olwp-PiketBlockName* "' на XY-координатах вершин вихідної кривої (з допуском " (rtos vertexTol 2 8) "). Нічого зміщувати.")) ; Блоки не знайдено, виділення буде знято в кінці
  ) ; кінець if > 0 знайдених блоків

  ;; --- 6. Очищення та відновлення налаштувань ---
  (if oldEcho (setvar "CMDECHO" oldEcho))
  (if oldOsmode (setvar "OSMODE" oldOsmode))
  (if oldCmdDia (setvar "CMDDIA" oldCmdDia)) ; Відновити діалоги
  (if oldOsmodeZ (setvar "OSNAPZ" oldOsmodeZ)) ; *** ВІДНОВИТИ ЗБЕРЕЖЕНИЙ OSNAPZ ***

  ;; *** ЗНЯТИ ВИДІЛЕННЯ, ТІЛЬКИ ЯКЩО ОСНОВНА ОПЕРАЦІЯ З БЛОКАМИ НЕ БУЛА УСПІШНОЮ ***
  (if (not wasSuccessful)
      (sssetfirst nil nil)
  )

  (princ "\nСкрипт завершено.")
  (if wasSuccessful (princ " Переміщені блоки залишено виділеними."))
  (princ) ; Тихий вихід
) ; кінець defun c:OffsetLineWithPikets

;; ============================================================
;; == Визначення короткої команди для запуску ==
;; ============================================================
(defun c:OLWP () (c:OffsetLineWithPikets)) ;; OLWP - Offset Line With Pikets

(princ (strcat "\nLISP 'OffsetLineWithPikets' (з глоб. налаштуваннями блоку: '" *olwp-PiketBlockName* "' та кольору RGB: " (itoa *olwp-TargetColorR*) "," (itoa *olwp-TargetColorG*) "," (itoa *olwp-TargetColorB*) ") завантажено."))
(princ "\nДля запуску введіть OLWP або OFFSETLINEWITHPIKETS.")
(princ "\n*** Після успішного виконання переміщені блоки залишаться виділеними. ***")
(princ "\n*** Щоб змінити ім'я блоку або колір, відредагуйте глобальні змінні на початку файлу .lsp ***")
(princ)