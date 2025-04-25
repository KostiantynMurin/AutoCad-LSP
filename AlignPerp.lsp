;; LISP Скрипт для AutoCAD: Вирівнювання Об'єкта Перпендикулярно в Точці Перетину
;; Ім'я команди: AlignPerp
;; Автор: AI Assistant (Gemini)
;; Дата: 2025-04-14
;; Версія: 2.1 (Українська локалізація, підтримка Блоків, Ліній, Поліліній)
;; Опис:
;;   1. Користувач ВИБИРАЄ ОДИН БЛОК, ЛІНІЮ або ПОЛІЛІНІЮ.
;;   2. Користувач запускає команду AlignPerp.
;;   3. Скрипт шукає інші лінії або полілінії, що перетинають вибраний об'єкт.
;;   4. Якщо перетин знайдено:
;;      - Для БЛОКІВ та ЛІНІЙ: скрипт повертає об'єкт навколо точки перетину
;;        так, щоб він став перпендикулярним до іншої лінії/полілінії в цій точці.
;;        Об'єкт НЕ переміщується, лише повертається.
;;      - Для ПОЛІЛІНІЙ: поворот НЕ виконується (видається попередження),
;;        оскільки результат може бути непередбачуваним.
;;

(vl-load-com) ; Переконуємось, що VLA функції доступні

(defun c:AlignPerp (/ *error* oldCmdEcho oldOsmode selSet objEnt objVla objType minPt maxPt expFactor p1 p2 ssCandidates i candEnt candVla intPoints intPtArr intPt param tangentVec tangentAng perpAng currentAng rotAngleRad rotAngleDegrees foundIntersection intersectionProcessed)

  ;; --- Обробник помилок ---
  (defun *error* (msg)
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho))
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if (and *acadDoc* (not (vlax-object-released-p *acadDoc*)))
        (vla-EndUndoMark *acadDoc*)
    )
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (princ (strcat "\nПомилка: " msg))
    )
    (princ (strcat "\nВиконання " command-name " скасовано."))
    (princ) ; Тихий вихід
  )

  ;; --- Основна Функція ---
  (setq command-name "AlignPerp") ; Зберігаємо ім'я команди для повідомлень
  (setq oldCmdEcho (getvar "CMDECHO"))
  (setq oldOsmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)

  (setq *acadDoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark *acadDoc*)

  (setq foundIntersection nil)     ; Прапорець, що знайдено хоча б один перетин
  (setq intersectionProcessed nil) ; Прапорець, що перетин було оброблено (з поворотом або попередженням)

  ;; --- Отримання попередньо вибраного об'єкта ---
  (prompt (strcat "\nОчікується ОДИН попередньо вибраний БЛОК, ЛІНІЯ або ПОЛІЛІНІЯ... Запустіть " command-name))
  (setq selSet (ssget "_P"))

  ;; --- Перевірка вибору ---
  (if (or (not selSet) (/= (sslength selSet) 1))
    (progn (princ "\nБудь ласка, виберіть РІВНО ОДИН об'єкт (Блок, Лінію або Полілінію) перед запуском команди.") (*error* nil))
  )

  (setq objEnt (ssname selSet 0))
  (setq objVla (vlax-ename->vla-object objEnt))
  (setq objType (strcase (vla-get-ObjectName objVla))) ; Отримуємо тип об'єкта в верхньому регістрі

  ;; Перевірка типу об'єкта
  (if (not (wcmatch objType "*BLOCK*,*LINE*")) ; *LINE* включає LINE та LWPOLYLINE
      (progn (princ "\nВибраний об'єкт не є Блоком, Лінією або Полілінією.") (*error* nil))
  )

  (princ (strcat "\nОбробка об'єкта: " (vl-princ-to-string objType)))

  ;; --- Отримання габаритного контейнера об'єкта для пошуку ліній поруч ---
  (vl-catch-all-apply 'vla-GetBoundingBox (list objVla 'minPt 'maxPt))
  (if (or (not minPt) (not maxPt))
      ;; Спроба отримати координати для ліній/поліліній, якщо GetBoundingBox не спрацював
      (if (wcmatch objType "*LINE*")
          (progn
            (setq minPt (vlax-get objVla (if (wcmatch objType "*POLYLINE*") 'Coordinates 'StartPoint)))
            (setq maxPt (vlax-get objVla (if (wcmatch objType "*POLYLINE*") 'Coordinates 'EndPoint)))
            ;; Для поліліній координати - це список, треба знайти min/max
            (if (wcmatch objType "*POLYLINE*")
                (let ((coords (vlax-safearray->list (vlax-variant-value minPt))) x y minx miny maxx maxy)
                     (setq minx (car coords) miny (cadr coords) maxx minx maxy miny)
                     (while coords
                       (setq x (car coords) y (cadr coords))
                       (if (< x minx) (setq minx x))
                       (if (< y miny) (setq miny y))
                       (if (> x maxx) (setq maxx x))
                       (if (> y maxy) (setq maxy y))
                       (setq coords (cddr coords))
                     )
                     (setq minPt (list minx miny 0.0) maxPt (list maxx maxy 0.0)) ; Припускаємо 2D
                )
                ;; Для лінії StartPoint/EndPoint вже є точками
                (setq minPt (list (apply 'min (list (car minPt) (car maxPt))) (apply 'min (list (cadr minPt) (cadr maxPt))) 0.0))
                (setq maxPt (list (apply 'max (list (car minPt) (car maxPt))) (apply 'max (list (cadr minPt) (cadr maxPt))) 0.0))
            )
          )
          (progn (princ "\nНе вдалося отримати габаритний контейнер або координати об'єкта.") (*error* nil))
      )
  )


  ;; Розширюємо область пошуку трохи за межі габаритів
  (setq expFactor 1.0)
  (setq p1 (list (- (car minPt) expFactor) (- (cadr minPt) expFactor) (caddr minPt)))
  (setq p2 (list (+ (car maxPt) expFactor) (+ (cadr maxPt) expFactor) (caddr maxPt)))

  ;; --- Пошук ЛІНІЙ та ПОЛІЛІНІЙ у розширеній області ---
  (princ "\nПошук ліній/поліліній, що перетинають вибраний об'єкт...")
  (setq ssCandidates (ssget "_C" p1 p2 '((0 . "*LINE")))) ; Вибираємо LINE та LWPOLYLINE

  (if (not ssCandidates)
    (progn (princ "\nПоблизу вибраного об'єкта не знайдено ліній або поліліній для перевірки перетину.") (*error* nil))
  )

  ;; --- Ітерація по знайдених лініях/полілініях для пошуку перетину ---
  (setq i 0)
  (while (and (< i (sslength ssCandidates)) (not intersectionProcessed)) ; Зупиняємось після першої успішної обробки
    (setq candEnt (ssname ssCandidates i))
    (setq i (1+ i))

    ;; Пропускаємо сам вибраний об'єкт
    (if (/= objEnt candEnt)
      (progn
        (setq candVla (vlax-ename->vla-object candEnt))
        (princ (strcat "\n  Перевірка перетину з: " (vla-get-ObjectName candVla)))

        (setq intPoints (vl-catch-all-apply 'vlax-invoke-method (list objVla 'IntersectWith candVla acExtendNone)))

        (if (vl-catch-all-error-p intPoints)
            (princ (strcat "\n    Помилка при перетині: " (vl-catch-all-error-message intPoints)))
          (if (and intPoints (not (vl-catch-all-error-p intPoints)) (= (vlax-variant-type intPoints) (+ vlax-vbArray vlax-vbDouble)))
             (progn
                (setq intPtArr (vlax-safearray->list (vlax-variant-value intPoints)))
                (if (> (length intPtArr) 0)
                  (progn
                    (setq intPt (list (car intPtArr) (cadr intPtArr) (caddr intPtArr)))
                    (princ (strcat "\n    Знайдено перетин в точці: " (vl-princ-to-string intPt)))
                    (setq foundIntersection T) ; Позначка, що перетин існує

                    ;; --- Отримання дотичної та перпендикуляра КАНДИДАТА в точці перетину ---
                    (setq param (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list candVla intPt)))
                    (if (vl-catch-all-error-p param)
                      (princ (strcat "\n      Помилка отримання параметра кривої-кандидата: " (vl-catch-all-error-message param)))
                      (progn
                        (setq tangentVec (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list candVla param)))
                        (if (vl-catch-all-error-p tangentVec)
                          (princ (strcat "\n      Помилка отримання вектора дотичної кандидата: " (vl-catch-all-error-message tangentVec)))
                          (if (< (distance '(0 0 0) tangentVec) 1e-9)
                            (princ "\n      Неможливо визначити дотичну кандидата в цій точці.")
                            (progn
                              ;; --- Розрахунок перпендикулярного кута ---
                              (setq tangentAng (angle '(0 0 0) tangentVec))
                              (setq perpAng (+ tangentAng (/ pi 2.0)))

                              ;; --- Обробка повороту в залежності від типу ВИБРАНОГО об'єкта ---
                              (cond
                                ((wcmatch objType "*BLOCK*")
                                 (setq currentAng (vla-get-Rotation objVla)) ; Кут блоку (радіани)
                                 (setq rotAngleRad (- perpAng currentAng))
                                 (setq rotAngleDegrees (/ (* rotAngleRad 180.0) pi))
                                 (princ (strcat "\n    Поворот БЛОКУ навколо точки перетину на " (rtos rotAngleDegrees 2 4) " градусів."))
                                 (command "_.ROTATE" objEnt "" "_non" intPt rotAngleDegrees)
                                 (setq intersectionProcessed T) ; Позначаємо, що обробили
                                )
                                ((wcmatch objType "ACDBLINE") ; Тільки для LINE (не LWPOLYLINE)
                                 (setq currentAng (vla-get-Angle objVla)) ; Кут лінії (радіани)
                                 (setq rotAngleRad (- perpAng currentAng))
                                 (setq rotAngleDegrees (/ (* rotAngleRad 180.0) pi))
                                 (princ (strcat "\n    Поворот ЛІНІЇ навколо точки перетину на " (rtos rotAngleDegrees 2 4) " градусів."))
                                 (command "_.ROTATE" objEnt "" "_non" intPt rotAngleDegrees)
                                 (setq intersectionProcessed T) ; Позначаємо, що обробили
                                )
                                ((wcmatch objType "*POLYLINE*") ; Для LWPOLYLINE
                                 (princ "\n    ПОПЕРЕДЖЕННЯ: Вибраний об'єкт - Полілінія. Поворот поліліній не виконується цим скриптом.")
                                 (setq intersectionProcessed T) ; Позначаємо, що обробили (попередженням)
                                )
                                (T ; Інші можливі типи (малоймовірно через фільтр)
                                 (princ (strcat "\n    Невідомий тип об'єкта для повороту: " objType))
                                 (setq intersectionProcessed T) ; Позначаємо, щоб вийти з циклу
                                )
                              );; cond
                            ) ; progn - розрахунок кутів і поворот/попередження
                          ) ; if - перевірка довжини вектора дотичної
                        ) ; if - помилка отримання вектора дотичної
                      ) ; progn - отримання вектора дотичної
                    ) ; if - помилка отримання параметра

                  ) ; progn - обробка точки перетину
                  (princ "\n    Масив точок перетину порожній.")
                ) ; if length > 0
             ) ; progn - перевірка типу variant
             (princ "\n    Перетин не знайдено або повернуто некоректний тип даних.")
          ) ; if - перевірка типу variant/помилки
        ) ; if - помилка методу IntersectWith
      ) ; progn - обробка кандидата
    ) ; if - пропускаємо сам об'єкт
  ) ; while

  ;; --- Завершення ---
  (if (not foundIntersection)
      (princ "\nНе знайдено перетинів між вибраним об'єктом та лініями/полілініями поблизу.")
      (if intersectionProcessed
          (princ (strcat "\nОбробку перетину завершено для об'єкта типу: " objType))
          (princ "\nЗнайдено перетин, але виникла помилка при обробці дотичної/кутів.")
      )
  )

  (setvar "CMDECHO" oldCmdEcho)
  (setvar "OSMODE" oldOsmode)
  (vla-EndUndoMark *acadDoc*)

  (setq *acadDoc* nil)
  (princ) ; Тихий вихід
)

;; --- Кінець скрипта ---
(princ (strcat "\nLISP скрипт AlignPerp завантажено. Спочатку ВИБЕРІТЬ БЛОК, ЛІНІЮ або ПОЛІЛІНІЮ, потім введіть ALIGNPERP для запуску."))
(princ)