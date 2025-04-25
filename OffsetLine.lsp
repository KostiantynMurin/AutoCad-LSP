;;; OffsetBlue.lsp


;;; Створює зміщену копію лінії або полілінії на відстань 0.8,
;;; фарбує нову копію в чорний колір (індекс 7)
;;; та встановлює товщину лінії 0.30 мм.

(defun C:OffsetLineToBlack (/ *error* doc obj sel ptoff new_obj obj_data new_obj_data)
  ;; Функція обробки помилок
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nПомилка: " msg))
    )
    (princ) ; Тихий вихід
  )

  ;; Отримати активний документ (для новіших версій AutoCAD)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  ;; Почати операцію Undo (дозволяє відмінити всю дію команди)
  (vla-startundomark doc)

  ;; 1 & 2: Запит на вибір об'єкта (лінії або полілінії)
  (setq sel nil) ; Очистити вибір
  (while (not sel) ; Повторювати, доки не буде обрано правильний об'єкт
    (setq obj (entsel "\nОберіть лінію або полілінію для зміщення: "))
    (if obj
      (progn
        (setq obj_ent (car obj))
        (setq obj_data (entget obj_ent))
        (setq obj_type (cdr (assoc 0 obj_data))) ; Отримати тип об'єкта
        (if (or (= obj_type "LINE") (= obj_type "LWPOLYLINE") (= obj_type "POLYLINE") (= obj_type "SPLINE") (= obj_type "ARC") (= obj_type "CIRCLE") (= obj_type "ELLIPSE")) ; Перевірка типу
          (setq sel obj_ent) ; Зберегти ім'я сутності, якщо тип підходить
          (princ "\nОбраний об'єкт не підтримується для зміщення цією командою. Оберіть лінію або полілінію.")
        )
      )
      (progn ; Якщо нічого не обрано (натиснуто Esc або Enter)
        (princ "\nВибір скасовано.")
        (setq sel T) ; Встановити прапорець для виходу з циклу while
      )
    )
  )

  ;; Якщо об'єкт було успішно обрано (sel не T і не nil)
  (if (and sel (/= sel T))
    (progn
      ;; 3: Запит на вказівку сторони зміщення
      (setq ptoff (getpoint "\nВкажіть точку на стороні зміщення: "))
      (if ptoff
        (progn
          ;; 4: Виконання команди зміщення (OFFSET)
          ;; Використовуємо command для виклику стандартної команди AutoCAD
          ;; "_OFFSET" - команда зміщення ( _ для англійської версії)
          ;; 0.8      - відстань зміщення
          ;; sel      - об'єкт для зміщення (його ім'я сутності)
          ;; ptoff    - точка, що вказує сторону
          ;; ""       - завершення команди OFFSET
          (command "_OFFSET" 0.8 sel ptoff "")

          ;; 5: Отримання останнього створеного об'єкта та зміна його кольору
          (setq new_obj (entlast))
          (if (and new_obj (/= new_obj sel))
            (progn
              (setq new_obj_data (entget new_obj))

              ;; --- Встановлення True Color Чорний (RGB 0,0,0) ---
              ;; Використовуємо DXF код 420 для True Color.
              ;; Значення - 32-бітне ціле число 0x00RRGGBB. Для чорного (0,0,0) це 0.
              (if (assoc 420 new_obj_data)
                (setq new_obj_data (subst '(420 . 0) (assoc 420 new_obj_data) new_obj_data))
                (setq new_obj_data (append new_obj_data '((420 . 0))))
              )
              ;; Щоб гарантувати, що True Color має пріоритет,
              ;; встановлюємо індексний колір (62) в ByLayer (256).
              (if (assoc 62 new_obj_data)
                  (setq new_obj_data (subst '(62 . 256) (assoc 62 new_obj_data) new_obj_data))
                  (setq new_obj_data (append new_obj_data '((62 . 256))))
              )

              ;; --- Зміна товщини лінії на 0.30 мм (залишається) ---
              ;; DXF код 370, значення 30 для 0.30 мм
              (if (assoc 370 new_obj_data)
                 (setq new_obj_data (subst '(370 . 30) (assoc 370 new_obj_data) new_obj_data))
                 (setq new_obj_data (append new_obj_data '((370 . 30))))
              )

              ;; Застосувати зміни до об'єкта
              (entmod new_obj_data)
              (princ (strcat "\nОб'єкт зміщено на 0.8, пофарбовано в чорний та встановлено товщину 0.30 мм."))
            )

            (princ "\nНе вдалося створити зміщений об'єкт.")
          )
        )
        (princ "\nВказівка точки скасована.")
      )
    )
    (if (not (eq sel T)) (princ "\nНе вдалося обрати об'єкт.")) ; Повідомлення, якщо вибір не вдався
  )

  ;; Завершити операцію Undo
  (vla-endundomark doc)

  (princ) ; Тихий вихід з функції (не показувати повернене значення)
)

;; ============================================================
;; == Визначення короткої команди для запуску ==
;; ============================================================
(defun c:OLTB () (c:OffsetLineToBlack)) ;; OLB - OffsetLineBlue

;; Повідомлення про завантаження
(princ "\nLISP скрипт 'OffsetLine.lsp' завантажено. Введіть OLTB або OFFSETLINETODLACK щоб змістити лінію та змінити колір на чорний з вагою 0.30мм.")
(princ)