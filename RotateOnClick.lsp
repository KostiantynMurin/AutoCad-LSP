;;; ROTATEONCLICK - Обертає обраний об'єкт на 180 градусів навколо його точки вставки/центра
;;; при кожному кліку мишею.

(defun c:ROTATEONCLICK ( / ent sel_obj ent_data ent_type ins_pt pick_point p1 p2)
  (setvar "CMDECHO" 0) ; Вимкнути відображення команд у командному рядку

  (while
    (setq sel_obj (entsel "\nОберіть об'єкт для розвороту на 180° (або натисніть Enter/Esc для завершення): "))

    (if sel_obj
      (progn
        (setq ent (car sel_obj))                ; Отримати ім'я об'єкта (entity name)
        (setq pick_point (cadr sel_obj))         ; Точка, по якій клікнув користувач
        (setq ent_data (entget ent))            ; Отримати дані об'єкта
        (setq ent_type (cdr (assoc 0 ent_data))) ; Отримати тип об'єкта як рядок (напр., "LINE", "CIRCLE", "INSERT")

        ;; Визначення точки обертання (ins_pt)
        (cond
          ;; Для Блоків, Тексту (одно- та багаторядкового), Точок, Атрибутів - DXF 10 це точка вставки
          ((member ent_type '("INSERT" "TEXT" "MTEXT" "POINT" "ATTDEF" "ATTRIB"))
           (setq ins_pt (cdr (assoc 10 ent_data)))
          )
          ;; Для Кіл та Дуг - DXF 10 це центр
          ((member ent_type '("CIRCLE" "ARC"))
           (setq ins_pt (cdr (assoc 10 ent_data)))
          )
          ;; Для Ліній - розворот навколо середини
          ((equal ent_type "LINE")
           (setq p1 (cdr (assoc 10 ent_data))) ; Початкова точка
           (setq p2 (cdr (assoc 11 ent_data))) ; Кінцева точка
           (if (and p1 p2)
             (setq ins_pt (mapcar (function (lambda (c1 c2) (/ (+ c1 c2) 2.0))) p1 p2))
             (progn
               (setq ins_pt pick_point) ; Якщо щось не так з точками лінії, використаємо точку кліку
               (princ (strcat "\nДля лінії не вдалося визначити кінцеві точки, використано точку кліку."))
             )
           )
          )
          ;; Для LWPOLYLINE, POLYLINE, SPLINE, ELLIPSE
          ;; Спробуємо використати DXF 10, якщо вона є (напр., центр еліпса, перший вертекс полілінії)
          ;; Це спрощення; для точного центру поліліній потрібен розрахунок bounding box.
          ((and (member ent_type '("LWPOLYLINE" "POLYLINE" "SPLINE" "ELLIPSE" "SOLID" "HATCH")) (assoc 10 ent_data))
            (setq ins_pt (cdr (assoc 10 ent_data)))
          )
          ;; Запасний варіант: точка, по якій клікнув користувач
          (t
           (setq ins_pt pick_point)
           (princ (strcat "\nДля об'єкта типу \"" ent_type "\" точка вставки/центр не визначена стандартно, використано точку кліку."))
          )
        )

        (if ins_pt
          (progn
            (command "_.ROTATE" ent "" "_non" ins_pt "180")
            ;; ВИПРАВЛЕНО: Використовуємо ent_type (рядок) для повідомлення
            (princ (strcat "\nОб'єкт типу \"" ent_type "\" розвернуто на 180° навколо точки ("
                           (rtos (car ins_pt) 2 4) ","
                           (rtos (cadr ins_pt) 2 4)
                           (if (caddr ins_pt) (strcat "," (rtos (caddr ins_pt) 2 4)) "") ; для 3D точок
                           ")."))
          )
          (princ (strcat "\nНе вдалося визначити точку обертання для об'єкта типу \"" ent_type "\"."))
        )
      )
      (progn
        (princ "\nЗавершення роботи скрипту ROTATEONCLICK.")
        nil ; Сигнал для завершення циклу while
      )
    )
  )
  (setvar "CMDECHO" 1) ; Увімкнути відображення команд
  (princ) ; Тихий вихід
)

;; Повідомлення, яке виводиться один раз при завантаженні файлу .lsp
(princ "\nСкрипт RotateOnClick.lsp завантажено. Введіть команду ROTATEONCLICK для запуску.")
(princ) ; Тихий вихід з файлу