;;; Скрипт для інтерактивного відображення пікетажу на LWPOLYLINE з XDATA
;;; Версія v2025-06-30_InteractivePicketDisplay_Initial
;;; Зчитує XDATA, збережені CREATE_PICKET_MARKER, та динамічно відображає пікетаж
;;; при наведенні курсора на полілінію. Дозволяє вставляти постійний текст.

(vl-load-com) ; Завантажуємо VLAX функції, якщо ще не завантажені

;; === Глобальні змінні для очищення ===
;; Зберігаємо старі значення системних змінних для коректного відновлення
(setq *picket-display-old-cmdecho* nil)
(setq *picket-display-old-osmode* nil)
(setq *picket-display-old-cursorsize* nil)
(setq *picket-display-old-textfill* nil)
(setq *picket-display-temp-text-obj* nil) ; Змінна для тимчасового текстового об'єкта

;; --- Допоміжні функції (скопійовані з CREATE_PICKET_MARKER.lsp) ---

(defun FormatPicketValue (p_val / pk_km pk_m val_str km_str fuzz)
  (setq fuzz 1e-9)
  (setq pk_km (fix (/ p_val 100.0)))
  (setq pk_m (abs (- p_val (* (float pk_km) 100.0))))
  (if (> pk_m (- 100.0 fuzz)) (progn (setq pk_m 0.0) (if (>= p_val 0.0) (setq pk_km (1+ pk_km)) (setq pk_km (1- pk_km)))))
  (setq km_str (itoa pk_km))
  (setq val_str (rtos pk_m 2 2))
  (if (and (< pk_m (- 10.0 fuzz)) (> pk_m (- 0.0 fuzz))) (setq val_str (strcat "0" val_str)))
  (strcat "ПК" km_str "+" val_str)
)

(defun GetAcadObjectAndDoc (/ acad_obj doc)
  (vl-catch-all-apply
    '(lambda ()
       (setq acad_obj (vlax-get-acad-object))
       (setq doc (vla-get-ActiveDocument acad_obj))
     )
  )
  (list acad_obj doc)
)

;; --- Функції для роботи з XDATA ---
;; Ця функція буде читати XDATA з об'єкта. Вона простіша, бо не реєструє, а лише читає.
(defun GetPicketXData (ent_ename app_name / xdata_list app_data picket_at_start_str dir_factor_str)
  (setq xdata_list (entget ent_ename (list app_name))) ; Отримуємо дані, фільтровані за нашим AppID
  (setq app_data nil)
  (setq picket_at_start nil)
  (setq dir_factor nil)

  (if (and xdata_list (assoc -3 xdata_list)) ; Перевіряємо, чи є взагалі XDATA для нашого AppID
      (progn
        ;; Знаходимо потрібний блок XDATA
        (setq app_data (cdr (assoc -3 xdata_list))) 
        (if (and app_data (>= (length app_data) 3) ; Перевіряємо, що список має достатньо елементів
                 (= (car app_data) app_name)        ; Перевіряємо, що це наш AppID
            )
            (progn
              (setq picket_at_start_str (cdr (nth 1 app_data))) ; (1000 . "value") -> "value"
              (setq dir_factor_str (cdr (nth 2 app_data)))      ; (1000 . "value") -> "value"
              
              (if (and picket_at_start_str dir_factor_str) ; Перевірка на nil
                  (progn
                    (setq picket_at_start (atof picket_at_start_str))
                    (setq dir_factor (atof dir_factor_str))
                  )
              )
            )
        )
      )
  )
  (if (and (numberp picket_at_start) (numberp dir_factor)) ; Повертаємо список, якщо обидва значення дійсні
      (list picket_at_start dir_factor)
      nil ; Повертаємо nil, якщо дані не знайдені або невалідні
  )
)


;; === Функція для очищення тимчасових об'єктів та відновлення змінних ===
(defun CleanUpPicketDisplay (/ acad_obj doc mspace)
  (if *picket-display-temp-text-obj*
    (vl-catch-all-apply 'vla-Delete (list *picket-display-temp-text-obj*))
  )
  (setq *picket-display-temp-text-obj* nil) ; Обнуляємо глобальну змінну

  (if *picket-display-old-cmdecho* (setvar "CMDECHO" *picket-display-old-cmdecho*))
  (if *picket-display-old-osmode* (setvar "OSMODE" *picket-display-old-osmode*))
  (if *picket-display-old-cursorsize* (setvar "CURSORSIZE" *picket-display-old-cursorsize*))
  (if *picket-display-old-textfill* (setvar "TEXTFILL" *picket-display-old-textfill*))

  (setq *error* nil) ; Скидаємо обробник помилок
  (princ)
)

;; === Обробник помилок для GET_PICKET_ON_LINE ===
(defun *error* (msg)
  (CleanUpPicketDisplay) ; Викликаємо функцію очищення
  (if (not (member msg '("Function cancelled" "quit / exit abort" "Відміна користувачем" "Initialization failed")))
    (princ (strcat "\nПомилка виконання: " msg))
  )
  (princ)
)

;; === Головна функція GET_PICKET_ON_LINE ===
(defun C:GET_PICKET_ON_LINE (/ acad_obj doc pline_ent pline_obj xdata app_id_name
                               picket_at_start dir_factor dist_on_pline current_picket_val
                               pt_on_pline pt_cursor view_size text_height mspace temp_text_obj
                               old_cmdecho old_osmode old_cursorsize old_textfill)

  (princ "\n*** Запущено GET_PICKET_ON_LINE v2025-06-30_InteractivePicketDisplay_Initial ***")
  (setq app_id_name "PicketMaster") ; Те саме AppID, що й у CREATE_PICKET_MARKER

  ;; Ініціалізація ActiveX/COM об'єктів
  (setq result_obj (GetAcadObjectAndDoc))
  (setq acad_obj (car result_obj))
  (setq doc (cadr result_obj))

  (if (or (not acad_obj) (not doc))
      (progn
        (princ "\n*** Помилка: Не вдалося ініціалізувати об'єкти AutoCAD ActiveX. Перезапустіть AutoCAD.")
        (*error* "Initialization failed")
      )
  )

  ;; Збереження системних змінних та налаштування для Drag-mode
  (setq *picket-display-old-cmdecho* (getvar "CMDECHO"))
  (setq *picket-display-old-osmode* (getvar "OSMODE"))
  (setq *picket-display-old-cursorsize* (getvar "CURSORSIZE"))
  (setq *picket-display-old-textfill* (getvar "TEXTFILL"))
  
  (setvar "CMDECHO" 0)   ; Вимкнути відлуння команд
  (setvar "OSMODE" 0)    ; Вимкнути прив'язки
  (setvar "CURSORSIZE" 100) ; Збільшити розмір курсора для кращої візуалізації
  (setvar "TEXTFILL" 0)  ; Вимкнути заповнення тексту для кращої продуктивності

  (setq mspace (vla-get-ModelSpace doc))

  ;; Цикл вибору полілінії та валідації XDATA
  (setq pline_obj nil)
  (while (not pline_obj)
    (setq pline_ent (entsel "\nОберіть LWPOLYLINE з даними пікетажу (або ESC для виходу): "))
    (if (null pline_ent) (*error* "Відміна користувачем")) ; Вихід при ESC

    (if (and pline_ent (= "LWPOLYLINE" (cdr (assoc 0 (entget (car pline_ent))))))
      (progn
        (setq pline_obj (vlax-ename->vla-object (car pline_ent)))
        (setq xdata (GetPicketXData (car pline_ent) app_id_name))

        (if xdata ; Якщо дані знайдені
          (progn
            (setq picket_at_start (car xdata))
            (setq dir_factor (cadr xdata))
            (princ (strcat "\nДані пікетажу знайдено! Початок: " (rtos picket_at_start) ", Напрямок: " (rtos dir_factor)))
          )
          (progn
            (princ "\n*** Помилка: На обраній полілінії не знайдено даних пікетажу або вони невалідні. Оберіть іншу LWPOLYLINE.")
            (setq pline_obj nil) ; Скидаємо pline_obj, щоб продовжити цикл
          )
        )
      )
      (princ "\nОбраний об'єкт не є LWPOLYLINE. Спробуйте ще раз.")
    )
  )

  ;; Розрахунок висоти тексту (динамічно відносно поточного вигляду)
  (setq view_size (getvar "VIEWSIZE")) ; Висота вікна в одиницях креслення
  (setq text_height (* view_size 0.02)) ; Наприклад, 2% від висоти вікна

  ;; --- Вхід в інтерактивний Drag-mode ---
  (princ "\nПереміщуйте курсор над полілінією. Лівий клік для вставки тексту, ESC/ENTER для виходу.")

  (setq keep_running T)
  (while keep_running
    (setq pt_cursor (grread T 13 0)) ; (grread T 13 0) - повертає (4 pt) для руху, (2 code pt) для кліку
    ;; Коди grread: 2 - клік миші, 3 - натискання клавіші, 4 - рух миші

    (cond
      ((= (car pt_cursor) 2) ; Клік мишею (ліва кнопка)
       (setq click_pt (cadr pt_cursor))
       (if pline_obj
           (progn
             (setq pt_on_pline (vlax-curve-getClosestPointTo pline_obj click_pt))
             (setq dist_on_pline (vlax-curve-getDistAtPoint pline_obj pt_on_pline))
             (setq current_picket_val (+ picket_at_start (* dir_factor dist_on_pline)))
             (setq picket_str (FormatPicketValue current_picket_val))

             ;; Вставка постійного текстового об'єкта
             (if doc
                 (vl-catch-all-apply
                   'vla-AddText (list mspace picket_str (vlax-3d-point pt_on_pline) text_height)
                 )
             )
             (princ (strcat "\nВставлено текст: " picket_str " в " (rtos (car pt_on_pline)) "," (rtos (cadr pt_on_pline))))
           )
       )
      )

      ((= (car pt_cursor) 4) ; Рух мишею
       (setq pt_actual_cursor (cadr pt_cursor)) ; Курсор у світових координатах
       (if pline_obj
           (progn
             (setq pt_on_pline (vlax-curve-getClosestPointTo pline_obj pt_actual_cursor))
             (setq dist_on_pline (vlax-curve-getDistAtPoint pline_obj pt_on_pline))
             (setq current_picket_val (+ picket_at_start (* dir_factor dist_on_pline)))
             (setq picket_str (FormatPicketValue current_picket_val))

             ;; Оновлення тимчасового тексту
             (if *picket-display-temp-text-obj*
                 (progn
                   (vl-catch-all-apply 'vla-put-TextString (list *picket-display-temp-text-obj* picket_str))
                   (vl-catch-all-apply 'vla-put-InsertionPoint (list *picket-display-temp-text-obj* (vlax-3d-point pt_on_pline)))
                   (vl-catch-all-apply 'vla-Update (list *picket-display-temp-text-obj*))
                 )
                 ;; Створення тимчасового тексту, якщо його ще немає
                 (if doc
                     (setq *picket-display-temp-text-obj*
                           (vl-catch-all-apply 'vla-AddText (list mspace picket_str (vlax-3d-point pt_on_pline) text_height))
                     )
                 )
             )
             (if (and *picket-display-temp-text-obj* (not (vl-catch-all-error-p *picket-display-temp-text-obj*)))
                 (progn
                   (vl-catch-all-apply 'vla-put-Color (list *picket-display-temp-text-obj* 256)) ; Червоний колір (ByLayer)
                   (vl-catch-all-apply 'vla-put-Height (list *picket-display-temp-text-obj* text_height))
                   (vl-catch-all-apply 'vla-put-Background (list *picket-display-temp-text-obj* :vlax-true)) ; Увімкнути фон (маску)
                   ;; Для TrueColor потрібен TrueColor об'єкт, а не просто Color. Це складніше.
                   ;; Поки що можемо залишити Background без TrueColor, або встановити просто чорний/білий.
                   (vl-catch-all-apply 'vla-put-TrueColor (list *picket-display-temp-text-obj* (vlax-get-property (vlax-get-acad-object) 'ActiveDocument 'Application 'Preferences 'Display 'Colors 'Background)))
                 )
             )
             ;; Вивід у командний рядок
             (princ (strcat "\rПікетаж: " picket_str))
           )
           (princ "\r") ; Очистити рядок, якщо полілінія не обрана
       )
      )

      ((or (= (car pt_cursor) 3) (= (cadr pt_cursor) 13)) ; Натиснуто клавішу (ESC або Enter)
       (setq keep_running nil) ; Вихід з циклу
      )
    )
  )

  ;; Завершальне очищення
  (CleanUpPicketDisplay)
  (princ "\nРоботу скрипта завершено.")
  (princ)
)

;; Повідомлення про завантаження
(princ "\nСкрипт для інтерактивного пікетажу завантажено. Введіть 'GET_PICKET_ON_LINE' для запуску.")
(princ)