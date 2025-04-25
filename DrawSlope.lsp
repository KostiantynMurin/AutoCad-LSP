;; Малювання умовного знаку схилу (Чергування довгих/коротких штрихів)
;; ВЕРСІЯ БЕЗ `vlax-curve-isKindOf` (Але все ще залежить від інших функцій vlax-curve-*)

(defun C:DrawSlope (/ *error* old_osmode old_cmdecho e_top p_top obj_top e_bot p_bot obj_bot interval total_len cur_dist pt_on_top perp_angle pt_on_bottom stroke_len long_len short_len is_long_stroke end_pt acadObj acadDoc esel_top esel_bot clayer_name clayer_data clayer_flags ent_type_top ent_type_bot) ; Додано змінні для типу об'єкта

  ;; --- Обробник помилок ---
  (defun *error* (msg)
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if acadDoc (vla-EndUndoMark acadDoc))
    (cond ((not msg))
          ((wcmatch (strcase msg T) "*BREAK,*CANCEL*,*EXIT*,*СКЛАД ЗАМОРОЖЕНИЙ*"))
          ((princ (strcat "\nПомилка DrawSlope: " msg)))
    )
    (princ)
  )

  ;; --- Основна функція ---
  (vl-load-com) ; Спроба завантажити Visual LISP (Все ще ДУЖЕ ВАЖЛИВО!)
  (setq acadObj (vlax-get-acad-object))
  (setq acadDoc (vla-get-ActiveDocument acadObj))

  ;; --- Зберегти системні змінні ---
  (setq old_osmode (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))

  ;; --- Перевірка поточного шару і початок групи відміни ---
  (setq clayer_name (getvar "CLAYER"))
  (setq clayer_data (tblsearch "LAYER" clayer_name))
  (if (not clayer_data)
      (progn (princ (strcat "\nПОМИЛКА: Не вдалося знайти дані для поточного шару '" clayer_name "'.")) (*error* "Помилка доступу до даних шару"))
      (progn
          (setq clayer_flags (cdr (assoc 70 clayer_data)))
          (if (= 1 (logand 1 clayer_flags))
              (progn (princ (strcat "\nПоточний шар '" clayer_name "' заморожений. Розморозьте його або змініть поточний шар.")) (*error* "Поточний шар заморожений"))
              (vla-StartUndoMark acadDoc) ; Почати Undo тільки якщо шар не заморожений
          )
      )
  )

  ;; --- Вимкнути об'єктну прив'язку та ехо команд ---
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  ;; --- Отримати верхній край (Бровку) ---
  (setq e_top nil obj_top nil) ; Ініціалізувати obj_top як nil
  (while (not e_top)
    (setq esel_top (entsel "\nВиберіть бровку схилу (Лінію, Полілінію, Дугу, Сплайн, Еліпс): "))
    (if esel_top
      (progn
        (setq e_top (car esel_top))
        (setq p_top (cadr esel_top))
        ;; Альтернативна перевірка типу об'єкта (без vlax-curve-isKindOf)
        (setq ent_type_top (cdr (assoc 0 (entget e_top))))
        (if (not (member ent_type_top '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "SPLINE" "ELLIPSE")))
            (progn
                (princ (strcat "\nВибраний об'єкт ('" ent_type_top "') не підтримується. Виберіть Лінію, Полілінію, Дугу, Сплайн або Еліпс."))
                (setq e_top nil) ; Скинути вибір, щоб запитати знову
            )
            ;; Якщо тип підходить, ОТРИМУЄМО VLA ОБ'ЄКТ (все ще потрібен для інших функцій!)
            (progn
                ;; НАСТУПНИЙ РЯДОК ВИКОРИСТОВУЄ VLAX - МОЖЕ ВИКЛИКАТИ ПОМИЛКУ!
                (setq obj_top (vlax-ename->vla-object e_top))
                ;; Додаткова перевірка, чи вдалося отримати VLA об'єкт
                (if (not obj_top)
                    (progn
                        (princ (strcat "\nПомилка отримання VLA-об'єкта для бровки типу '" ent_type_top "'. Можливо, Visual LISP не працює."))
                        (*error* "Не вдалося отримати VLA-об'єкт")
                    )
                )
            )
        )
      )
      (progn (princ "\nОб'єкт не вибрано, операція скасована.") (*error* "Скасовано користувачем"))
    )
  )

  ;; --- Отримати нижній край (Підошву) ---
  (setq e_bot nil obj_bot nil) ; Ініціалізувати obj_bot як nil
  (while (not e_bot)
    (setq esel_bot (entsel "\nВиберіть підошву схилу (Лінію, Полілінію, Дугу, Сплайн, Еліпс): "))
    (if esel_bot
      (progn
        (setq e_bot (car esel_bot))
        (setq p_bot (cadr esel_bot))
        (if (= e_top e_bot)
           (progn (princ "\nБровка та підошва не можуть бути одним і тим же об'єктом.")(setq e_bot nil))
           (progn
             ;; Альтернативна перевірка типу об'єкта
             (setq ent_type_bot (cdr (assoc 0 (entget e_bot))))
             (if (not (member ent_type_bot '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "SPLINE" "ELLIPSE")))
                 (progn
                     (princ (strcat "\nВибраний об'єкт ('" ent_type_bot "') не підтримується. Виберіть Лінію, Полілінію, Дугу, Сплайн або Еліпс."))
                     (setq e_bot nil) ; Скинути вибір
                 )
                 ;; Якщо тип підходить, ОТРИМУЄМО VLA ОБ'ЄКТ
                 (progn
                    ;; НАСТУПНИЙ РЯДОК ВИКОРИСТОВУЄ VLAX - МОЖЕ ВИКЛИКАТИ ПОМИЛКУ!
                    (setq obj_bot (vlax-ename->vla-object e_bot))
                    ;; Додаткова перевірка, чи вдалося отримати VLA об'єкт
                    (if (not obj_bot)
                        (progn
                            (princ (strcat "\nПомилка отримання VLA-об'єкта для підошви типу '" ent_type_bot "'. Можливо, Visual LISP не працює."))
                            (*error* "Не вдалося отримати VLA-об'єкт")
                        )
                    )
                 )
             )
           )
        )
      )
      (progn (princ "\nОб'єкт не вибрано, операція скасована.") (*error* "Скасовано користувачем"))
    )
  )

  ;; --- Отримати інтервал ---
  (setq interval nil)
  (while (not interval)
    (initget 6) ; Не нуль, не від'ємне
    (setq interval (getdist "\nВкажіть інтервал між штрихами вздовж бровки: "))
    (if (not interval)
        (progn (princ "\nВведення скасовано.") (*error* "Скасовано користувачем"))
        (if (<= interval 1e-6)
            (progn (princ "\nІнтервал повинен бути позитивним числом.") (setq interval nil))
        )
    )
  )

  ;; --- Цикл обчислення та малювання ---
  ;; !!! НАСТУПНІ РЯДКИ ВИКОРИСТОВУЮТЬ ІНШІ VLAX-CURVE-* ФУНКЦІЇ !!!
  ;; !!! ЯКЩО VISUAL LISP НЕ ПРАЦЮЄ КОРЕКТНО, ТУТ БУДЕ ПОМИЛКА !!!
  (princ "\nОбчислення параметрів кривої...")
  ;; НАСТУПНИЙ РЯДОК ВИКОРИСТОВУЄ VLAX - МОЖЕ ВИКЛИКАТИ ПОМИЛКУ!
  (setq total_len (vlax-curve-getDistAtParam obj_top (vlax-curve-getEndParam obj_top)))
  (setq cur_dist 0.0)
  (setq is_long_stroke T)

  (princ "\nМалювання штрихів...")
  (while (<= cur_dist (+ total_len 1e-6))
    ;; НАСТУПНІ РЯДКИ ВИКОРИСТОВУЮТЬ VLAX - МОЖУТЬ ВИКЛИКАТИ ПОМИЛКУ!
    (setq pt_on_top (vlax-curve-getPointAtDist obj_top cur_dist))
    (setq pt_on_bottom (vlax-curve-getClosestPointTo obj_bot pt_on_top))

    (if (and pt_on_top pt_on_bottom)
      (progn
        (setq long_len (distance pt_on_top pt_on_bottom))
        (setq short_len (/ long_len 2.0))
        (setq stroke_angle (angle pt_on_top pt_on_bottom))

        (if is_long_stroke (setq stroke_len long_len) (setq stroke_len short_len))

        (if (> stroke_len 1e-6)
           (progn
              (setq end_pt (polar pt_on_top stroke_angle stroke_len))
              (entmake (list (cons 0 "LINE") (cons 10 pt_on_top) (cons 11 end_pt)))
           )
        )
        (setq is_long_stroke (not is_long_stroke))
      )
      (princ (strcat "\nНеможливо знайти відповідну точку на підошві для відстані " (rtos cur_dist) "."))
    )
    (setq cur_dist (+ cur_dist interval))
  )
  (princ " Завершено.")

  ;; --- Відновити системні змінні ---
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)

  (princ)
)

; --- Повідомлення про завантаження ---
(princ "\nLISP 'DrawSlope' (без vlax-curve-isKindOf) завантажено. Введіть 'DrawSlope'.")
(princ)