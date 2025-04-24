;;; Скрипт для розстановки пікетажу вздовж полілінії AutoCAD (LWPOLYLINE)
;;; Створює блок маркера пікету (якщо не існує) та розставляє його
;;; екземпляри кожні 100м з урахуванням точки прив'язки, значення пікету,
;;; напрямку та сторони розміщення, вказаних користувачем.

;; === Допоміжні функції ===

;; Функція для створення шару, якщо він не існує
;; Повертає T, якщо шар існує або був створений, nil у разі помилки
(defun EnsureLayer (layername color linetype lineweight plotstyle plot)
  (if (tblsearch "LAYER" layername)
    (progn
      ;; Якщо шар заморожений, розморозити його
      (if (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" layername)))))
          (command "_.-LAYER" "_THAW" layername "")
      )
      ;; Якщо шар вимкнений, увімкнути його
      (if (< (cdr (assoc 62 (tblsearch "LAYER" layername))) 0)
          (command "_.-LAYER" "_ON" layername "")
      )
      T ; Шар вже існує
    )
    (progn ; Шар не існує, створюємо
      (command "_.-LAYER"
               "_MAKE" layername ; Створити та зробити поточним (тимчасово)
               (if color (strcat "_COLOR" (itoa color) "")) ""
               (if linetype (strcat "_LTYPE" linetype)) ""
               (if lineweight (strcat "_LWEIGHT" (itoa lineweight))) ""
               ;(if plotstyle (strcat "_PSTYLE" plotstyle)) "" ; PStyle потребує додаткової логіки
               (if (not plot) "_PLOT" "_Off") ""
               "" ; Завершити команду
      )
      (if (tblsearch "LAYER" layername) T nil) ; Перевірити чи створився
    )
  )
)

;; Функція для створення блоку маркера пікету
;; Повертає ім'я блоку у разі успіху, nil у разі помилки
(defun MakePicketBlock (blkname stylename txtheight layer_for_def)
  (if (tblsearch "BLOCK" blkname)
    blkname ; Блок вже існує
    (progn
      (princ (strcat "\nСтворення блоку маркера пікету '" blkname "'..."))
      (setq base_pt '(0.0 0.0 0.0)
            line_len 38.0
            cap_len 5.0
            half_line (/ line_len 2.0)
            half_cap (/ cap_len 2.0)
            txt_h txtheight
            att_tag "PIKET_NO"
            att_prompt "Номер пікету:"
            att_default "ПКXX"
            ; Визначаємо точку вставки атрибуту (над T-засічкою)
            att_ins_pt (list 0.0 (+ half_line (/ txt_h 2.0)) 0.0)
      )

      ;; Перевірка наявності текстового стилю
      (if (not (tblsearch "STYLE" stylename))
        (progn
          (princ (strcat "\n*** Попередження: Текстовий стиль '" stylename "' не знайдено."))
          (setq stylename (getvar "TEXTSTYLE")) ; Використовуємо поточний стиль
          (princ (strcat " Буде використано поточний стиль '" stylename "'."))
        )
      )

      ;; Створення геометрії блоку на вказаному шарі
      (setq entlist (list
                      ;; Головна перпендикулярна лінія
                      (list '(0 . "LINE") '(100 . "AcDbEntity") (cons 8 layer_for_def) '(100 . "AcDbLine") (cons 10 (list 0.0 (- half_line) 0.0)) (cons 11 (list 0.0 half_line 0.0)))
                      ;; T-засічка
                      (list '(0 . "LINE") '(100 . "AcDbEntity") (cons 8 layer_for_def) '(100 . "AcDbLine") (cons 10 (list (- half_cap) half_line 0.0)) (cons 11 (list half_cap half_line 0.0)))
                      ;; Визначення атрибуту
                      (list '(0 . "ATTDEF") '(100 . "AcDbEntity") (cons 8 layer_for_def) '(100 . "AcDbText")
                            (cons 10 att_ins_pt) ; Точка вставки (враховуємо вирівнювання)
                            (cons 40 txt_h)      ; Висота тексту
                            (cons 1 att_default) ; Значення за замовчуванням
                            '(100 . "AcDbAttributeDefinition")
                            (cons 2 att_tag)     ; Тег атрибуту
                            (cons 3 att_prompt)  ; Підказка
                            (cons 7 stylename)   ; Стиль тексту
                            '(71 . 0)            ; Flags (видимий)
                            '(72 . 4)            ; Горизонтальне вирівнювання (Middle)
                            '(74 . 1)            ; Вертикальне вирівнювання (Middle)
                      )
                    )
      )

      ;; Створення записів BLOCK та ENDBLK
      (setq block_header (list '(0 . "BLOCK") '(100 . "AcDbEntity") (cons 8 layer_for_def) '(100 . "AcDbBlockBegin") (cons 2 blkname) '(70 . 0) (cons 10 base_pt))) ; Base point 0,0,0
      (setq block_end (list '(0 . "ENDBLK") '(100 . "AcDbEntity") (cons 8 layer_for_def) '(100 . "AcDbBlockEnd")))

      ;; Об'єднання всіх частин
      (setq block_definition (append (list block_header) entlist (list block_end)))

      ;; Створення блоку за допомогою entmake
      (if (entmake block_definition)
        (progn
          (princ (strcat "\nБлок '" blkname "' успішно створено."))
          blkname ; Повертаємо ім'я блоку
        )
        (progn
          (princ (strcat "\n*** Помилка: Не вдалося створити блок '" blkname "'."))
          nil ; Помилка
        )
      )
    )
  )
)

;; Допоміжна функція для заміни стандартної floor (якщо вона недоступна)
(defun my-floor (num / num_fix) ; Додано локальну змінну num_fix
  (setq num_fix (fix num))
  (if (and (< num 0.0) (/= num num_fix))
    (- num_fix 1.0)
    num_fix
  )
)

;; Головна функція (оновлена для використання my-floor)
(defun C:CREATE_PICKETMARKER (/ *error* old_vars pline_ent pline_obj pt_ref pt_ref_on_pline dist_ref_on_pline
                             val_ref pt_dir vec_dir vec_tangent_ref dir_factor pt_side_ref vec_side_ref
                             vec_perp_ref dot_prod_side side_factor picket_at_start pline_len
                             first_picket_val last_picket_val current_picket_val dist_on_pline
                             pt_on_pline vec_tangent vec_perp vec_perp_final block_angle piket_str
                             target_layer block_name text_style text_height block_def_layer
                             fuzz mspace inserted_block atts att entmake_result num_fix) ; Додано num_fix для my-floor

  ;; Налаштування констант
  (setq target_layer "Pickets"    ; Шар для вставки маркерів
        block_name   "PicketMarker" ; Ім'я блоку маркера (внутрішнє ім'я блоку може лишатись таким)
        text_style   "Д-431"      ; Бажаний стиль тексту
        text_height  1.8          ; Висота тексту в блоці
        block_def_layer "0"       ; Шар для геометрії визначення блоку
        fuzz         1e-9         ; Допуск для порівняння дійсних чисел
  )

  ;; Перевизначення обробника помилок
  (defun *error* (msg)
    (if old_vars
      (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars)) ; Відновити старі змінні
    )
    ;; (if (and pline_obj (not (vlax-object-released-p pline_obj))) ; ВИДАЛЕНО HIGHLIGHT
    ;;     (vlax-invoke pline_obj 'Highlight :vlax_false)
    ;; )
    (if (not (member msg '("Function cancelled" "quit / exit abort" "Відміна користувачем")))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ) ; Тихий вихід
  )

  ;; Збереження системних змінних
  (setq old_vars (mapcar '(lambda (v) (cons v (getvar v)))
                         '("CMDECHO" "OSMODE" "CLAYER" "ATTREQ" "ATTDIA")))
  (setvar "CMDECHO" 0) ; Вимкнути ехо команд
  (setvar "ATTREQ" 1)  ; Увімкнути запит атрибутів (для vla-insertblock)
  (setvar "ATTDIA" 0)  ; Вимкнути діалогове вікно атрибутів

  ;; --- Збір вхідних даних ---
  (princ "\nРозстановка пікетажу вздовж полілінії.")

  ;; 1. Вибір полілінії
  (while (not pline_obj)
    (setq pline_ent (entsel "\nОберіть 2D полілінію (LWPOLYLINE): "))
    (if (and pline_ent (= "LWPOLYLINE" (cdr (assoc 0 (entget (car pline_ent))))))
      (setq pline_obj (vlax-ename->vla-object (car pline_ent)))
      (princ "\nОбраний об'єкт не є LWPOLYLINE. Спробуйте ще раз.")
    )
  )

  ;; 2. Точка прив'язки
  (setq pt_ref (getpoint "\nВкажіть точку прив'язки на полілінії або біля неї: "))
  (if pt_ref
    (progn
      (setq pt_ref_on_pline (vlax-curve-getClosestPointTo pline_obj (trans pt_ref 1 0))) ; Перетворюємо в WCS, якщо потрібно
      (setq dist_ref_on_pline (vlax-curve-getDistAtPoint pline_obj pt_ref_on_pline))
    )
    (progn (princ "\nТочку не вказано. Вихід.") (*error* "Відміна користувачем")) ; Вихід через *error*
  )
  (princ (strcat "\nВідстань до точки прив'язки від початку полілінії: " (rtos dist_ref_on_pline 2 4) " м."))

  ;; 3. Значення пікету
  (setq val_ref (getdist pt_ref_on_pline (strcat "\nВведіть значення пікету для цієї точки (в метрах): ")))
  (if (not val_ref)
     (progn (princ "\nЗначення пікету не введено. Вихід.") (*error* "Відміна користувачем"))
  )

  ;; 4. Напрямок збільшення пікетажу
  (setq pt_dir (getpoint pt_ref_on_pline "\nВкажіть точку в напрямку ЗБІЛЬШЕННЯ пікетажу: "))
  (if (not pt_dir)
     (progn (princ "\nНапрямок не вказано. Вихід.") (*error* "Відміна користувачем"))
  )

  ;; 5. Сторона для маркерів (ОДИН РАЗ)
  (setq pt_side_ref (getpoint pt_ref_on_pline "\nВкажіть сторону для розміщення всіх маркерів (T-засічки/тексту): "))
   (if (not pt_side_ref)
     (progn (princ "\nСторону не вказано. Вихід.") (*error* "Відміна користувачем"))
  )

  ;; Перевірка валідності даних одразу після введення
  (if (not (and pt_ref_on_pline (= (type pt_ref_on_pline) 'LIST) (= (length pt_ref_on_pline) 3)))
      (progn (princ "\n*** Помилка: Недійсна точка прив'язки на полілінії!") (*error* "Reference point on polyline invalid"))
  )
  (if (not (and pt_dir (= (type pt_dir) 'LIST) (= (length pt_dir) 3)))
      (progn (princ "\n*** Помилка: Недійсна точка напрямку!") (*error* "Direction point invalid"))
  )

  ;; --- Розрахунки ---
  ;; (vlax-invoke pline_obj 'Highlight :vlax_true) ; ВИДАЛЕНО HIGHLIGHT
  (command "_REGEN") ; Оновити екран

  ;; Визначення напрямку (dir_factor)
  (setq vec_dir (mapcar '- (trans pt_dir 1 0) pt_ref_on_pline))
  (setq vec_tangent_ref (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtPoint pline_obj pt_ref_on_pline)))
  (if (not vec_tangent_ref) (progn (princ "\n*** Помилка: Не вдалося отримати дотичну в точці прив'язки!") (*error* "Tangent calculation failed")))

  (setq dot_prod_dir (apply '+ (mapcar '* vec_dir vec_tangent_ref)))
  (setq dir_factor (if (< dot_prod_dir 0.0) -1.0 1.0))

  ;; Визначення сторони (side_factor)
  (setq vec_side_ref (mapcar '- (trans pt_side_ref 1 0) pt_ref_on_pline))
  (setq vec_perp_ref (list (- (cadr vec_tangent_ref)) (car vec_tangent_ref) 0.0))
  (setq dot_prod_side (apply '+ (mapcar '* vec_side_ref vec_perp_ref)))
  (setq side_factor (if (< dot_prod_side 0.0) -1.0 1.0))

  ;; Розрахунок пікету на початку полілінії
  (if (= dir_factor 1.0)
      (setq picket_at_start (- val_ref dist_ref_on_pline))
      (setq picket_at_start (+ val_ref dist_ref_on_pline))
  )

  ;; Загальна довжина полілінії
  (setq pline_len (vlax-curve-getDistAtParam pline_obj (vlax-curve-getEndParam pline_obj)))
  (if (not pline_len) (progn (princ "\n*** Помилка: Не вдалося отримати довжину полілінії!") (*error* "Length calculation failed")))
  (princ (strcat "\nЗагальна довжина полілінії: " (rtos pline_len 2 4) " м."))

  ;; Визначення діапазону 100-метрових пікетів --- ВИКОРИСТАННЯ MY-FLOOR ---
  (if (= dir_factor 1.0)
    (progn
      (setq first_picket_val (* (ceiling (/ (+ picket_at_start fuzz) 100.0)) 100.0))
      (setq last_picket_val (* (my-floor (/ (- (+ picket_at_start pline_len) fuzz) 100.0)) 100.0)) ; <--- ЗМІНЕНО
    )
    (progn ; Зворотній напрямок
      (setq first_picket_val (* (my-floor (/ (- picket_at_start fuzz) 100.0)) 100.0)) ; <--- ЗМІНЕНО
      (setq last_picket_val (* (ceiling (/ (+ (- picket_at_start pline_len) fuzz) 100.0)) 100.0))
    )
  )
  ;; ------------------------------------------------------------------

  ;; --- Підготовка до розстановки ---
  (princ (strcat "\nШукаємо пікети від " (rtos (min first_picket_val last_picket_val) 2 1) " до " (rtos (max first_picket_val last_picket_val) 2 1)))

  ;; Перевірка/створення шару для маркерів
  (if (not (EnsureLayer target_layer 7 "Continuous" -3 T T))
    (progn (princ (strcat "\n*** Помилка: Не вдалося створити/знайти шар '" target_layer "'.")) (*error* "Layer creation failed"))
  )
  (setvar "CLAYER" target_layer)

  ;; Перевірка/створення блоку
  (if (not (MakePicketBlock block_name text_style text_height block_def_layer))
      (progn (princ (strcat "\n*** Помилка: Не вдалося створити/знайти блок '" block_name "'.")) (*error* "Block creation/check failed"))
  )

  ;; Отримання об'єкту простору моделі
  (setq mspace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (if (not mspace) (progn (princ "\n*** Помилка: Не вдалося отримати Modelspace!") (*error* "Modelspace failed")))

  ;; --- Цикл розстановки пікетів ---
  (setq current_picket_val first_picket_val)
  (while (if (= dir_factor 1.0) (<= current_picket_val (+ last_picket_val fuzz)) (>= current_picket_val (- last_picket_val fuzz)))
    ;; Розрахунок відстані вздовж полілінії
    (if (= dir_factor 1.0)
      (setq dist_on_pline (- current_picket_val picket_at_start))
      (setq dist_on_pline (- picket_at_start current_picket_val))
    )

    ;; Перевірка, чи точка в межах полілінії
    (if (and (>= dist_on_pline (- 0.0 fuzz)) (<= dist_on_pline (+ pline_len fuzz)))
      (progn
        ;; Отримання точки та дотичної
        (setq pt_on_pline (vlax-curve-getPointAtDist pline_obj dist_on_pline))
        (setq vec_tangent (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtDist pline_obj dist_on_pline)))

        ;; --- Додаткова перевірка на нульовий вектор дотичної ---
        (if (and pt_on_pline vec_tangent (< (distance '(0 0 0) vec_tangent) fuzz))
            (princ (strcat "\n*** Попередження: Нульовий вектор дотичної на відстані " (rtos dist_on_pline) ". Пропуск пікету."))
            ;; --- Якщо дотична не нульова, продовжуємо ---
            (if (and pt_on_pline vec_tangent) ; Переконатись, що попередні функції спрацювали
              (progn
                ;; Розрахунок перпендикуляру та кута повороту
                (setq vec_perp (list (- (cadr vec_tangent)) (car vec_tangent) 0.0))
                (setq vec_perp_final (if (= side_factor 1.0) vec_perp (mapcar '- vec_perp)))
                (setq block_angle (angle '(0.0 0.0 0.0) vec_perp_final))

                ;; Формування тексту пікету
                (setq piket_str (strcat "ПК" (itoa (fix (+ (/ current_picket_val 100.0) fuzz)))))

                ;; Вставка блоку
                (princ (strcat "\n Вставка маркера: " piket_str " на відстані " (rtos dist_on_pline 2 2)))
                (setq inserted_block (vl-catch-all-apply
                                      'vla-InsertBlock
                                      (list mspace (vlax-3d-point pt_on_pline) block_name 1.0 1.0 1.0 block_angle)
                                     )
                )

                ;; --- Додаткова перевірка результату вставки ---
                (if (vl-catch-all-error-p inserted_block)
                    (princ (strcat "\n*** Помилка під час vla-InsertBlock: " (vl-catch-all-error-message inserted_block)))
                    (if (not inserted_block)
                       (princ "\n*** Помилка: vla-InsertBlock повернув nil!")
                       (progn ; Вставка успішна, обробляємо атрибути
                         ;; Встановлення значення атрибуту
                         (if (= (vla-get-HasAttributes inserted_block) :vlax_true)
                           (progn
                             (setq atts (vl-catch-all-apply 'vlax-invoke (list inserted_block 'GetAttributes)))
                             (if (vl-catch-all-error-p atts)
                                (princ (strcat "\n*** Помилка отримання атрибутів: " (vl-catch-all-error-message atts)))
                                (if atts
                                    (progn
                                       (foreach att atts
                                         (if (= (strcase (vla-get-TagString att)) (strcase att_tag))
                                           (vl-catch-all-apply 'vla-put-TextString (list att piket_str))
                                         )
                                       )
                                       (vl-catch-all-apply 'vlax-invoke (list inserted_block 'Update)) ; Оновити відображення атрибутів
                                    )
                                )
                             )
                           )
                         )
                       ) ; progn block inserted successfully
                    ) ; endif block inserted successfully
                ) ; endif check vla-InsertBlock error
              ) ; progn if point and tangent are valid
            ) ; endif zero tangent check
        ) ; endif point and tangent exist check
      ) ; progn if point on polyline
    ) ; if point on polyline

    ;; Перехід до наступного пікету
    (setq current_picket_val (+ current_picket_val (* dir_factor 100.0)))
  ) ; while loop

  ;; --- Завершення ---
  ;; (vlax-invoke pline_obj 'Highlight :vlax_false) ; ВИДАЛЕНО HIGHLIGHT
  (command "_REGEN") ; Оновити екран для відображення атрибутів
  (princ "\nРозстановка пікетажу завершена.")

  ;; Відновлення системних змінних та стандартного обробника помилок
  (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars))
  (setq *error* nil) ; Відновити стандартний обробник
  (princ) ; Тихий вихід
)

;; Повідомлення про завантаження - ЗМІНЕНО
(princ "\nСкрипт для розстановки пікетажу завантажено. Введіть 'CREATE_PICKETMARKER' v6 для запуску.")
(princ)