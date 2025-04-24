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

;; Функція для створення блоку маркера пікету (з використанням COMMAND)
;; Повертає ім'я блоку у разі успіху, nil у разі помилки
(defun MakePicketBlock (blkname stylename txtheight layer_for_def / base_pt line_len cap_len half_line half_cap txt_h att_tag att_prompt att_default att_ins_pt temp_ents old_layer old_osmode old_cmdecho line1 line2 attdef) ; Додано локальні змінні
  (if (tblsearch "BLOCK" blkname)
    blkname ; Блок вже існує
    (progn
      (princ (strcat "\nСтворення блоку маркера пікету '" blkname "'..."))
      (setq base_pt '(0.0 0.0 0.0)
            line_len 38.0 cap_len 5.0 half_line (/ line_len 2.0) half_cap (/ cap_len 2.0)
            txt_h txtheight att_tag "PIKET_NO" att_prompt "Номер пікету:" att_default "ПКXX"
            att_ins_pt (list 0.0 (+ half_line (/ txt_h 2.0)) 0.0)
            temp_ents (ssadd) ; Selection set for temporary entities
            old_layer (getvar "CLAYER") ; Save current layer
            old_osmode (getvar "OSMODE")
            old_cmdecho (getvar "CMDECHO")
      )
      (setvar "OSMODE" 0)
      (setvar "CMDECHO" 0)
      (setvar "CLAYER" "0") ; Тимчасово створюємо геометрію на Шарі 0

      ;; Перевірка наявності текстового стилю
      (if (not (tblsearch "STYLE" stylename))
        (progn
          (princ (strcat "\n*** Попередження: Текстовий стиль '" stylename "' не знайдено."))
          (setq stylename (getvar "TEXTSTYLE")) ; Використовуємо поточний стиль
          (princ (strcat " Буде використано поточний стиль '" stylename "'."))
        )
      )

      ;; Створення тимчасової геометрії за допомогою entmakex (безпечніше за entmake)
      ;; Об'єкти створюються на поточному шарі (який ми встановили як "0")
      (setq line1 (entmakex (list '(0 . "LINE") (cons 10 (list 0.0 (- half_line) 0.0)) (cons 11 (list 0.0 half_line 0.0)))))
      (setq line2 (entmakex (list '(0 . "LINE") (cons 10 (list (- half_cap) half_line 0.0)) (cons 11 (list half_cap half_line 0.0)))))
      (setq attdef (entmakex (list '(0 . "ATTDEF") (cons 10 att_ins_pt) (cons 40 txt_h) (cons 1 att_default) (cons 2 att_tag) (cons 3 att_prompt) (cons 7 stylename) '(71 . 0) '(72 . 4) '(74 . 1))))

      ;; Перевірка чи об'єкти створено і додавання їх до набору
      ;; entlast використовується тому що entmakex не повертає ім'я об'єкта напряму
      (if line1 (ssadd (entlast) temp_ents))
      (if line2 (ssadd (entlast) temp_ents))
      (if attdef (ssadd (entlast) temp_ents))

      (if (> (sslength temp_ents) 0) ; Продовжуємо тільки якщо геометрію створено
          (progn
            ;; Використання команди BLOCK для визначення блоку з тимчасових об'єктів
            (command "_.-BLOCK" blkname ; Ім'я блоку
                     "_Y" ; Перезаписати, якщо існує (безпека, хоча tblsearch вже перевірив)
                     base_pt ; Базова точка вставки блоку
                     temp_ents ; Вибір об'єктів, що увійдуть у блок
                     "" ; Завершити вибір об'єктів
            )
            ;; Перевірка чи блок дійсно створено після команди
            (if (tblsearch "BLOCK" blkname)
                (progn (princ (strcat "\nБлок '" blkname "' успішно створено.")) blkname) ; Успіх
                (progn (princ (strcat "\n*** Помилка: Не вдалося створити блок '" blkname "' за допомогою команди BLOCK.")) nil) ; Невдача
            )
          )
          (progn (princ "\n*** Помилка: Не вдалося створити геометрію для блоку.") nil) ; Невдача створення геометрії
      )
      ;; Відновлення попередніх налаштувань AutoCAD
      (setvar "CLAYER" old_layer)
      (setvar "OSMODE" old_osmode)
      (setvar "CMDECHO" old_cmdecho)
    ) ; progn if block needs creation
  ) ; if tblsearch
)

;; Допоміжна функція для заміни стандартної floor (якщо вона недоступна)
(defun my-floor (num / num_fix) ; Додано локальну змінну num_fix
  (setq num_fix (fix num))
  (if (and (< num 0.0) (/= num num_fix))
    (- num_fix 1.0)
    num_fix
  )
)

;; Головна функція (оновлена для використання FIX, вставки на шар 0, та COMMAND для блоку)
(defun C:CREATE_PICKETMARKER (/ *error* old_vars pline_ent pline_obj pt_ref pt_ref_on_pline dist_ref_on_pline
                             val_ref pt_dir vec_dir vec_tangent_ref dir_factor pt_side_ref vec_side_ref
                             vec_perp_ref dot_prod_side side_factor picket_at_start pline_len
                             first_picket_val last_picket_val current_picket_val dist_on_pline
                             pt_on_pline vec_tangent vec_perp vec_perp_final block_angle piket_str
                             target_layer block_name text_style text_height block_def_layer
                             fuzz mspace inserted_block atts att old_layer old_osmode old_cmdecho) ; Видалено entmake_result, додано змінні для MakeBlock

  (princ "\n*** Running CREATE_PICKETMARKER v2025-04-24_FIX_Layer0_CmdBlock ***") ; <<< Оновлено версію

  ;; Налаштування констант
  (setq target_layer "0"         ; <--- ЗМІНЕНО: Шар для вставки маркерів = 0
        block_name   "PicketMarker"
        text_style   "Д-431"
        text_height  1.8
        block_def_layer "0"       ; Шар для геометрії визначення блоку (використовується в MakeBlock)
        fuzz         1e-9
  )

  ;; Перевизначення обробника помилок
  (defun *error* (msg)
    (if old_vars (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars)))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "Відміна користувачем")))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ)
  )

  ;; Збереження системних змінних
  (setq old_vars (mapcar '(lambda (v) (cons v (getvar v))) '("CMDECHO" "OSMODE" "CLAYER" "ATTREQ" "ATTDIA")))
  (setvar "CMDECHO" 0) (setvar "ATTREQ" 1) (setvar "ATTDIA" 0)

  ;; --- Збір вхідних даних ---
  (princ "\nРозстановка пікетажу вздовж полілінії.")
  (while (not pline_obj)
    (setq pline_ent (entsel "\nОберіть 2D полілінію (LWPOLYLINE): "))
    (if (and pline_ent (= "LWPOLYLINE" (cdr (assoc 0 (entget (car pline_ent))))))
      (setq pline_obj (vlax-ename->vla-object (car pline_ent)))
      (princ "\nОбраний об'єкт не є LWPOLYLINE. Спробуйте ще раз.")
    )
  )
  (setq pt_ref (getpoint "\nВкажіть точку прив'язки на полілінії або біля неї: "))
  (if pt_ref
    (progn (setq pt_ref_on_pline (vlax-curve-getClosestPointTo pline_obj (trans pt_ref 1 0)))
           (setq dist_ref_on_pline (vlax-curve-getDistAtPoint pline_obj pt_ref_on_pline)))
    (*error* "Відміна користувачем")
  )
  (princ (strcat "\nВідстань до точки прив'язки від початку полілінії: " (rtos dist_ref_on_pline 2 4) " м."))
  (setq val_ref (getdist pt_ref_on_pline (strcat "\nВведіть значення пікету для цієї точки (в метрах): ")))
  (if (not val_ref) (*error* "Відміна користувачем"))
  (setq pt_dir (getpoint pt_ref_on_pline "\nВкажіть точку в напрямку ЗБІЛЬШЕННЯ пікетажу: "))
  (if (not pt_dir) (*error* "Відміна користувачем"))
  (setq pt_side_ref (getpoint pt_ref_on_pline "\nВкажіть сторону для розміщення всіх маркерів (T-засічки/тексту): "))
  (if (not pt_side_ref) (*error* "Відміна користувачем"))

  ;; Перевірка валідності
  (if (not (and pt_ref_on_pline (= 'LIST (type pt_ref_on_pline)) (= 3 (length pt_ref_on_pline)))) (*error* "Reference point on polyline invalid"))
  (if (not (and pt_dir (= 'LIST (type pt_dir)) (= 3 (length pt_dir)))) (*error* "Direction point invalid"))

  ;; --- Розрахунки ---
  (command "_REGEN")
  (setq vec_dir (mapcar '- (trans pt_dir 1 0) pt_ref_on_pline))
  (setq vec_tangent_ref (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtPoint pline_obj pt_ref_on_pline)))
  (if (not vec_tangent_ref) (*error* "Tangent calculation failed"))
  (setq dot_prod_dir (apply '+ (mapcar '* vec_dir vec_tangent_ref)))
  (setq dir_factor (if (< dot_prod_dir 0.0) -1.0 1.0))
  (setq vec_side_ref (mapcar '- (trans pt_side_ref 1 0) pt_ref_on_pline))
  (setq vec_perp_ref (list (- (cadr vec_tangent_ref)) (car vec_tangent_ref) 0.0))
  (setq dot_prod_side (apply '+ (mapcar '* vec_side_ref vec_perp_ref)))
  (setq side_factor (if (< dot_prod_side 0.0) -1.0 1.0))
  (if (= dir_factor 1.0) (setq picket_at_start (- val_ref dist_ref_on_pline)) (setq picket_at_start (+ val_ref dist_ref_on_pline)))
  (setq pline_len (vlax-curve-getDistAtParam pline_obj (vlax-curve-getEndParam pline_obj)))
  (if (not pline_len) (*error* "Length calculation failed"))
  (princ (strcat "\nЗагальна довжина полілінії: " (rtos pline_len 2 4) " м."))

  ;; Визначення діапазону 100-метрових пікетів --- ВИКОРИСТАННЯ FIX ---
  (princ "\n*** Увага: Використовується FIX замість CEILING/FLOOR через помилки середовища LISP. Можлива неточність у першому/останньому пікеті.")
  (if (= dir_factor 1.0)
    (progn (setq first_picket_val (* (fix (+ (/ (+ picket_at_start fuzz) 100.0) (- 1.0 fuzz))) 100.0)) ; Імітація ceiling
           (setq last_picket_val (* (fix (/ (- (+ picket_at_start pline_len) fuzz) 100.0)) 100.0)))      ; Імітація floor (для +)
    (progn (setq first_picket_val (* (fix (/ (- picket_at_start fuzz) 100.0)) 100.0)) ; Імітація floor (може бути неточно для -)
           (setq last_picket_val (* (fix (+ (/ (+ (- picket_at_start pline_len) fuzz) 100.0) (- 1.0 fuzz))) 100.0))) ; Імітація ceiling
  )
  (princ (strcat "\nРозрахований діапазон (з FIX): " (rtos first_picket_val) " до " (rtos last_picket_val)))
  ;; ------------------------------------------------------------------

  (princ (strcat "\nШукаємо пікети від " (rtos (min first_picket_val last_picket_val) 2 1) " до " (rtos (max first_picket_val last_picket_val) 2 1)))

  ;; Перевірка/створення шару - НЕ ПОТРІБНО, шар "0" завжди існує
  ;; (if (not (EnsureLayer target_layer ...)) (*error* "Layer creation failed"))
  (setvar "CLAYER" target_layer) ; <--- Встановлюємо поточний шар в "0"

  ;; Перевірка/створення блоку
  (if (not (MakePicketBlock block_name text_style text_height block_def_layer)) ; Виклик оновленої MakePicketBlock
      (*error* "Block creation/check failed")
  )

  ;; Отримання об'єкту простору моделі
  (setq mspace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (if (not mspace) (*error* "Modelspace failed"))

  ;; --- Цикл розстановки пікетів ---
  (setq current_picket_val first_picket_val)
  (while (if (= dir_factor 1.0) (<= current_picket_val (+ last_picket_val fuzz)) (>= current_picket_val (- last_picket_val fuzz)))
    (if (= dir_factor 1.0) (setq dist_on_pline (- current_picket_val picket_at_start)) (setq dist_on_pline (- picket_at_start current_picket_val)))
    (if (and (>= dist_on_pline (- 0.0 fuzz)) (<= dist_on_pline (+ pline_len fuzz)))
      (progn
        (setq pt_on_pline (vlax-curve-getPointAtDist pline_obj dist_on_pline))
        (setq vec_tangent (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtDist pline_obj dist_on_pline)))
        (if (and pt_on_pline vec_tangent (< (distance '(0 0 0) vec_tangent) fuzz))
            (princ (strcat "\n*** Попередження: Нульовий вектор дотичної на відстані " (rtos dist_on_pline) ". Пропуск пікету."))
            (if (and pt_on_pline vec_tangent)
              (progn
                (setq vec_perp (list (- (cadr vec_tangent)) (car vec_tangent) 0.0))
                (setq vec_perp_final (if (= side_factor 1.0) vec_perp (mapcar '- vec_perp)))
                (setq block_angle (angle '(0.0 0.0 0.0) vec_perp_final))
                (setq piket_str (strcat "ПК" (itoa (fix (+ (/ current_picket_val 100.0) fuzz)))))
                (princ (strcat "\n Вставка маркера: " piket_str " на відстані " (rtos dist_on_pline 2 2) " на шар " (getvar "CLAYER"))) ; Перевірка поточного шару
                (setq inserted_block (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_on_pline) block_name 1.0 1.0 1.0 block_angle)))
                (if (vl-catch-all-error-p inserted_block) (princ (strcat "\n*** Помилка під час vla-InsertBlock: " (vl-catch-all-error-message inserted_block)))
                    (if inserted_block
                       (if (= :vlax_true (vla-get-HasAttributes inserted_block))
                          (progn
                            (setq atts (vl-catch-all-apply 'vlax-invoke (list inserted_block 'GetAttributes)))
                            (if (not (vl-catch-all-error-p atts))
                              (if atts (foreach att atts (if (= (strcase (vla-get-TagString att)) (strcase att_tag)) (vl-catch-all-apply 'vla-put-TextString (list att piket_str)))))
                            )
                            (vl-catch-all-apply 'vlax-invoke (list inserted_block 'Update))
                          )
                       )
                       (princ "\n*** Помилка: vla-InsertBlock повернув nil!")
                    )
                )
              )
            )
        )
      )
    )
    (setq current_picket_val (+ current_picket_val (* dir_factor 100.0)))
  ) ; while loop

  ;; --- Завершення ---
  (command "_REGEN") ; Оновити екран для відображення атрибутів
  (princ "\nРозстановка пікетажу завершена.")

  ;; Відновлення системних змінних та стандартного обробника помилок
  (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars))
  (setq *error* nil) ; Відновити стандартний обробник
  (princ) ; Тихий вихід
)

;; Повідомлення про завантаження - ЗМІНЕНО
(princ "\nСкрипт для розстановки пікетажу завантажено. Введіть 'CREATE_PICKETMARKER' v8 для запуску.")
(princ)