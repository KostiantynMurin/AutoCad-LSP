;;; Скрипт для розстановки пікетажу вздовж полілінії AutoCAD (LWPOLYLINE)
;;; Версія v2025-04-25_UseBlock (Використання блоку користувача з атрибутом "НОМЕР")
;;; Розставляє екземпляри обраного блоку кожні 100м, а також на початку/кінці
;;; полілінії (якщо пікет >= 0). Використовує FIX замість floor/ceiling.

;; === Допоміжні функції для векторної математики ===
(defun normalize (v / len) (setq len (distance '(0 0 0) v)) (if (< len 1e-12) nil (mapcar '(lambda (x) (/ x len)) v)))
(defun v_scale (v s) (mapcar '(lambda (x) (* x s)) v))
(defun v_add (v1 v2) (mapcar '+ v1 v2))
(defun v_sub (v1 v2) (mapcar '- v1 v2))

;; === Допоміжна функція для форматування значення пікету ===
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

;; === Допоміжна функція для перевірки наявності атрибуту в блоці ===
(defun CheckBlockAttrib (blockname att_tag / blk_obj ent attdef_found acad_obj doc blocks)
  (setq attdef_found nil)
  (setq acad_obj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acad_obj))
  (setq blocks (vla-get-Blocks doc))
  ;; Перевірка чи існує блок з таким ім'ям взагалі
  (if (not (vl-catch-all-error-p (setq blk_obj (vla-item blocks blockname))))
      ;; Якщо блок існує, шукаємо атрибут
      (vlax-for ent blk_obj
        (if (= "AcDbAttributeDefinition" (vla-get-ObjectName ent))
          (if (= (strcase (vla-get-TagString ent)) (strcase att_tag))
            (setq attdef_found T) ; Знайдено!
          )
        )
      )
      (princ (strcat "\n*** Помилка: Блок з ім'ям '" blockname "' не знайдено в таблиці блоків."))
  )
  attdef_found ; Повертає T якщо знайдено, nil інакше
)


;; === Допоміжна функція для встановлення значення атрибуту ===
(defun SetAttributeValue (block_vla_obj att_tag new_value / atts att found update_needed)
  (setq found nil update_needed nil)
  ;; Перевіряємо чи передано валідний об'єкт VLA
  (if (and block_vla_obj (= (type block_vla_obj) 'VLA-OBJECT) (not (vlax-object-released-p block_vla_obj)))
      ;; Перевіряємо чи є у вставці блоку атрибути
      (if (= :vlax_true (vla-get-HasAttributes block_vla_obj))
        (progn
          ;; Отримуємо колекцію атрибутів
          (setq atts (vl-catch-all-apply 'vlax-invoke (list block_vla_obj 'GetAttributes)))
          (if (vl-catch-all-error-p atts)
              (princ (strcat "\n*** Помилка отримання атрибутів для об'єкта: " (vl-princ-to-string block_vla_obj)))
              (if atts
                  ;; Перебираємо атрибути
                  (foreach att atts
                    ;; Порівнюємо тег (регістронезалежно)
                    (if (= (strcase (vla-get-TagString att)) (strcase att_tag))
                      (progn
                        ;; Якщо значення відрізняється, встановлюємо нове
                        (if (/= (vla-get-TextString att) new_value)
                           (progn
                              (vl-catch-all-apply 'vla-put-TextString (list att new_value))
                              (setq update_needed T) ; Позначка, що потрібно оновити блок
                           )
                        )
                        (setq found T) ; Позначка, що атрибут знайдено
                      ) ; progn if tag matches
                    ) ; if tag matches
                  ) ; foreach
                  (princ (strcat "\n*** Помилка: Не вдалося отримати атрибути для об'єкта: " (vl-princ-to-string block_vla_obj)))
              )
          )
          ;; Оновлюємо відображення блоку, якщо значення атрибуту було змінено
          (if update_needed (vla-Update block_vla_obj))
        ) ; progn if has attributes
        (princ (strcat "\n*** Попередження: Вставлений блок " (vla-get-Name block_vla_obj) " не має атрибутів."))
      ) ; if has attributes
      (princ "\n*** Помилка: Передано невалідний об'єкт блоку для SetAttributeValue.")
  ) ; if valid VLA object
  found ; Повертає T якщо атрибут було знайдено (незалежно від того, чи змінювалось значення)
)


;; === Головна функція ===
(defun C:CREATE_PICKETMARKER (/ *error* old_vars pline_ent pline_obj pt_ref pt_ref_on_pline dist_ref_on_pline
                             val_ref pt_dir vec_dir vec_tangent_ref dir_factor pt_side_ref vec_side_ref
                             vec_perp_ref dot_prod_side side_factor picket_at_start pline_len picket_at_end
                             first_picket_val last_picket_val current_picket_val dist_on_pline
                             pt_on_pline vec_tangent ; Видалено змінні для геометрії
                             target_layer block_name_selected block_vla_obj block_insert_obj ; Змінні для блоку
                             fuzz piket_str piket_str_start piket_str_end
                             pt_start pt_end vec_tangent_start vec_tangent_end block_angle mspace
                             acad_obj doc blocks blk_obj ent attdef_found update_needed att atts) ; Додаткові змінні

  (princ "\n*** Running CREATE_PICKETMARKER v2025-04-25_UseBlock ***") ; <<< Оновлено версію

  ;; Налаштування констант
  (setq target_layer   "0"         ; Шар для вставки блоків
        fuzz           1e-9
  )

  ;; Перевизначення обробника помилок
  (defun *error* (msg)
    (if old_vars (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars)))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "Відміна користувачем" "Block check failed")))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ)
  )

  ;; Збереження системних змінних
  (setq old_vars (mapcar '(lambda (v) (cons v (getvar v))) '("CMDECHO" "OSMODE" "CLAYER" "ATTREQ" "ATTDIA")))
  (setvar "CMDECHO" 0) (setvar "ATTREQ" 1) (setvar "ATTDIA" 0)

  ;; --- Збір вхідних даних ---
  (princ "\nРозстановка пікетажу (з використанням блоку користувача).")

  ;; --- 1. Вибір блоку користувачем ---
  (setq block_name_selected nil)
  (while (not block_name_selected)
     (setq block_insert_obj nil) ; Скидаємо змінну
     (setq block_ent (entsel "\nОберіть екземпляр блоку, який буде використовуватися для маркера: "))
     (if block_ent
        (progn
            (setq block_vla_obj (vlax-ename->vla-object (car block_ent)))
            (if (and block_vla_obj (= "AcDbBlockReference" (vla-get-ObjectName block_vla_obj)))
                (progn
                  (setq block_name_selected (vla-get-EffectiveName block_vla_obj))
                  (princ (strcat "\nОбрано блок: '" block_name_selected "'."))
                  ;; Перевірка на наявність атрибуту "НОМЕР"
                  (if (not (CheckBlockAttrib block_name_selected "НОМЕР"))
                      (progn
                        (princ (strcat "\n*** Помилка: Обраний блок '" block_name_selected "' не містить атрибуту з тегом 'НОМЕР'. Оберіть інший блок."))
                        (setq block_name_selected nil) ; Скидаємо ім'я, щоб цикл вибору продовжився
                      )
                      (princ "\n -> Атрибут 'НОМЕР' знайдено в блоці.")
                  )
                )
                (princ "\nОбраний об'єкт не є вставкою блоку (Block Reference). Спробуйте ще раз.")
            )
        )
        (*error* "Відміна користувачем") ; Якщо користувач натиснув Esc при виборі блоку
     )
  ) ; while not block_name_selected

  ;; --- 2. Вибір полілінії ---
  (while (not pline_obj)
    (setq pline_ent (entsel "\nОберіть 2D полілінію (LWPOLYLINE): "))
    (if (and pline_ent (= "LWPOLYLINE" (cdr (assoc 0 (entget (car pline_ent))))))
      (setq pline_obj (vlax-ename->vla-object (car pline_ent)))
      (princ "\nОбраний об'єкт не є LWPOLYLINE. Спробуйте ще раз.")
    )
  )

  ;; --- 3. Інші вхідні дані ---
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
  (setq pt_side_ref (getpoint pt_ref_on_pline "\nВкажіть сторону для розміщення блоку: ")) ; Запит сторони лишається
  (if (not pt_side_ref) (*error* "Відміна користувачем"))

  ;; Перевірка валідності точок
  (if (not (and pt_ref_on_pline (= 'LIST (type pt_ref_on_pline)) (= 3 (length pt_ref_on_pline)))) (*error* "Reference point on polyline invalid"))
  (if (not (and pt_dir (= 'LIST (type pt_dir)) (= 3 (length pt_dir)))) (*error* "Direction point invalid"))
  (if (not (and pt_side_ref (= 'LIST (type pt_side_ref)) (= 3 (length pt_side_ref)))) (*error* "Side point invalid"))

  ;; --- Розрахунки ---
  (command "_REGEN")
  (setq vec_dir (mapcar '- (trans pt_dir 1 0) pt_ref_on_pline))
  (setq vec_tangent_ref (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtPoint pline_obj pt_ref_on_pline)))
  (if (not vec_tangent_ref) (*error* "Tangent calculation failed at ref point"))
  (setq dot_prod_dir (apply '+ (mapcar '* vec_dir vec_tangent_ref)))
  (setq dir_factor (if (< dot_prod_dir 0.0) -1.0 1.0))
  ;; Розрахунок фактору сторони (для визначення перпендикуляру)
  (setq vec_side_ref (mapcar '- (trans pt_side_ref 1 0) pt_ref_on_pline))
  (setq vec_perp_ref (list (- (cadr vec_tangent_ref)) (car vec_tangent_ref) 0.0))
  (setq dot_prod_side (apply '+ (mapcar '* vec_side_ref vec_perp_ref)))
  (setq side_factor (if (< dot_prod_side 0.0) -1.0 1.0))
  ;; --------------------------------------------
  (if (= dir_factor 1.0) (setq picket_at_start (- val_ref dist_ref_on_pline)) (setq picket_at_start (+ val_ref dist_ref_on_pline)))
  (setq pline_len (vlax-curve-getDistAtParam pline_obj (vlax-curve-getEndParam pline_obj)))
  (if (not pline_len) (*error* "Length calculation failed"))
  (princ (strcat "\nЗагальна довжина полілінії: " (rtos pline_len 2 4) " м."))
  (princ (strcat "\nРозрахункове значення пікету на початку: " (rtos picket_at_start 2 4) " м."))

  ;; Визначення діапазону 100-метрових пікетів --- ВИКОРИСТАННЯ FIX ---
  (princ "\n*** Увага: Використовується FIX замість CEILING/FLOOR через помилки середовища LISP. Можлива неточність у першому/останньому пікеті.")
  (if (= dir_factor 1.0)
    (progn (setq first_picket_val (* (fix (+ (/ (+ picket_at_start fuzz) 100.0) (- 1.0 fuzz))) 100.0))
           (setq last_picket_val (* (fix (/ (- (+ picket_at_start pline_len) fuzz) 100.0)) 100.0)))
    (progn (setq first_picket_val (* (fix (/ (- picket_at_start fuzz) 100.0)) 100.0))
           (setq last_picket_val (* (fix (+ (/ (+ (- picket_at_start pline_len) fuzz) 100.0) (- 1.0 fuzz))) 100.0)))
  )
  (princ (strcat "\nРозрахований діапазон (з FIX): " (rtos first_picket_val) " до " (rtos last_picket_val)))

  ;; --- Підготовка до розстановки ---
  (setvar "CLAYER" target_layer) ; Встановлюємо поточний шар "0"
  (setq mspace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))) ; Отримуємо Modelspace
  (if (not mspace) (*error* "Modelspace failed"))

  ;; --- Маркер на ПОЧАТКУ полілінії (якщо пікет >= 0) ---
  (if (>= picket_at_start (- 0.0 fuzz))
      (progn
        (princ "\nСпроба поставити маркер на початку полілінії...")
        (setq pt_start (vlax-curve-getStartPoint pline_obj))
        (setq vec_tangent_start (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getStartParam pline_obj)))
        (if vec_tangent_start
            (progn
              (setq vec_perp (list (- (cadr vec_tangent_start)) (car vec_tangent_start) 0.0))
              (setq vec_perp_final (if (= side_factor 1.0) vec_perp (mapcar '- vec_perp)))
              (setq block_angle (angle '(0.0 0.0 0.0) vec_perp_final)) ; Кут повороту
              (setq piket_str_start (FormatPicketValue picket_at_start)) ; Форматований текст
              ;; Вставка блоку
              (setq block_insert_obj (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_start) block_name_selected 1.0 1.0 1.0 block_angle)))
              (if (vl-catch-all-error-p block_insert_obj)
                  (princ (strcat "\n*** Помилка вставки блоку на початку: " (vl-catch-all-error-message block_insert_obj)))
                  (if block_insert_obj
                      (progn
                        (princ (strcat "\n Вставлено блок для: " piket_str_start))
                        (SetAttributeValue block_insert_obj "НОМЕР" piket_str_start) ; Встановлення атрибуту
                      )
                      (princ "\n*** Помилка: vla-InsertBlock повернув nil на початку.")
                  )
              )
            )
            (princ (strcat "\n*** Попередження: Не вдалося отримати дотичну в початковій точці. Маркер не створено."))
        )
      )
      (princ (strcat "\n--- Пропуск маркера на початку полілінії (Пікет=" (rtos picket_at_start 2 2) " < 0)."))
  )

  ;; --- Цикл розстановки 100-метрових пікетів ---
  (princ (strcat "\nШукаємо 100м пікети від " (rtos (min first_picket_val last_picket_val) 2 1) " до " (rtos (max first_picket_val last_picket_val) 2 1)))
  (setq current_picket_val first_picket_val)
  (while (if (= dir_factor 1.0) (<= current_picket_val (+ last_picket_val fuzz)) (>= current_picket_val (- last_picket_val fuzz)))
    (if (= dir_factor 1.0) (setq dist_on_pline (- current_picket_val picket_at_start)) (setq dist_on_pline (- picket_at_start current_picket_val)))
    (if (and (>= current_picket_val (- 0.0 fuzz))
             (> dist_on_pline fuzz)
             (< dist_on_pline (- pline_len fuzz))
             (>= dist_on_pline (- 0.0 fuzz))
             (<= dist_on_pline (+ pline_len fuzz)))
      (progn
          (setq pt_on_pline (vlax-curve-getPointAtDist pline_obj dist_on_pline))
          (setq vec_tangent (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtDist pline_obj dist_on_pline)))
          (if vec_tangent
             (progn
                (setq vec_perp (list (- (cadr vec_tangent)) (car vec_tangent) 0.0))
                (setq vec_perp_final (if (= side_factor 1.0) vec_perp (mapcar '- vec_perp)))
                (setq block_angle (angle '(0.0 0.0 0.0) vec_perp_final))
                ;; Форматуємо пікет - використовуємо повний формат і для 100м
                (setq piket_str (FormatPicketValue current_picket_val))
                ;; Вставка блоку
                (setq block_insert_obj (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_on_pline) block_name_selected 1.0 1.0 1.0 block_angle)))
                (if (vl-catch-all-error-p block_insert_obj)
                    (princ (strcat "\n*** Помилка вставки блоку для " piket_str ": " (vl-catch-all-error-message block_insert_obj)))
                    (if block_insert_obj
                        (progn
                          (princ (strcat "\n Вставлено блок для: " piket_str))
                          (SetAttributeValue block_insert_obj "НОМЕР" piket_str)
                        )
                        (princ (strcat "\n*** Помилка: vla-InsertBlock повернув nil для " piket_str))
                    )
                )
             )
             (princ (strcat "\n*** Попередження: Не вдалося отримати дотичну на відстані " (rtos dist_on_pline) ". Пропуск 100м пікету."))
          )
      )
      (progn ; ELSE
         (if (< current_picket_val (- 0.0 fuzz))
             (princ (strcat "\n--- Пропуск 100м пікету " (rtos current_picket_val 2 1) " (від'ємне значення).")))
         (if (or (<= dist_on_pline fuzz) (>= dist_on_pline (- pline_len fuzz)))
             (princ (strcat "\n--- Пропуск 100м пікету " (rtos current_picket_val 2 1) " (збігається або близько до початку/кінця).")))
      ) ; progn (else) end
    ) ; IF END
    (setq current_picket_val (+ current_picket_val (* dir_factor 100.0)))
  ) ; WHILE END

  ;; --- Маркер в КІНЦІ полілінії ---
  (if (= dir_factor 1.0) (setq picket_at_end (+ picket_at_start pline_len)) (setq picket_at_end (- picket_at_start pline_len)))
  (princ (strcat "\nРозрахункове значення пікету в кінці: " (rtos picket_at_end 2 4) " м."))
  (if (>= picket_at_end (- 0.0 fuzz))
      (progn
        (princ "\nСпроба поставити маркер в кінці полілінії...")
        (setq pt_end (vlax-curve-getEndPoint pline_obj))
        (setq vec_tangent_end (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtDist pline_obj (- pline_len fuzz))))
        (if (not vec_tangent_end) (setq vec_tangent_end (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getEndParam pline_obj))))
        (if vec_tangent_end
            (progn
              (setq vec_perp (list (- (cadr vec_tangent_end)) (car vec_tangent_end) 0.0))
              (setq vec_perp_final (if (= side_factor 1.0) vec_perp (mapcar '- vec_perp)))
              (setq block_angle (angle '(0.0 0.0 0.0) vec_perp_final))
              (setq piket_str_end (FormatPicketValue picket_at_end))
              ;; Вставка блоку
              (setq block_insert_obj (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_end) block_name_selected 1.0 1.0 1.0 block_angle)))
              (if (vl-catch-all-error-p block_insert_obj)
                  (princ (strcat "\n*** Помилка вставки блоку в кінці: " (vl-catch-all-error-message block_insert_obj)))
                  (if block_insert_obj
                      (progn
                        (princ (strcat "\n Вставлено блок для: " piket_str_end))
                        (SetAttributeValue block_insert_obj "НОМЕР" piket_str_end)
                      )
                      (princ "\n*** Помилка: vla-InsertBlock повернув nil в кінці.")
                  )
              )
            )
            (princ (strcat "\n*** Попередження: Не вдалося отримати дотичну в кінцевій точці. Маркер не створено."))
        )
      )
      (princ (strcat "\n--- Пропуск маркера в кінці полілінії (Пікет=" (rtos picket_at_end 2 2) " < 0)."))
  )

  ;; --- Завершення ---
  (command "_REGEN")
  (princ "\nРозстановка пікетажу (блоками) завершена.")
  (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars))
  (setq *error* nil)
  (princ)
) ; Defun C:CREATE_PICKETMARKER End

;; Повідомлення про завантаження
(princ "\nСкрипт для розстановки пікетажу БЛОКАМИ завантажено. Введіть 'CREATE_PICKETMARKER' для запуску.")
(princ)