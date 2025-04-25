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


;; === Допоміжна функція для встановлення значення атрибуту (з налагодженням) ===
(defun SetAttributeValue (block_vla_obj att_tag new_value / atts att found update_needed has_attribs current_tag set_result) ; Додано змінні
  (setq found nil update_needed nil)
  (princ (strcat "\n  Debug [SetAttrib]: Setting '" att_tag "' to '" new_value "' for " (vl-princ-to-string block_vla_obj))) ; DEBUG
  (if (and block_vla_obj (= (type block_vla_obj) 'VLA-OBJECT) (not (vlax-object-released-p block_vla_obj)))
      (progn
        (setq has_attribs (vla-get-HasAttributes block_vla_obj))
        (princ (strcat "\n  Debug [SetAttrib]: HasAttributes = " (vl-princ-to-string has_attribs))) ; DEBUG
        (if (= :vlax_true has_attribs)
          (progn
            (princ "\n  Debug [SetAttrib]: Getting attributes...") ; DEBUG
            (setq atts (vl-catch-all-apply 'vlax-invoke (list block_vla_obj 'GetAttributes)))
            (if (vl-catch-all-error-p atts)
                (princ (strcat "\n  Debug [SetAttrib]: *** Помилка отримання атрибутів: " (vl-catch-all-error-message atts)))
                (if (and atts (or (= (type atts) 'LIST) (= (type atts) 'SAFEARRAY))) ; Перевірка чи список/масив не порожній
                   (progn ; Атрибути отримано, перебираємо
                     (princ (strcat "\n  Debug [SetAttrib]: Знайдено атрибути (тип: " (vl-princ-to-string (type atts)) "). Ітерація...")) ; DEBUG
                     (foreach att (vlax-safearray->list atts) ; Конвертуємо в список для foreach
                       (setq current_tag (vla-get-TagString att))
                       (princ (strcat "\n    Debug [SetAttrib]: Перевірка Тегу: '" current_tag "'")) ; DEBUG
                       (if (= (strcase current_tag) (strcase att_tag)) ; Порівняння без урахування регістру
                         (progn
                           (princ (strcat "\n      Debug [SetAttrib]: Тег СПІВПАВ! Поточне значення: '" (vla-get-TextString att) "'")) ; DEBUG
                           ;; Встановлюємо значення без перевірки на відмінність
                           (princ (strcat "\n      Debug [SetAttrib]: Спроба встановити значення в '" new_value "'...")) ; DEBUG
                           (setq set_result (vl-catch-all-apply 'vla-put-TextString (list att new_value)))
                           (if (vl-catch-all-error-p set_result)
                               (princ (strcat "\n      Debug [SetAttrib]: *** Помилка встановлення значення: " (vl-catch-all-error-message set_result)))
                               (princ "\n      Debug [SetAttrib]: Значення встановлено (або спроба зроблена).") ; DEBUG
                           )
                           (setq update_needed T) ; Позначка для оновлення
                           (setq found T)
                         ) ; progn if tag matches
                       ) ; if tag matches
                     ) ; foreach
                   ) ; progn iterate attributes
                   (princ "\n  Debug [SetAttrib]: GetAttributes повернув nil або порожній список.") ; DEBUG
                ) ; If Atts not error or nil/empty
            ) ; Check GetAttributes Error/Result
            (if update_needed
                (progn
                  (princ "\n  Debug [SetAttrib]: Виклик vla-Update...") ; DEBUG
                  (vla-Update block_vla_obj)
                  (princ "\n  Debug [SetAttrib]: vla-Update завершено.") ; DEBUG
                )
            )
            (if (not found) (princ (strcat "\n  Debug [SetAttrib]: *** Атрибут з тегом '" att_tag "' не знайдено серед атрибутів блоку."))) ; DEBUG
          ) ; progn if has attributes
          (princ (strcat "\n*** Попередження: Вставлений блок " (vla-get-Name block_vla_obj) " не має атрибутів (або HasAttributes=False)."))
        ) ; if has attributes
      ) ; progn if valid object
      (princ "\n*** Помилка: Передано невалідний об'єкт блоку для SetAttributeValue.")
  ) ; if valid VLA object
  found ; Повернути T якщо атрибут знайдено
)


;; Головна функція (Додано перевірку ATTREQ та vla-Update після вставки)
(defun C:CREATE_PICKETMARKER (/ *error* old_vars pline_ent pline_obj pt_ref pt_ref_on_pline dist_ref_on_pline
                             val_ref pt_dir vec_dir vec_tangent_ref dir_factor pt_side_ref vec_side_ref
                             vec_perp_ref dot_prod_side side_factor picket_at_start pline_len picket_at_end
                             first_picket_val last_picket_val current_picket_val dist_on_pline
                             pt_on_pline vec_tangent target_layer block_name_selected block_vla_obj block_insert_obj
                             fuzz piket_str piket_str_start piket_str_end
                             pt_start pt_end vec_tangent_start vec_tangent_end block_angle mspace
                             acad_obj doc blocks blk_obj ent attdef_found update_needed att atts vec_perp vec_perp_final) ; Додано пропущені змінні

  (princ "\n*** Running CREATE_PICKETMARKER v2025-04-25_UseBlock_AttreqUpdate ***") ; <<< Оновлено версію

  ;; Налаштування констант
  (setq target_layer   "0"
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
  (princ (strcat "\nПеревірка перед стартом: ATTREQ=" (itoa (getvar "ATTREQ")) ", ATTDIA=" (itoa (getvar "ATTDIA")))) ; Перевірка

  ;; --- Збір вхідних даних ---
  (princ "\nРозстановка пікетажу (з використанням блоку користувача).")
  (setq block_name_selected nil)
  (while (not block_name_selected)
     (setq block_insert_obj nil)
     (setq block_ent (entsel "\nОберіть екземпляр блоку, який буде використовуватися для маркера: "))
     (if block_ent
        (progn
            (setq block_vla_obj (vlax-ename->vla-object (car block_ent)))
            (if (and block_vla_obj (= "AcDbBlockReference" (vla-get-ObjectName block_vla_obj)))
                (progn
                  (setq block_name_selected (vla-get-EffectiveName block_vla_obj))
                  (princ (strcat "\nОбрано блок: '" block_name_selected "'."))
                  (if (not (CheckBlockAttrib block_name_selected "НОМЕР"))
                      (progn (princ (strcat "\n*** Помилка: Обраний блок '" block_name_selected "' не містить атрибуту з тегом 'НОМЕР'. Оберіть інший блок.")) (setq block_name_selected nil))
                      (princ "\n -> Атрибут 'НОМЕР' знайдено в блоці.")
                  )
                )
                (princ "\nОбраний об'єкт не є вставкою блоку. Спробуйте ще раз.")
            )
        )
        (*error* "Відміна користувачем")
     )
  ) ; while not block_name_selected

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
  (setq pt_side_ref (getpoint pt_ref_on_pline "\nВкажіть сторону для розміщення блоку: "))
  (if (not pt_side_ref) (*error* "Відміна користувачем"))

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
  (setq vec_side_ref (mapcar '- (trans pt_side_ref 1 0) pt_ref_on_pline))
  (setq vec_perp_ref (list (- (cadr vec_tangent_ref)) (car vec_tangent_ref) 0.0))
  (setq dot_prod_side (apply '+ (mapcar '* vec_side_ref vec_perp_ref)))
  (setq side_factor (if (< dot_prod_side 0.0) -1.0 1.0))
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
  (setq mspace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
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
              (setq block_angle (angle '(0.0 0.0 0.0) vec_perp_final))
              (setq piket_str_start (FormatPicketValue picket_at_start))
              ;; Перевірка ATTREQ/ATTDIA перед вставкою
              (princ (strcat "\n  Debug [Insert Start]: ATTREQ=" (itoa (getvar "ATTREQ")) ", ATTDIA=" (itoa (getvar "ATTDIA"))))
              ;; Вставка блоку
              (setq block_insert_obj (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_start) block_name_selected 1.0 1.0 1.0 block_angle)))
              (if (vl-catch-all-error-p block_insert_obj)
                  (princ (strcat "\n*** Помилка вставки блоку на початку: " (vl-catch-all-error-message block_insert_obj)))
                  (if block_insert_obj
                      (progn
                        ;; Примусове оновлення перед перевіркою атрибутів
                        (princ "\n  Debug [Insert Start]: Оновлення вставленого об'єкта...")
                        (vla-Update block_insert_obj) ; <--- ДОДАНО ОНОВЛЕННЯ
                        (princ " Завершено.")
                        ;; Встановлення атрибуту
                        (princ (strcat "\n Вставлено блок для: " piket_str_start))
                        (SetAttributeValue block_insert_obj "НОМЕР" piket_str_start)
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
                (setq piket_str (FormatPicketValue current_picket_val))
                ;; Перевірка ATTREQ/ATTDIA перед вставкою
                (princ (strcat "\n  Debug [Insert Loop]: ATTREQ=" (itoa (getvar "ATTREQ")) ", ATTDIA=" (itoa (getvar "ATTDIA"))))
                ;; Вставка блоку
                (setq block_insert_obj (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_on_pline) block_name_selected 1.0 1.0 1.0 block_angle)))
                (if (vl-catch-all-error-p block_insert_obj)
                    (princ (strcat "\n*** Помилка вставки блоку для " piket_str ": " (vl-catch-all-error-message block_insert_obj)))
                    (if block_insert_obj
                        (progn
                          ;; Примусове оновлення перед перевіркою атрибутів
                          (princ "\n  Debug [Insert Loop]: Оновлення вставленого об'єкта...")
                          (vla-Update block_insert_obj) ; <--- ДОДАНО ОНОВЛЕННЯ
                          (princ " Завершено.")
                          ;; Встановлення атрибуту
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
              ;; Перевірка ATTREQ/ATTDIA перед вставкою
              (princ (strcat "\n  Debug [Insert End]: ATTREQ=" (itoa (getvar "ATTREQ")) ", ATTDIA=" (itoa (getvar "ATTDIA"))))
              ;; Вставка блоку
              (setq block_insert_obj (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_end) block_name_selected 1.0 1.0 1.0 block_angle)))
              (if (vl-catch-all-error-p block_insert_obj)
                  (princ (strcat "\n*** Помилка вставки блоку в кінці: " (vl-catch-all-error-message block_insert_obj)))
                  (if block_insert_obj
                      (progn
                        ;; Примусове оновлення перед перевіркою атрибутів
                        (princ "\n  Debug [Insert End]: Оновлення вставленого об'єкта...")
                        (vla-Update block_insert_obj) ; <--- ДОДАНО ОНОВЛЕННЯ
                        (princ " Завершено.")
                        ;; Встановлення атрибуту
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