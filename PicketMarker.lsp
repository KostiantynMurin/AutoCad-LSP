;;; Скрипт для розстановки пікетажу вздовж полілінії AutoCAD (LWPOLYLINE)
;;; Версія v2025-06-29_UseBlock_RotateFixY_RemAngle_XDATA_FINAL_ATTEMPT (Використання блоку користувача з атрибутом "ПІКЕТ")
;;; Розставляє екземпляри обраного блоку кожні 100м, а також на початку/кінці
;;; полілінії (якщо пікет >= 0). Використовує FIX замість floor/ceiling.
;;; Оновлення: Зберігає дані пікетажу (picket_at_start, dir_factor) в XDATA на полілінії.

(vl-load-com) ; Завантажуємо VLAX-функції на старті, щоб уникнути проблем з ініціалізацією.

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
  (if (not (vl-catch-all-error-p (setq blk_obj (vlax-invoke-method blocks 'Item blockname))))
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

;; === Допоміжна функція для встановлення значення атрибуту (обробка типу атрибутів) ===
(defun SetAttributeValue (block_vla_obj att_tag new_value / atts att found update_needed has_attribs current_tag set_result att_list)
  (setq found nil update_needed nil)
  (if (and block_vla_obj (= (type block_vla_obj) 'VLA-OBJECT) (not (vlax-object-released-p block_vla_obj)))
      (progn
        (setq has_attribs (vla-get-HasAttributes block_vla_obj))
        (if (and has_attribs (/= :vlax_false has_attribs))
            (progn
              (setq atts (vl-catch-all-apply 'vlax-invoke (list block_vla_obj 'GetAttributes)))
              (if (vl-catch-all-error-p atts)
                  (princ (strcat "\n  Debug [SetAttrib]: *** Помилка отримання атрибутів: " (vl-catch-all-error-message atts)))
                  (if atts
                     (progn
                       (setq att_list nil)
                       (cond
                         ((= (type atts) 'VARIANT)
                           (if (and (= (type (vlax-variant-value atts)) 'SAFEARRAY))
                              (setq att_list (vlax-safearray->list (vlax-variant-value atts)))
                           )
                         )
                         ((= (type atts) 'LIST)
                           (setq att_list atts)
                         )
                         ((= (type atts) 'SAFEARRAY)
                           (setq att_list (vlax-safearray->list atts))
                         )
                         (T)
                       )
                       (if att_list
                          (progn
                             (foreach att att_list
                               (if (= (type att) 'VLA-OBJECT)
                                   (progn
                                      (setq current_tag (vla-get-TagString att))
                                      (if (= (strcase current_tag) (strcase att_tag))
                                        (progn
                                          (setq set_result (vl-catch-all-apply 'vla-put-TextString (list att new_value)))
                                          (if (vl-catch-all-error-p set_result)
                                              (princ (strcat "\n      Debug [SetAttrib]: *** Помилка встановлення значення: " (vl-catch-all-error-message set_result)))
                                          )
                                          (setq update_needed T) (setq found T)
                                        )
                                      )
                                   )
                                   (princ (strcat "\n    Debug [SetAttrib]: *** Помилка: Елемент списку атрибутів не є VLA-OBJECT: " (vl-princ-to-string att)))
                               )
                             )
                          )
                       )
                     )
                  )
              )
              (if update_needed (vla-Update block_vla_obj))
              (if (not found) (princ (strcat "\n  Debug [SetAttrib]: *** Атрибут з тегом '" att_tag "' не знайдено серед атрибутів блоку.")))
        )
            (princ (strcat "\n  Debug [SetAttrib]: Check FAILED (HasAttributes is nil or False)."))
        )
      )
      (princ "\n*** Помилка: Передано невалідний об'єкт блоку для SetAttributeValue.")
  )
  found
)

;; === Функції для роботи з XDATA ===
(defun GetAcadObjectAndDoc (/ acad_obj doc)
  (vl-catch-all-apply
    '(lambda ()
       (setq acad_obj (vlax-get-acad-object))
       (setq doc (vla-get-ActiveDocument acad_obj))
     )
  )
  (list acad_obj doc) ; Повертаємо список об'єктів
)

;; *** ОНОВЛЕНА ФУНКЦІЯ RegisterAppID з використанням (command "" "_.-REGAPP" ...) ***
(defun RegisterAppID (app_name / err_check)
  (princ (strcat "\nСпроба реєстрації AppID: " app_name " за допомогою REGAPP..."))
  ;; Додаємо порожню команду (command ""), щоб очистити буфер і забезпечити правильний контекст
  (command "")
  ;; Використовуємо _.-REGAPP команду:
  ;; 'A' означає Add (додати), якщо його немає. Якщо є, нічого не робить, помилок не буде.
  (setq err_check (vl-catch-all-apply 'command (list "_.-REGAPP" app_name "A")))
  
  (if (vl-catch-all-error-p err_check)
      (princ (strcat "\n*** ПОМИЛКА REGAPP: Не вдалося зареєструвати AppID '" app_name "': " (vl-catch-all-error-message err_check)))
      (princ (strcat "\nAppID '" app_name "' зареєстровано (або вже існувало)."))
  )
  (princ (strcat "\nAppID реєстрація для " app_name " завершена."))
)

;; Головна функція (Нормалізація кута через REM)
(defun C:CREATE_PICKET_MARKER (/ *error* old_vars pline_ent pline_obj pt_ref pt_ref_on_pline dist_ref_on_pline
                             val_ref pt_dir vec_dir vec_tangent_ref dir_factor pt_side_ref vec_side_ref
                             vec_perp_ref dot_prod_side side_factor picket_at_start pline_len picket_at_end
                             first_picket_val last_picket_val current_picket_val dist_on_pline
                             pt_on_pline vec_tangent target_layer block_name_selected block_vla_obj block_insert_obj
                             fuzz piket_str piket_str_start piket_str_end
                             pt_start pt_end vec_tangent_start vec_tangent_end block_angle block_angle_perp mspace
                             acad_obj doc blocks blk_obj ent attdef_found update_needed att atts vec_perp vec_perp_final
                             num_fix km_str val_str set_result att_list current_tag has_attribs final_stylename
                            app_id_name result_obj)

  (princ "\n*** Running CREATE_PICKET_MARKER v2025-06-29_UseBlock_RotateFixY_RemAngle_XDATA_FINAL_ATTEMPT ***")

  ;; Налаштування констант
  (setq target_layer   "0"
        fuzz           1e-9
        app_id_name     "PicketMaster" ; Унікальне ім'я для XDATA
  )

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

  ;; Реєстрація AppID (тепер через REGAPP команду)
  (RegisterAppID app_id_name)


  ;; Перевизначення обробника помилок
  (defun *error* (msg)
    (if old_vars (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars)))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "Відміна користувачем" "Block check failed" "Initialization failed")))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ)
  ) ; *error* defun end

  ;; Збереження системних змінних
  (setq old_vars (mapcar '(lambda (v) (cons v (getvar v))) '("CMDECHO" "OSMODE" "CLAYER" "ATTREQ" "ATTDIA")))
  ;; Важливо: CMDECHO має бути 1 під час використання (command) для REGAPP.
  ;; Відновимо його потім.
  (setvar "CMDECHO" 1)
  (setvar "ATTREQ" 1)
  (setvar "ATTDIA" 0)

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
                  (if (not (CheckBlockAttrib block_name_selected "ПІКЕТ"))
                      (progn (princ (strcat "\n*** Помилка: Обраний блок '" block_name_selected "' не містить атрибуту з тегом 'ПІКЕТ'. Оберіть інший блок.")) (setq block_name_selected nil))
                      (princ "\n -> Атрибут 'ПІКЕТ' знайдено в блоці.")
                  )
                )
                (princ "\nОбраний об'єкт не є вставкою блоку. Спробуйте ще раз.")
            )
        )
        (*error* "Відміна користувачем")
     )
  )

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

  ;; Перевірка валідності
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
  (setvar "CLAYER" target_layer)
  (setq mspace (vla-get-ModelSpace doc)) ; Використовуємо doc
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
              (setq block_angle_perp (angle '(0.0 0.0 0.0) vec_perp_final))
              (setq block_angle (- block_angle_perp (/ pi 2.0)))
              (setq block_angle (rem block_angle (* 2.0 pi)))
              (if (< block_angle 0.0) (setq block_angle (+ block_angle (* 2.0 pi))))
              (setq piket_str_start (FormatPicketValue picket_at_start))
              (setq block_insert_obj (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_start) block_name_selected 1.0 1.0 1.0 block_angle)))
              (if (vl-catch-all-error-p block_insert_obj)
                  (princ (strcat "\n*** Помилка вставки блоку на початку: " (vl-catch-all-error-message block_insert_obj)))
                  (if block_insert_obj
                      (progn
                        (princ (strcat "\n Вставлено блок для: " piket_str_start))
                        (SetAttributeValue block_insert_obj "ПІКЕТ" piket_str_start)
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
                (setq block_angle_perp (angle '(0.0 0.0 0.0) vec_perp_final))
                (setq block_angle (- block_angle_perp (/ pi 2.0)))
                (setq block_angle (rem block_angle (* 2.0 pi)))
                (if (< block_angle 0.0) (setq block_angle (+ block_angle (* 2.0 pi))))
                (if (< (abs (rem current_picket_val 100.0)) fuzz)
                    (setq piket_str (strcat "ПК" (itoa (fix (+ (/ current_picket_val 100.0) fuzz)))))
                    (setq piket_str (FormatPicketValue current_picket_val))
                )
                (setq block_insert_obj (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_on_pline) block_name_selected 1.0 1.0 1.0 block_angle)))
                (if (vl-catch-all-error-p block_insert_obj)
                    (princ (strcat "\n*** Помилка вставки блоку для " piket_str ": " (vl-catch-all-error-message block_insert_obj)))
                    (if block_insert_obj
                        (progn
                          (princ (strcat "\n Вставлено блок для: " piket_str))
                          (SetAttributeValue block_insert_obj "ПІКЕТ" piket_str)
                        )
                        (princ (strcat "\n*** Помилка: vla-InsertBlock повернув nil для " piket_str))
                    )
                )
             )
             (princ (strcat "\n*** Попередження: Не вдалося отримати дотичну на відстані " (rtos dist_on_pline) ". Пропуск 100м пікету."))
          )
      )
      (progn
         (if (< current_picket_val (- 0.0 fuzz))
             (princ (strcat "\n--- Пропуск 100м пікету " (rtos current_picket_val 2 1) " (від'ємне значення).")))
         (if (or (<= dist_on_pline fuzz) (>= dist_on_pline (- pline_len fuzz)))
             (princ (strcat "\n--- Пропуск 100м пікету " (rtos current_picket_val 2 1) " (збігається або близько до початку/кінця).")))
      )
    )
    (setq current_picket_val (+ current_picket_val (* dir_factor 100.0)))
  )

  ;; --- Маркер в КІНЦІ полілінії ---
  (if (= dir_factor 1.0) (setq picket_at_end (+ picket_at_start pline_len)) (setq picket_at_end (- picket_at_start pline_len)))
  (princ (strcat "\nРозрахункове значення пікету в кінці: " (rtos picket_at_end 2 4) " м."))
  (if (>= picket_at_end (- 0.0 fuzz))
      (progn
        (princ "\nСпроба поставити маркер в кінці полілінії...")
        (setq pt_end (vlax-curve-getEndPoint pline_obj))
        (setq vec_tangent_end (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtDist pline_len))) ; Використовуємо pline_len
        (if (not vec_tangent_end) (setq vec_tangent_end (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getEndParam pline_obj))))
        (if vec_tangent_end
            (progn
              (setq vec_perp (list (- (cadr vec_tangent_end)) (car vec_tangent_end) 0.0))
              (setq vec_perp_final (if (= side_factor 1.0) vec_perp (mapcar '- vec_perp)))
              (setq block_angle_perp (angle '(0.0 0.0 0.0) vec_perp_final))
              (setq block_angle (- block_angle_perp (/ pi 2.0)))
              (setq block_angle (rem block_angle (* 2.0 pi)))
              (if (< block_angle 0.0) (setq block_angle (+ block_angle (* 2.0 pi))))
              (setq piket_str_end (FormatPicketValue picket_at_end))
              (setq block_insert_obj (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point pt_end) block_name_selected 1.0 1.0 1.0 block_angle)))
              (if (vl-catch-all-error-p block_insert_obj)
                  (princ (strcat "\n*** Помилка вставки блоку в кінці: " (vl-catch-all-error-message block_insert_obj)))
                  (if block_insert_obj
                      (progn
                        (princ (strcat "\n Вставлено блок для: " piket_str_end))
                        (SetAttributeValue block_insert_obj "ПІКЕТ" piket_str_end)
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

  ;; --- Збереження XDATA на полілінії ---
  (if (and pline_obj picket_at_start dir_factor)
      (progn
        (princ (strcat "\nЗберігаємо XDATA на полілінії '" (vla-get-Handle pline_obj) "' під AppID '" app_id_name "'..."))
        (vl-catch-all-apply
          '(lambda ()
             (setq ent_data (entget (vlax-vla-object->ename pline_obj) (list app_id_name)))
             ;; Видаляємо старі XDATA, якщо вони існують, щоб уникнути дублювання
             (if (assoc -3 ent_data)
                 (setq ent_data (vl-remove-if '(lambda (x) (and (listp x) (= (car x) -3) (equal (cadr x) (list app_id_name)))) ent_data))
             )
             (setq xdata_list (list (cons -3 (list app_id_name))
                                    (cons 1000 (rtos picket_at_start 2 8)) ; Код 1000 для рядка
                                    (cons 1000 (rtos dir_factor 2 8))
                                    (cons -3 (list app_id_name)) ; Закриваємо список XDATA
                             ))
             (setq new_ent_data (append ent_data xdata_list)) ; Додаємо до існуючих даних
             (entmod new_ent_data)
             (princ "\nXDATA успішно збережено на полілінії.")
           )
          (vl-catch-all-error-message)
        )
      )
      (princ "\n*** Помилка: Неможливо зберегти XDATA - відсутні валідні дані або об'єкт полілінії.")
  )

  ;; --- Завершення ---
  (command "_REGEN")
  (princ "\nРозстановка пікетажу (блоками) завершена.")
  (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars)) ; Відновлюємо всі змінні
  (setq *error* nil)
  (princ)
)

;; Повідомлення про завантаження
(princ "\nСкрипт для розстановки пікетажу БЛОКАМИ завантажено. Введіть 'CREATE_PICKET_MARKER' для запуску.")
(princ)