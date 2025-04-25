;;; Скрипт для розстановки пікетажу вздовж полілінії AutoCAD (LWPOLYLINE)
;;; Версія v2025-04-25_StartEndPickets (Додано маркери на початку/кінці)
;;; Розставляє перпендикулярні елементи кожні 100м, а також на початку/кінці
;;; полілінії (якщо пікет >= 0). Використовує FIX замість floor/ceiling.

;; === Допоміжні функції для векторної математики ===

(defun normalize (v / len) (setq len (distance '(0 0 0) v)) (if (< len 1e-12) nil (mapcar '(lambda (x) (/ x len)) v)))
(defun v_scale (v s) (mapcar '(lambda (x) (* x s)) v))
(defun v_add (v1 v2) (mapcar '+ v1 v2))
(defun v_sub (v1 v2) (mapcar '- v1 v2))

;; === Допоміжна функція для форматування значення пікету ===
;; Приймає числове значення, повертає рядок "ПКX+YY.YY"
(defun FormatPicketValue (p_val / pk_km pk_m val_str km_str fuzz)
  (setq fuzz 1e-9) ; Локальний допуск
  ;; Отримуємо цілу частину (сотні метрів)
  (setq pk_km (fix (/ p_val 100.0)))
  ;; Обережно розраховуємо залишок (метри)
  (setq pk_m (abs (- p_val (* (float pk_km) 100.0))))
  ;; Обробка випадків, коли залишок дуже близький до 100 (напр., 99.999...)
  (if (> pk_m (- 100.0 fuzz))
      (progn
         (setq pk_m 0.0)
         ;; Коригуємо цілу частину
         (if (>= p_val 0.0) (setq pk_km (1+ pk_km)) (setq pk_km (1- pk_km)))
      )
  )
  (setq km_str (itoa pk_km))
  (setq val_str (rtos pk_m 2 2)) ; Форматуємо метри з 2 знаками після коми
  ;; Додаємо ведучий нуль для метрів < 10 (напр., ПК5+07.50)
  (if (and (< pk_m (- 10.0 fuzz)) (> pk_m (- 0.0 fuzz)))
      (setq val_str (strcat "0" val_str))
  )
  (strcat "ПК" km_str "+" val_str) ; Збираємо фінальний рядок
)

;; === Допоміжна функція для створення елементів маркера ===
;; Створює лінію, Т-засічку та текст у заданій точці
(defun PlaceMarkerElements (pt_center vec_tangent side_factor piket_str final_stylename txt_h target_layer len_main len_tcap /
                           vec_perp vec_perp_final vec_perp_norm vec_tangent_norm half_main half_tcap
                           pt_end_far pt_end_near pt_tcap_start pt_tcap_end pt_text angle_text angle_text_raw
                           line1_ent line2_ent text_ent fuzz) ; <<< ДОДАНО 'fuzz' ДО ЛОКАЛЬНИХ ЗМІННИХ

  (setq fuzz 1e-9) ; Локальний допуск
  (setq half_main (/ len_main 2.0) half_tcap (/ len_tcap 2.0))

  ;; Перевірка валідності дотичної перед нормалізацією
  (if (or (not vec_tangent) (< (distance '(0 0 0) vec_tangent) fuzz))
      (progn (princ (strcat "\n*** Попередження: Недійсна дотична для точки: " (vl-princ-to-string pt_center) ". Маркер не створено.")) nil)
      ;; Якщо дотична валідна, продовжуємо
      (progn
        ;; Розрахунок геометрії
        (setq vec_perp (list (- (cadr vec_tangent)) (car vec_tangent) 0.0))
        (setq vec_perp_final (if (= side_factor 1.0) vec_perp (mapcar '- vec_perp)))
        (setq vec_perp_norm (normalize vec_perp_final))
        (setq vec_tangent_norm (normalize vec_tangent))

        (if (and vec_perp_norm vec_tangent_norm)
            (progn
              ;; --- 1. Створення основної лінії ---
              (setq pt_end_far (v_sub pt_center (v_scale vec_perp_norm half_main)))
              (setq pt_end_near (v_add pt_center (v_scale vec_perp_norm half_main)))
              (entmake (list '(0 . "LINE") (cons 8 target_layer) (cons 10 pt_end_far) (cons 11 pt_end_near)))

              ;; --- 2. Створення Т-засічки ---
              (setq pt_tcap_start (v_sub pt_end_near (v_scale vec_tangent_norm half_tcap)))
              (setq pt_tcap_end (v_add pt_end_near (v_scale vec_tangent_norm half_tcap)))
              (entmake (list '(0 . "LINE") (cons 8 target_layer) (cons 10 pt_tcap_start) (cons 11 pt_tcap_end)))

              ;; --- 3. Створення Тексту ---
              (if final_stylename
                  (progn
                    (setq pt_text (v_add pt_end_near (v_scale vec_perp_norm (* txt_h 0.75))))
                    (setq angle_text_raw (angle '(0 0 0) vec_tangent_norm))
                    (if (and (> angle_text_raw (/ pi 2.0)) (< angle_text_raw (* 1.5 pi)))
                        (setq angle_text (+ angle_text_raw pi))
                        (setq angle_text angle_text_raw)
                    )
                    (if (>= angle_text (* 2.0 pi)) (setq angle_text (- angle_text (* 2.0 pi))))
                    (entmake (list '(0 . "TEXT") (cons 8 target_layer) (cons 10 pt_text) (cons 40 txt_h)
                                   (cons 1 piket_str) (cons 50 angle_text) (cons 7 final_stylename)
                                   (cons 72 4) (cons 73 2) (cons 11 pt_text)))
                  )
              ) ; if final_stylename
              (princ (strcat "\n Створено маркер для: " piket_str " в точці " (vl-princ-to-string pt_center)))
              T ; Успіх
            )
            (progn
              (princ (strcat "\n*** Попередження: Не вдалося нормалізувати вектор(и) для точки: " (vl-princ-to-string pt_center) ". Маркер не створено."))
              nil ; Невдача
            )
        ) ; if vectors normalized
      ) ; progn if tangent valid
  ) ; if tangent valid check
)

;; === Головна функція ===
(defun C:CREATE_PICKETMARKER (/ *error* old_vars pline_ent pline_obj pt_ref pt_ref_on_pline dist_ref_on_pline
                             val_ref pt_dir vec_dir vec_tangent_ref dir_factor pt_side_ref vec_side_ref
                             vec_perp_ref dot_prod_side side_factor picket_at_start pline_len picket_at_end ; Додано picket_at_end
                             first_picket_val last_picket_val current_picket_val dist_on_pline
                             pt_on_pline vec_tangent vec_perp vec_perp_final vec_perp_norm vec_tangent_norm
                             target_layer line_len_main line_len_tcap half_line_main half_line_tcap
                             fuzz pt_end_far pt_end_near pt_tcap_start pt_tcap_end pt_text angle_text angle_text_raw
                             piket_str piket_str_start piket_str_end text_style text_height final_stylename ; Додано piket_str_start/end
                             line1_ent line2_ent text_ent pt_start pt_end vec_tangent_start vec_tangent_end) ; Додано змінні для поч/кінця

  (princ "\n*** Running CREATE_PICKETMARKER v2025-04-25_StartEndPickets  ***") ; <<< Оновлено версію

  ;; Налаштування констант
  (setq target_layer   "0"
        line_len_main  38.0
        line_len_tcap  5.0
        text_style     "Д-431"
        text_height    1.8
        fuzz           1e-9
  )
  (setq half_line_main (/ line_len_main 2.0))
  (setq half_line_tcap (/ line_len_tcap 2.0))
  (setq final_stylename text_style)

  ;; Перевизначення обробника помилок
  (defun *error* (msg)
    (if old_vars (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars)))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "Відміна користувачем")))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ)
  )

  ;; Збереження системних змінних
  (setq old_vars (mapcar '(lambda (v) (cons v (getvar v))) '("CMDECHO" "OSMODE" "CLAYER")))
  (setvar "CMDECHO" 0)

  ;; --- Збір вхідних даних ---
  (princ "\nРозстановка пікетажу (з маркерами на початку/кінці).")
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
  (setq pt_side_ref (getpoint pt_ref_on_pline "\nВкажіть сторону для розміщення 'T'-засічки та тексту: "))
  (if (not pt_side_ref) (*error* "Відміна користувачем"))

  ;; Перевірка валідності
  (if (not (and pt_ref_on_pline (= 'LIST (type pt_ref_on_pline)) (= 3 (length pt_ref_on_pline)))) (*error* "Reference point on polyline invalid"))
  (if (not (and pt_dir (= 'LIST (type pt_dir)) (= 3 (length pt_dir)))) (*error* "Direction point invalid"))
  (if (not (and pt_side_ref (= 'LIST (type pt_side_ref)) (= 3 (length pt_side_ref)))) (*error* "Side point invalid"))

  ;; --- Перевірка стилю тексту ---
  (if (not (tblsearch "STYLE" final_stylename))
    (progn
      (princ (strcat "\n*** Попередження: Текстовий стиль '" final_stylename "' не знайдено."))
      (setq final_stylename (getvar "TEXTSTYLE"))
      (princ (strcat " Спроба використати поточний стиль '" final_stylename "'."))
      (if (not (tblsearch "STYLE" final_stylename))
          (progn
             (princ (strcat "\n*** Попередження: Поточний стиль '" final_stylename "' також не знайдено!"))
             (setq final_stylename "Standard")
             (princ (strcat " Спроба використати стиль 'Standard'."))
             (if (not (tblsearch "STYLE" final_stylename))
                 (progn (princ (strcat "\n*** Помилка: Стиль 'Standard' не знайдено! Текст не буде створено.")) (setq final_stylename nil))
                 (princ (strcat "\n Використовується стиль '" final_stylename "'."))
             )
          )
          (princ (strcat "\n Використовується стиль '" final_stylename "'."))
      )
    )
    (princ (strcat "\n Використовується стиль тексту '" final_stylename "'."))
  )

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
  (princ (strcat "\nРозрахований діапазон для 100м пікетів (з FIX): " (rtos first_picket_val) " до " (rtos last_picket_val)))

  ;; --- Підготовка до розстановки ---
  (setvar "CLAYER" target_layer) ; Встановлюємо поточний шар "0"

  ;; --- Маркер на ПОЧАТКУ полілінії (якщо пікет >= 0) ---
  (if (>= picket_at_start (- 0.0 fuzz))
      (progn
        (princ "\nСпроба поставити маркер на початку полілінії...")
        (setq pt_start (vlax-curve-getStartPoint pline_obj))
        (setq vec_tangent_start (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getStartParam pline_obj)))
        (if vec_tangent_start
            (progn
              (setq piket_str_start (FormatPicketValue picket_at_start))
              (PlaceMarkerElements pt_start vec_tangent_start side_factor piket_str_start final_stylename text_height target_layer line_len_main line_len_tcap)
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

    ;; Перевірка, чи точка в межах полілінії ТА чи значення пікету не від'ємне
    ;; Додаємо перевірку, щоб не дублювати початкову/кінцеву точки, якщо вони збігаються з 100м
    (if (and (>= current_picket_val (- 0.0 fuzz))
             (> dist_on_pline fuzz) ; Не ставити, якщо дуже близько до початку
             (< dist_on_pline (- pline_len fuzz)) ; Не ставити, якщо дуже близько до кінця
             (>= dist_on_pline (- 0.0 fuzz)) ; (Ця перевірка вже є, але для ясності)
             (<= dist_on_pline (+ pline_len fuzz)))
      (progn
        (setq pt_on_pline (vlax-curve-getPointAtDist pline_obj dist_on_pline))
        (setq vec_tangent (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtDist pline_obj dist_on_pline)))
        (if (and pt_on_pline vec_tangent (< (distance '(0 0 0) vec_tangent) fuzz))
            (princ (strcat "\n*** Попередження: Нульовий вектор дотичної на відстані " (rtos dist_on_pline) ". Пропуск 100м пікету."))
            (if (and pt_on_pline vec_tangent)
              (progn
                ;; Створення елементів (код дублюється, але можна замінити на PlaceMarkerElements якщо потрібно)
                (setq vec_perp (list (- (cadr vec_tangent)) (car vec_tangent) 0.0))
                (setq vec_perp_final (if (= side_factor 1.0) vec_perp (mapcar '- vec_perp)))
                (setq vec_perp_norm (normalize vec_perp_final))
                (setq vec_tangent_norm (normalize vec_tangent))
                (if (and vec_perp_norm vec_tangent_norm)
                    (progn
                      (setq pt_end_far (v_sub pt_on_pline (v_scale vec_perp_norm half_line_main)))
                      (setq pt_end_near (v_add pt_on_pline (v_scale vec_perp_norm half_line_main)))
                      (entmake (list '(0 . "LINE") (cons 8 target_layer) (cons 10 pt_end_far) (cons 11 pt_end_near)))
                      (setq pt_tcap_start (v_sub pt_end_near (v_scale vec_tangent_norm half_line_tcap)))
                      (setq pt_tcap_end (v_add pt_end_near (v_scale vec_tangent_norm half_line_tcap)))
                      (entmake (list '(0 . "LINE") (cons 8 target_layer) (cons 10 pt_tcap_start) (cons 11 pt_tcap_end)))
                      (if final_stylename
                          (progn
                            (setq piket_str (strcat "ПК" (itoa (fix (+ (/ current_picket_val 100.0) fuzz)))))
                            (setq pt_text (v_add pt_end_near (v_scale vec_perp_norm (* text_height 0.75))))
                            (setq angle_text_raw (angle '(0 0 0) vec_tangent_norm))
                            (if (and (> angle_text_raw (/ pi 2.0)) (< angle_text_raw (* 1.5 pi))) (setq angle_text (+ angle_text_raw pi)) (setq angle_text angle_text_raw))
                            (if (>= angle_text (* 2.0 pi)) (setq angle_text (- angle_text (* 2.0 pi))))
                            (entmake (list '(0 . "TEXT") (cons 8 target_layer) (cons 10 pt_text) (cons 40 text_height) (cons 1 piket_str) (cons 50 angle_text) (cons 7 final_stylename) (cons 72 4) (cons 73 2) (cons 11 pt_text)))
                            (princ (strcat "\n Створено 100м маркер: " piket_str " на відстані " (rtos dist_on_pline 2 2)))
                          )
                      )
                    )
                    (princ (strcat "\n*** Попередження: Не вдалося нормалізувати вектор(и) на відстані " (rtos dist_on_pline) ". Пропуск 100м пікету."))
                )
              )
            )
        )
      )
      ;; --- Якщо умови НЕ виконано (пікет від'ємний або близько до кінців) ---
      (if (< current_picket_val (- 0.0 fuzz))
        (princ (strcat "\n--- Пропуск 100м пікету " (rtos current_picket_val 2 1) " (від'ємне значення).")))
      (if (or (<= dist_on_pline fuzz) (>= dist_on_pline (- pline_len fuzz)))
         (princ (strcat "\n--- Пропуск 100м пікету " (rtos current_picket_val 2 1) " (збігається або близько до початку/кінця).")))

    ) ; if conditions met

    ;; Перехід до наступного пікету
    (setq current_picket_val (+ current_picket_val (* dir_factor 100.0)))
  ) ; while loop

  ;; --- Маркер в КІНЦІ полілінії (якщо пікет >= 0) ---
  ;; Розрахунок пікету в кінці
  (if (= dir_factor 1.0)
      (setq picket_at_end (+ picket_at_start pline_len))
      (setq picket_at_end (- picket_at_start pline_len))
  )
  (princ (strcat "\nРозрахункове значення пікету в кінці: " (rtos picket_at_end 2 4) " м."))

  (if (>= picket_at_end (- 0.0 fuzz))
      (progn
        (princ "\nСпроба поставити маркер в кінці полілінії...")
        (setq pt_end (vlax-curve-getEndPoint pline_obj))
        ;; Спробуємо отримати дотичну трохи раніше кінця для надійності
        (setq vec_tangent_end (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getParamAtDist pline_obj (- pline_len fuzz))))
        ;; Якщо не вийшло, спробуємо точно в кінці
        (if (not vec_tangent_end) (setq vec_tangent_end (vlax-curve-getFirstDeriv pline_obj (vlax-curve-getEndParam pline_obj))))

        (if vec_tangent_end
            (progn
              (setq piket_str_end (FormatPicketValue picket_at_end))
              (PlaceMarkerElements pt_end vec_tangent_end side_factor piket_str_end final_stylename text_height target_layer line_len_main line_len_tcap)
            )
            (princ (strcat "\n*** Попередження: Не вдалося отримати дотичну в кінцевій точці. Маркер не створено."))
        )
      )
      (princ (strcat "\n--- Пропуск маркера в кінці полілінії (Пікет=" (rtos picket_at_end 2 2) " < 0)."))
  )

  ;; --- Завершення ---
  (command "_REGEN")
  (princ "\nРозстановка пікетажу завершена.")
  (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars))
  (setq *error* nil)
  (princ)
)

;; Повідомлення про завантаження
(princ "\nСкрипт для розстановки пікетажу (з поч/кінц маркерами) завантажено. Введіть 'CREATE_PICKETMARKER' v14 для запуску.")
(princ)