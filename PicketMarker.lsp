;;; Скрипт для розстановки пікетажу вздовж полілінії AutoCAD (LWPOLYLINE)
;;; Версія v2025-04-24_LineTeeText (Вставка лінії, Т-засічки та тексту)
;;; Розставляє перпендикулярні елементи кожні 100м з урахуванням точки прив'язки,
;;; значення пікету, напрямку та сторони розміщення. ВИКОРИСТОВУЄ FIX замість floor/ceiling.

;; === Допоміжні функції для векторної математики ===

;; Нормалізація вектора (отримання одиничного вектора)
(defun normalize (v / len)
  (setq len (distance '(0 0 0) v))
  (if (< len 1e-12) ; Уникнення ділення на нуль для нульових векторів
      nil ; Повернути nil якщо вектор нульовий
      (mapcar '(lambda (x) (/ x len)) v)
  )
)

;; Множення вектора на скаляр
(defun v_scale (v s)
  (mapcar '(lambda (x) (* x s)) v)
)

;; Додавання векторів (або точки та вектора)
(defun v_add (v1 v2)
  (mapcar '+ v1 v2)
)

;; Віднімання векторів (або точки та вектора)
(defun v_sub (v1 v2)
  (mapcar '- v1 v2)
)

;; Головна функція (виправлено позицію та кут тексту)
(defun C:CREATE_PICKETMARKER (/ *error* old_vars pline_ent pline_obj pt_ref pt_ref_on_pline dist_ref_on_pline
                             val_ref pt_dir vec_dir vec_tangent_ref dir_factor pt_side_ref vec_side_ref
                             vec_perp_ref dot_prod_side side_factor picket_at_start pline_len
                             first_picket_val last_picket_val current_picket_val dist_on_pline
                             pt_on_pline vec_tangent vec_perp vec_perp_final vec_perp_norm vec_tangent_norm
                             target_layer line_len_main line_len_tcap half_line_main half_line_tcap
                             fuzz pt_end_far pt_end_near pt_tcap_start pt_tcap_end pt_text angle_text angle_text_raw ; Додано angle_text_raw
                             piket_str text_style text_height final_stylename line1_ent line2_ent text_ent)

  (princ "\n*** Running CREATE_PICKETMARKER v2025-04-24_LineTeeText_FixPosRot ***") ; <<< Оновлено версію

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
  (princ "\nРозстановка пікетажу (Лінія+Засічка+Текст).")
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
  (princ (strcat "\nШукаємо пікети від " (rtos (min first_picket_val last_picket_val) 2 1) " до " (rtos (max first_picket_val last_picket_val) 2 1)))
  (setvar "CLAYER" target_layer) ; Встановлюємо поточний шар "0"

  ;; --- Цикл розстановки елементів ---
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
                ;; Розрахунок перпендикуляру та нормалізація
                (setq vec_perp (list (- (cadr vec_tangent)) (car vec_tangent) 0.0))
                (setq vec_perp_final (if (= side_factor 1.0) vec_perp (mapcar '- vec_perp)))
                (setq vec_perp_norm (normalize vec_perp_final))

                ;; Нормалізація тангенти
                (setq vec_tangent_norm (normalize vec_tangent))

                (if (and vec_perp_norm vec_tangent_norm)
                    (progn
                      ;; --- 1. Створення основного відрізка (38 од.) ---
                      (setq pt_end_far (v_sub pt_on_pline (v_scale vec_perp_norm half_line_main)))
                      (setq pt_end_near (v_add pt_on_pline (v_scale vec_perp_norm half_line_main)))
                      (setq line1_ent (entmake (list '(0 . "LINE") (cons 8 target_layer) (cons 10 pt_end_far) (cons 11 pt_end_near))))
                      (if (not line1_ent) (princ (strcat "\n *** Помилка створення основного відрізка на пікеті ~" (rtos (/ current_picket_val 100.0) 2 1))))

                      ;; --- 2. Створення "Т"-засічки (5 од.) ---
                      (setq pt_tcap_start (v_sub pt_end_near (v_scale vec_tangent_norm half_line_tcap)))
                      (setq pt_tcap_end (v_add pt_end_near (v_scale vec_tangent_norm half_line_tcap)))
                      (setq line2_ent (entmake (list '(0 . "LINE") (cons 8 target_layer) (cons 10 pt_tcap_start) (cons 11 pt_tcap_end))))
                      (if (not line2_ent) (princ (strcat "\n *** Помилка створення Т-засічки на пікеті ~" (rtos (/ current_picket_val 100.0) 2 1))))

                      ;; --- 3. Створення Тексту ---
                      (if final_stylename
                          (progn
                            (setq piket_str (strcat "ПК" (itoa (fix (+ (/ current_picket_val 100.0) fuzz)))))
                            ;; Точка вставки тексту (зменшений відступ)
                            (setq pt_text (v_add pt_end_near (v_scale vec_perp_norm (* text_height 0.75)))) ; <--- ЗМІНЕНО ВІДСТУП
                            ;; Розрахунок кута тексту з корекцією на читабельність
                            (setq angle_text_raw (angle '(0 0 0) vec_tangent_norm))
                            (if (and (> angle_text_raw (/ pi 2.0)) (< angle_text_raw (* 1.5 pi))) ; <--- ДОДАНО ПЕРЕВІРКУ КУТА
                                (setq angle_text (+ angle_text_raw pi))
                                (setq angle_text angle_text_raw)
                            )
                            (if (>= angle_text (* 2.0 pi)) (setq angle_text (- angle_text (* 2.0 pi)))) ; Нормалізація 0-2PI

                            (setq text_ent (entmake (list '(0 . "TEXT")
                                                          (cons 8 target_layer)
                                                          (cons 10 pt_text)
                                                          (cons 40 text_height)
                                                          (cons 1 piket_str)
                                                          (cons 50 angle_text)   ; <--- ВИКОРИСТАННЯ СКОРИГОВАНОГО КУТА
                                                          (cons 7 final_stylename)
                                                          (cons 72 4) ; Гор: Middle
                                                          (cons 73 2) ; Верт: Middle
                                                          (cons 11 pt_text)
                                                    )
                                          )
                            )
                            (if text_ent
                                (princ (strcat "\n Створено елементи для пікету " piket_str " на відстані " (rtos dist_on_pline 2 2)))
                                (princ (strcat "\n *** Помилка створення тексту на пікеті ~" (rtos (/ current_picket_val 100.0) 2 1)))
                            )
                          )
                          (princ (strcat "\n*** Пропуск створення тексту на пікеті ~" (rtos (/ current_picket_val 100.0) 2 1) " через відсутність стилю.")) ; Повідомлення, якщо стиль не знайдено
                      ) ; if final_stylename
                    ) ; progn if normalization successful
                    (princ (strcat "\n*** Попередження: Не вдалося нормалізувати вектор(и) на відстані " (rtos dist_on_pline) ". Пропуск пікету."))
                )
              ) ; progn if pt_on_pline and vec_tangent valid
            ) ; endif tangent non-zero check
        ) ; endif pt_on_pline and vec_tangent exist check
      ) ; progn if point on polyline
    ) ; if point on polyline

    ;; Перехід до наступного пікету
    (setq current_picket_val (+ current_picket_val (* dir_factor 100.0)))
  ) ; while loop

  ;; --- Завершення ---
  (command "_REGEN")
  (princ "\nРозстановка пікетажу (Лінія+Засічка+Текст) завершена.")
  (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars))
  (setq *error* nil)
  (princ)
)
;; Повідомлення про завантаження
(princ "\nСкрипт для розстановки пікетажу (Лінія+Засічка+Текст) завантажено. Введіть 'CREATE_PICKETMARKER' v12 для запуску.")
(princ)