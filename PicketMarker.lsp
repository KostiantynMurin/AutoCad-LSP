;;; Скрипт для розстановки пікетажу вздовж полілінії AutoCAD (LWPOLYLINE)
;;; Версія v2025-04-24_SimpleLine (Вставка простих відрізків замість блоків)
;;; Розставляє перпендикулярні відрізки довжиною 38 од. кожні 100м
;;; з урахуванням точки прив'язки, значення пікету, напрямку та сторони розміщення.

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

;; === Головна функція ===
(defun C:CREATE_PICKETMARKER (/ *error* old_vars pline_ent pline_obj pt_ref pt_ref_on_pline dist_ref_on_pline
                             val_ref pt_dir vec_dir vec_tangent_ref dir_factor pt_side_ref vec_side_ref
                             vec_perp_ref dot_prod_side side_factor picket_at_start pline_len
                             first_picket_val last_picket_val current_picket_val dist_on_pline
                             pt_on_pline vec_tangent vec_perp vec_perp_final vec_perp_norm
                             target_layer line_len half_line
                             fuzz pt_start pt_end line_ent) ; Змінено список змінних

  (princ "\n*** Running CREATE_PICKETMARKER v2025-04-24_SimpleLine ***") ; <<< Оновлено версію

  ;; Налаштування констант
  (setq target_layer "0"         ; Шар для вставки відрізків
        line_len     38.0        ; Довжина перпендикулярного відрізка
        fuzz         1e-9         ; Допуск для порівняння дійсних чисел
  )
  (setq half_line (/ line_len 2.0)) ; Розрахунок половини довжини

  ;; Перевизначення обробника помилок
  (defun *error* (msg)
    (if old_vars (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars)))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "Відміна користувачем")))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ)
  )

  ;; Збереження системних змінних
  (setq old_vars (mapcar '(lambda (v) (cons v (getvar v))) '("CMDECHO" "OSMODE" "CLAYER"))) ; ATTREQ/ATTDIA не потрібні
  (setvar "CMDECHO" 0)

  ;; --- Збір вхідних даних ---
  (princ "\nРозстановка пікетажу вздовж полілінії (простими лініями).")
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
  ;; Визначення сторони більше не потрібне для простої лінії, але залишимо логіку визначення напрямку
  ;; (setq pt_side_ref (getpoint pt_ref_on_pline "\nВкажіть сторону (не впливає на лінію): "))
  ;; (if (not pt_side_ref) (*error* "Відміна користувачем"))

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
  ;; Визначення фактору сторони більше не потрібне для симетричної лінії
  ;; (setq vec_side_ref (mapcar '- (trans pt_side_ref 1 0) pt_ref_on_pline))
  ;; (setq vec_perp_ref (list (- (cadr vec_tangent_ref)) (car vec_tangent_ref) 0.0))
  ;; (setq dot_prod_side (apply '+ (mapcar '* vec_side_ref vec_perp_ref)))
  ;; (setq side_factor (if (< dot_prod_side 0.0) -1.0 1.0))
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

  ;; --- Цикл розстановки відрізків ---
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
                ;; Розрахунок перпендикуляру
                (setq vec_perp (list (- (cadr vec_tangent)) (car vec_tangent) 0.0))
                ;; Нормалізація перпендикуляру
                (setq vec_perp_norm (normalize vec_perp))

                (if vec_perp_norm ; Продовжуємо тільки якщо нормалізація успішна
                    (progn
                      ;; Розрахунок кінцевих точок відрізка
                      (setq pt_start (v_sub pt_on_pline (v_scale vec_perp_norm half_line)))
                      (setq pt_end (v_add pt_on_pline (v_scale vec_perp_norm half_line)))

                      ;; Створення відрізка за допомогою entmake
                      (setq line_ent (entmake (list '(0 . "LINE")
                                                    (cons 8 target_layer) ; Явно вказуємо шар "0"
                                                    (cons 10 pt_start)
                                                    (cons 11 pt_end)
                                              )
                                     )
                      )
                      (if line_ent
                          (princ (strcat "\n Створено відрізок на пікеті ~" (rtos (/ current_picket_val 100.0) 2 1) " на відстані " (rtos dist_on_pline 2 2)))
                          (princ (strcat "\n *** Помилка створення відрізка на пікеті ~" (rtos (/ current_picket_val 100.0) 2 1)))
                      )
                    )
                    (princ (strcat "\n*** Попередження: Не вдалося нормалізувати перпендикуляр на відстані " (rtos dist_on_pline) ". Пропуск пікету."))
                )
              )
            )
        )
      )
    )
    (setq current_picket_val (+ current_picket_val (* dir_factor 100.0)))
  ) ; while loop

  ;; --- Завершення ---
  (command "_REGEN")
  (princ "\nРозстановка пікетажу (лініями) завершена.")
  (mapcar 'setvar (mapcar 'car old_vars) (mapcar 'cdr old_vars))
  (setq *error* nil)
  (princ)
)

;; Повідомлення про завантаження
(princ "\nСкрипт для розстановки пікетажу ЛІНІЯМИ завантажено. Введіть 'CREATE_PICKETMARKER' без блоків для запуску.")
(princ)