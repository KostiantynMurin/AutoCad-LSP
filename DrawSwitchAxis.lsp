(defun c:DrawSwitchAxis ( / p1_ent p4_ent p3_ent p5_ent p1_coords p4_coords p3_coords p5_coords
                                 line_straight_obj proj_pt csp_pt branch_angle branch_end_pt )
  (vl-load-com) ; Завантажуємо ActiveX-функції для роботи з об'єктами
  (princ "\n--- Побудова осі стрілочного переводу (1/11) за блоками ---")

  ;; Допоміжна функція для вибору блоку та отримання його точки вставки
  ;; Повертає список координат точки вставки блоку або nil у випадку помилки
  (defun GetBlockInsertionPoint (prompt_msg / ent_data ent_type insertion_pt)
    (setq ent_data (entsel prompt_msg)) ; Користувач обирає об'єкт
    (if ent_data
      (progn
        (setq ent_name (car ent_data))
        (setq ent_list (entget ent_name)) ; Отримуємо DXF-список об'єкта
        (setq ent_type (cdr (assoc 0 ent_list))) ; Отримуємо тип об'єкта

        (if (equal ent_type "INSERT") ; Перевіряємо, чи це блок (INSERT)
          (progn
            (setq insertion_pt (cdr (assoc 10 ent_list))) ; Отримуємо точку вставки (DXF-код 10)
            (princ (strcat "\nОбрано блок. Точка: " (vl-princ-to-string insertion_pt)))
            insertion_pt
          )
          (progn
            (princ "\nПомилка: Вибраний об'єкт не є блоком. Спробуйте ще раз.")
            nil ; Повертаємо nil, якщо це не блок
          )
        )
      )
      (progn
        (princ "\nВідміна вибору. Операцію скасовано.")
        nil ; Повертаємо nil, якщо вибір скасовано
      )
    )
  )

  ;; 1. Запит блоків у користувача та отримання їхніх координат
  (setq p1_coords (GetBlockInsertionPoint "\nВиберіть блок для точки стику рамної рейки (P1): "))
  (if (not p1_coords) (progn (princ "\nОперацію скасовано.") (exit)))

  (setq p4_coords (GetBlockInsertionPoint "\nВиберіть блок для точки хвоста хрестовини по прямому напрямку (P4): "))
  (if (not p4_coords) (progn (princ "\nОперацію скасовано.") (exit)))

  (setq p3_coords (GetBlockInsertionPoint "\nВиберіть блок для точки центру хрестовини (P3): "))
  (if (not p3_coords) (progn (princ "\nОперацію скасовано.") (exit)))

  (setq p5_coords (GetBlockInsertionPoint "\nВиберіть блок для точки хвоста хрестовини по відгалуженню (P5): "))
  (if (not p5_coords) (progn (princ "\nОперацію скасовано.") (exit)))

  ;; --- Починаємо побудову, використовуючи отримані координати ---

  ;; 2. Побудова прямої лінії між P1 і P4
  (command "_.LINE" p1_coords p4_coords "") ; Креслимо пряму вісь P1-P4
  (setq line_straight_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт тільки що створеної лінії

  ;; 3. Від точки центру хрестовини (P3) провести перпендикуляр до прямої лінії P1-P4
  ;;    І знайти точку перетину (проекція P3)
  (setq proj_pt (vlax-curve-getClosestPointTo line_straight_obj p3_coords))

  (command "_.POINT" proj_pt) ; Для візуалізації: ставимо точку проекції

  ;; 4. Від отриманої точки (proj_pt) знайти Центр Стрілочного Переводу (ЦСП)
  ;;    Відкласти 16.72 в сторону P1 від proj_pt вздовж прямої P1-P4
  (setq dist_to_csp 16.72)
  (setq vec_p1_proj (mapcar '- p1_coords proj_pt)) ; Вектор від proj_pt до P1
  (setq vec_p1_proj_unit (unit_vector vec_p1_proj)) ; Одиничний вектор
  (setq csp_pt (mapcar '+ proj_pt (mapcar '* vec_p1_proj_unit (list dist_to_csp dist_to_csp 0.0))))

  (command "_.POINT" csp_pt) ; Для візуалізації: ставимо точку ЦСП

  ;; 5. Визначення напрямку відгалуження (вліво/вправо)
  (setq vec_line (mapcar '- p4_coords p1_coords)) ; Вектор прямої P1-P4
  (setq vec_test (mapcar '- p5_coords p1_coords)) ; Вектор P1-P5
  (setq cross_z (caddr (cross_product vec_line vec_test)))

  (setq is_left nil)
  (if (> cross_z 0)
    (setq is_left T)
    (setq is_left nil)
  )

  ;; 6. Побудова осі відгалуження від ЦСП
  (setq branch_length 20.0)
  (setq branch_angle_deg 5.194444444) ; 5d11'40" в десяткових градусах
  (setq branch_angle_rad (dtr branch_angle_deg)) ; Переводимо в радіани

  (setq straight_line_angle (angle p1_coords p4_coords)) ; Кут прямої лінії P1-P4 в радіанах

  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad)) ; Додаємо кут
    (setq final_branch_angle (- straight_line_angle branch_angle_rad)) ; Віднімаємо кут
  )

  (setq branch_end_pt (polar csp_pt final_branch_angle branch_length))

  (command "_.LINE" csp_pt branch_end_pt "") ; Креслимо вісь відгалуження

  (princ "\n--- Осі стрілочного переводу побудовано! ---")
  (princ)
)

;; Допоміжні функції (потрібні для LISP)
(defun dtr (a) (* pi (/ a 180.0)))
(defun rtd (a) (* 180.0 (/ a pi)))

(defun unit_vector (vec)
  (setq len (distance '(0 0 0) vec))
  (if (and (numberp len) (> len 0.00000001))
    (mapcar '/ vec (list len len (if (caddr vec) len 1.0)))
    '(0.0 0.0 0.0)
  )
)

(defun cross_product (v1 v2)
  (setq v1 (if (= (length v1) 2) (append v1 '(0.0)) v1))
  (setq v2 (if (= (length v2) 2) (append v2 '(0.0)) v2))
  (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
        (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
        (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
)