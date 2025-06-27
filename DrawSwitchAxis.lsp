(defun c:DrawSwitchAxisBlocks ( / p1_ent p4_ent p3_ent p5_ent p1_coords p4_coords p3_coords p5_coords
                                 line_straight_obj proj_pt csp_pt branch_angle branch_end_pt
                                 branch_axis_obj p5_projected_on_branch ) ; <-- Додаємо нові змінні
  (vl-load-com)
  (princ "\n--- Побудова осі стрілочного переводу (1/11) за блоками ---")

  (defun GetBlockInsertionPoint (prompt_msg / ent_data ent_name ent_list ent_type insertion_pt)
    (setq ent_data (entsel prompt_msg))
    (if ent_data
      (progn
        (setq ent_name (car ent_data))
        (setq ent_list (entget ent_name))
        (setq ent_type (cdr (assoc 0 ent_list)))

        (if (equal ent_type "INSERT")
          (progn
            (setq insertion_pt (cdr (assoc 10 ent_list)))
            (princ (strcat "\nОбрано блок. Точка: " (vl-princ-to-string insertion_pt)))
            insertion_pt
          )
          (progn
            (princ "\nПомилка: Вибраний об'єкт не є блоком. Спробуйте ще раз.")
            nil
          )
        )
      )
      (progn
        (princ "\nВідміна вибору. Операцію скасовано.")
        nil
      )
    )
  )

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
  (command "_.LINE" p1_coords p4_coords "")
  (setq line_straight_obj (vlax-ename->vla-object (entlast)))

  ;; 3. Від точки центру хрестовини (P3) провести перпендикуляр до прямої лінії P1-P4
  (setq proj_pt (vlax-curve-getClosestPointTo line_straight_obj p3_coords))
  (command "_.POINT" proj_pt) ; Для візуалізації: ставимо точку проекції

  ;; 4. Від отриманої точки (proj_pt) знайти Центр Стрілочного Переводу (ЦСП)
  (setq dist_to_csp 16.72)
  (setq vec_p1_proj (mapcar '- p1_coords proj_pt))
  (setq vec_p1_proj_unit (unit_vector vec_p1_proj))
  (setq csp_pt (mapcar '+ proj_pt (mapcar '* vec_p1_proj_unit (list dist_to_csp dist_to_csp 0.0))))
  (command "_.POINT" csp_pt) ; Для візуалізації: ставимо точку ЦСП

  ;; 5. Визначення напрямку відгалуження (вліво/вправо)
  (setq vec_line (mapcar '- p4_coords p1_coords))
  (setq vec_test (mapcar '- p5_coords p1_coords))
  (setq cross_z (caddr (cross_product vec_line vec_test)))
  (setq is_left (if (> cross_z 0) T nil))

  ;; 6. Побудова осі відгалуження від ЦСП
  (setq branch_length 20.0) ; Тимчасова довжина, потім можемо її обрізати або продовжити до проекції P5
  (setq branch_angle_deg 5.194444444)
  (setq branch_angle_rad (dtr branch_angle_deg))
  (setq straight_line_angle (angle p1_coords p4_coords))

  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad))
    (setq final_branch_angle (- straight_line_angle branch_angle_rad))
  )

  (setq branch_end_pt (polar csp_pt final_branch_angle branch_length))

  (command "_.LINE" csp_pt branch_end_pt "") ; Креслимо вісь відгалуження
  (setq branch_axis_obj (vlax-ename->vla-object (entlast))) ; <--- Отримуємо об'єкт нової осі відгалуження

  ;; --- НОВИЙ КРОК: Проектування P5 на вісь відгалуження ---
  (setq p5_projected_on_branch (vlax-curve-getClosestPointTo branch_axis_obj p5_coords))
  (command "_.POINT" p5_projected_on_branch) ; Для візуалізації: ставимо точку спроектованого P5

  ;; Можливо, тепер ми захочемо обрізати або продовжити вісь відгалуження до p5_projected_on_branch.
  ;; Для цього нам знадобиться замінити вже існуючу лінію на нову або змінити її кінцеву точку.
  ;; Або просто використовувати цю точку p5_projected_on_branch для подальших побудов (наприклад, символів).

  (princ "\n--- Осі стрілочного переводу побудовано та P5 спроектовано на вісь відгалуження! ---")
  (princ)
)

;; Допоміжні функції (без змін)
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