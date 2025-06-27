(defun c:DrawSwitchAxisPro ( / p1_ent p4_ent p3_ent p5_ent p1_coords p4_coords p3_coords p5_coords
                                 line_straight_obj proj_pt csp_pt branch_angle branch_end_pt
                                 branch_axis_obj p5_projected_on_branch p5_block_vla ) ; <--- Нові змінні
  (vl-load-com) ; Завантажуємо ActiveX-функції для роботи з об'єктами
  (princ "\n--- Побудова осі стрілочного переводу (1/11) за блоками (Pro) ---")

  ;; Допоміжна функція для вибору блоку та отримання його точки вставки
  ;; Повертає список (координати_точки_вставки . VLA_об'єкт_блоку) або nil
  (defun GetBlockInsertionPointAndVLA (prompt_msg / ent_data ent_name ent_list ent_type insertion_pt vla_obj)
    (setq ent_data (entsel prompt_msg))
    (if ent_data
      (progn
        (setq ent_name (car ent_data))
        (setq vla_obj (vlax-ename->vla-object ent_name)) ; Отримуємо VLA-об'єкт блоку
        (setq ent_list (entget ent_name))
        (setq ent_type (cdr (assoc 0 ent_list)))

        (if (equal ent_type "INSERT")
          (progn
            (setq insertion_pt (cdr (assoc 10 ent_list)))
            (princ (strcat "\nОбрано блок. Точка: " (vl-princ-to-string insertion_pt)))
            (cons insertion_pt vla_obj) ; Повертаємо пару: координати . VLA-об'єкт
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

  ;; 1. Запит блоків у користувача та отримання їхніх координат і VLA-об'єктів
  (setq p1_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки стику рамної рейки (P1): "))
  (if (not p1_data) (progn (princ "\nОперацію скасовано.") (exit)))
  (setq p1_coords (car p1_data))

  (setq p4_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по прямому напрямку (P4): "))
  (if (not p4_data) (progn (princ "\nОперацію скасовано.") (exit)))
  (setq p4_coords (car p4_data))

  (setq p3_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки центру хрестовини (P3): "))
  (if (not p3_data) (progn (princ "\nОперацію скасовано.") (exit)))
  (setq p3_coords (car p3_data))

  (setq p5_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по відгалуженню (P5): "))
  (if (not p5_data) (progn (princ "\nОперацію скасовано.") (exit)))
  (setq p5_coords (car p5_data))
  (setq p5_block_vla (cdr p5_data)) ; <--- Зберігаємо VLA-об'єкт блоку P5

  ;; --- Починаємо побудову, використовуючи отримані координати ---

  ;; 2. Побудова прямої полілінії між P1 і P4
  ;;    Створюємо 2D полілінію
  (command "_.PLINE" p1_coords p4_coords "") ; <--- Використовуємо PLINE
  (setq line_straight_obj (vlax-ename->vla-object (entlast)))

  ;; 3. Від точки центру хрестовини (P3) провести перпендикуляр до прямої лінії P1-P4
  (setq proj_pt (vlax-curve-getClosestPointTo line_straight_obj p3_coords))
  ;(command "_.POINT" proj_pt) ; Можна тимчасово залишити для візуалізації

  ;; 4. Від отриманої точки (proj_pt) знайти Центр Стрілочного Переводу (ЦСП)
  (setq dist_to_csp 16.72)
  (setq vec_p1_proj (mapcar '- p1_coords proj_pt))
  (setq vec_p1_proj_unit (unit_vector vec_p1_proj))
  (setq csp_pt (mapcar '+ proj_pt (mapcar '* vec_p1_proj_unit (list dist_to_csp dist_to_csp 0.0))))
  ;(command "_.POINT" csp_pt) ; Можна тимчасово залишити для візуалізації

  ;; 5. Визначення напрямку відгалуження (вліво/вправо)
  (setq vec_line (mapcar '- p4_coords p1_coords))
  (setq vec_test (mapcar '- p5_coords p1_coords))
  (setq cross_z (caddr (cross_product vec_line vec_test)))
  (setq is_left (if (> cross_z 0) T nil))

  ;; 6. Побудова осі відгалуження від ЦСП
  (setq branch_length 20.0) ; Початкова довжина для побудови
  (setq branch_angle_deg 5.194444444)
  (setq branch_angle_rad (dtr branch_angle_deg))
  (setq straight_line_angle (angle p1_coords p4_coords))

  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad))
    (setq final_branch_angle (- straight_line_angle branch_angle_rad))
  )

  (setq temp_branch_end_pt (polar csp_pt final_branch_angle branch_length)) ; Тимчасова кінцева точка для полілінії
  (command "_.PLINE" csp_pt temp_branch_end_pt "") ; <--- Креслимо полілінію відгалуження
  (setq branch_axis_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт нової осі відгалуження

  ;; --- Проектування P5 на вісь відгалуження ---
  (setq p5_projected_on_branch (vlax-curve-getClosestPointTo branch_axis_obj p5_coords))
  ;(command "_.POINT" p5_projected_on_branch) ; Можна тимчасово залишити для візуалізації

  ;; --- Переміщення блоку P5 на спроектовану точку ---
  (if p5_block_vla
    (vlax-invoke p5_block_vla 'move (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename p5_block_vla))))) (vlax-3d-point p5_projected_on_branch))
    (princ "\nПомилка: Не вдалося перемістити блок P5, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини осі відгалуження ---
  ;; Змінюємо кінцеву точку полілінії на спроектовану P5
  ;; Для поліліній це робиться через її вершини.
  (if (eq (vla-get-objectname branch_axis_obj) "AcDbPolyline") ; Перевірка, що це справді легка полілінія (2D)
    (progn
      (vla-put-Coordinate branch_axis_obj 1 (vlax-3d-point p5_projected_on_branch)) ; Встановлюємо 2-гу вершину (індекс 1)
      (vla-update branch_axis_obj) ; Оновлюємо об'єкт в AutoCAD
      (princ "\nВісь відгалуження обрізано/продовжено до спроектованої точки P5.")
    )
    (princ "\nПомилка: Об'єкт осі відгалуження не є легкою полілінією, не вдалося обрізати.")
  )

  (princ "\n--- Осі стрілочного переводу побудовано, блок P5 переміщено та вісь відгалуження скориговано! ---")
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