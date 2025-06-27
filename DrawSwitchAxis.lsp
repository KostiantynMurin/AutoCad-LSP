(defun c:DrawSwitchAxis ( / p1 p4 p3 p5 line_straight proj_pt csp_pt branch_angle branch_end_pt )
  (vl-load-com) ; Завантажуємо ActiveX-функції для роботи з об'єктами
  (princ "\n--- Побудова осі стрілочного переводу (1/11) ---")

  ;; 1. Запит точок у користувача
  (setq p1 (getpoint "\nВкажіть точку стику рамної рейки (P1): "))
  (setq p4 (getpoint "\nВкажіть точку хвоста хрестовини по прямому напрямку (P4): "))
  (setq p3 (getpoint "\nВкажіть точку центру хрестовини (P3): "))
  (setq p5 (getpoint "\nВкажіть точку хвоста хрестовини по відгалуженню (P5): "))

  ;; 2. Побудова прямої лінії між P1 і P4
  (setq line_straight (entmakepoint (list (car p1) (cadr p1)))) ; Початок прямої лінії
  (setq line_straight_end (entmakepoint (list (car p4) (cadr p4)))) ; Кінець прямої лінії

  (command "_.LINE" p1 p4 "") ; Креслимо пряму вісь P1-P4

  ;; 3. Від точки центру хрестовини (P3) провести перпендикуляр до прямої лінії P1-P4
  ;;    І знайти точку перетину (проекція P3)
  (setq proj_pt (vlax-curve-getClosestPointTo (vlax-ename->vla-object (entlast)) p3))
  ;; entlast поверне останній створений об'єкт, тобто нашу лінію P1-P4
  ;; vlax-curve-getClosestPointTo знайде проекцію P3 на цю лінію

  (command "_.POINT" proj_pt) ; Для візуалізації: ставимо точку проекції

  ;; 4. Від отриманої точки (proj_pt) знайти Центр Стрілочного Переводу (ЦСП)
  ;;    Відкласти 16.72 в сторону P1 від proj_pt вздовж прямої P1-P4
  (setq dist_to_csp 16.72)
  (setq vec_p4_p1 (mapcar '- p1 proj_pt)) ; Вектор від proj_pt до P1
  (setq vec_p4_p1_unit (unit_vector vec_p4_p1)) ; Одиничний вектор
  (setq csp_pt (mapcar '+ proj_pt (mapcar '* vec_p4_p1_unit (list dist_to_csp dist_to_csp))))

  (command "_.POINT" csp_pt) ; Для візуалізації: ставимо точку ЦСП

  ;; 5. Визначення напрямку відгалуження (вліво/вправо)
  ;;    Цей крок складніший, оскільки потрібно визначити з якого боку P5 відносно лінії P1-P4
  ;;    Використовуємо векторний добуток або sign of cross product
  (setq p1_vec (vlax-3D-point p1))
  (setq p4_vec (vlax-3D-point p4))
  (setq p3_vec (vlax-3D-point p3))
  (setq p5_vec (vlax-3D-point p5))

  (setq vec_straight (mapcar '- p4 p1)) ; Вектор прямого напрямку
  (setq vec_branch_test (mapcar '- p5 p3)) ; Вектор від P3 до P5

  ;; Визначаємо напрямок відхилення відносно прямої P1-P4
  ;; Якщо (cross (P4-P1) (P5-P1)) Z-координата > 0, то P5 лівіше (проти годинникової стрілки)
  ;; Якщо < 0, то P5 правіше (за годинниковою стрілкою)
  (setq direction_val (caddr (cross_product (mapcar '- p4 p1) (mapcar '- p5 p1))))
  (setq is_left nil)
  (if (> direction_val 0)
    (setq is_left T) ; Відгалуження вліво
    (setq is_left nil) ; Відгалуження вправо
  )

  ;; 6. Побудова осі відгалуження від ЦСП
  (setq branch_length 20.0)
  (setq branch_angle_rad (dtr 5.194444444)) ; 5d11'40" в градусах = 5 + 11/60 + 40/3600 = 5.194444444
                                          ; Переводимо в радіани для тригонометричних функцій

  (setq straight_angle (angle p1 p4)) ; Кут прямої лінії P1-P4

  (if is_left
    (setq final_branch_angle (+ straight_angle branch_angle_rad)) ; Додаємо кут, якщо вліво
    (setq final_branch_angle (- straight_angle branch_angle_rad)) ; Віднімаємо кут, якщо вправо
  )

  (setq branch_end_pt (polar csp_pt final_branch_angle branch_length))

  (command "_.LINE" csp_pt branch_end_pt "") ; Креслимо вісь відгалуження

  (princ "\n--- Осі стрілочного переводу побудовано! ---")
  (princ)
)

;; Допоміжні функції (потрібні для LISP)
(defun dtr (a) (* pi (/ a 180.0))) ; Перетворення градусів в радіани
(defun rtd (a) (* 180.0 (/ a pi))) ; Перетворення радіан в градуси (для перевірки)

;; Функція для отримання одиничного вектора
(defun unit_vector (vec)
  (setq len (distance '(0 0 0) vec))
  (if (> len 0.0)
    (mapcar '/ vec (list len len len))
    '(0.0 0.0 0.0) ; Повертаємо нульовий вектор, якщо довжина 0
  )
)

;; Функція для векторного добутку (cross product) - для визначення ліво/право
(defun cross_product (v1 v2)
  (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
        (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
        (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
)