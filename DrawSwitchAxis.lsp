(defun c:DrawSwitchAxis ( / p1 p4 p3 p5 line_straight_obj proj_pt csp_pt branch_angle branch_end_pt )
  (vl-load-com) ; Завантажуємо ActiveX-функції для роботи з об'єктами
  (princ "\n--- Побудова осі стрілочного переводу (1/11) ---")

  ;; 1. Запит точок у користувача
  (setq p1 (getpoint "\nВкажіть точку стику рамної рейки (P1): "))
  (setq p4 (getpoint "\nВкажіть точку хвоста хрестовини по прямому напрямку (P4): "))
  (setq p3 (getpoint "\nВкажіть точку центру хрестовини (P3): "))
  (setq p5 (getpoint "\nВкажіть точку хвоста хрестовини по відгалуженню (P5): "))

  ;; 2. Побудова прямої лінії між P1 і P4
  (command "_.LINE" p1 p4 "") ; Креслимо пряму вісь P1-P4
  (setq line_straight_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт тільки що створеної лінії

  ;; 3. Від точки центру хрестовини (P3) провести перпендикуляр до прямої лінії P1-P4
  ;;    І знайти точку перетину (проекція P3)
  (setq proj_pt (vlax-curve-getClosestPointTo line_straight_obj p3))
  ;; line_straight_obj - це тепер наш VLA-об'єкт лінії

  (command "_.POINT" proj_pt) ; Для візуалізації: ставимо точку проекції

  ;; 4. Від отриманої точки (proj_pt) знайти Центр Стрілочного Переводу (ЦСП)
  ;;    Відкласти 16.72 в сторону P1 від proj_pt вздовж прямої P1-P4
  (setq dist_to_csp 16.72)
  (setq vec_p1_proj (mapcar '- p1 proj_pt)) ; Вектор від proj_pt до P1
  (setq vec_p1_proj_unit (unit_vector vec_p1_proj)) ; Одиничний вектор
  (setq csp_pt (mapcar '+ proj_pt (mapcar '* vec_p1_proj_unit (list dist_to_csp dist_to_csp 0.0)))) ; Враховуємо Z-координату як 0.0

  (command "_.POINT" csp_pt) ; Для візуалізації: ставимо точку ЦСП

  ;; 5. Визначення напрямку відгалуження (вліво/вправо)
  ;;    Використовуємо векторний добуток P1-P4 та P1-P5.
  ;;    Це дозволить визначити, з якого боку P5 відносно прямої P1-P4.
  ;;    Це буде індикатор для визначення, куди повернути кут відгалуження.

  (setq vec_line (mapcar '- p4 p1)) ; Вектор прямої P1-P4
  (setq vec_test (mapcar '- p5 p1)) ; Вектор P1-P5

  ;; Обчислення Z-координати векторного добутку.
  ;; Якщо Z > 0, P5 знаходиться ліворуч від вектора P1->P4.
  ;; Якщо Z < 0, P5 знаходиться праворуч від вектора P1->P4.
  (setq cross_z (caddr (cross_product vec_line vec_test)))

  (setq is_left nil)
  (if (> cross_z 0)
    (setq is_left T) ; Відгалуження вліво (проти годинникової стрілки від прямої осі)
    (setq is_left nil) ; Відгалуження вправо (за годинниковою стрілкою від прямої осі)
  )

  ;; 6. Побудова осі відгалуження від ЦСП
  (setq branch_length 20.0)
  (setq branch_angle_deg 5.194444444) ; 5d11'40" в десяткових градусах
  (setq branch_angle_rad (dtr branch_angle_deg)) ; Переводимо в радіани

  (setq straight_line_angle (angle p1 p4)) ; Кут прямої лінії P1-P4 в радіанах

  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad)) ; Додаємо кут, якщо відгалуження вліво
    (setq final_branch_angle (- straight_line_angle branch_angle_rad)) ; Віднімаємо кут, якщо відгалуження вправо
  )

  (setq branch_end_pt (polar csp_pt final_branch_angle branch_length))

  (command "_.LINE" csp_pt branch_end_pt "") ; Креслимо вісь відгалуження

  (princ "\n--- Осі стрілочного переводу побудовано! ---")
  (princ)
)

;; Допоміжні функції (потрібні для LISP)
(defun dtr (a) (* pi (/ a 180.0))) ; Перетворення градусів в радіани
(defun rtd (a) (* 180.0 (/ a pi))) ; Перетворення радіан в градуси (для перевірки)

;; Функція для отримання одиничного вектора (має працювати для 2D/3D)
(defun unit_vector (vec)
  (setq len (distance '(0 0 0) vec))
  (if (and (numberp len) (> len 0.00000001)) ; Перевірка на число і майже нуль
    (mapcar '/ vec (list len len (if (caddr vec) len 1.0))) ; Для 2D/3D, якщо Z-координата існує
    '(0.0 0.0 0.0) ; Повертаємо нульовий вектор, якщо довжина 0 або нечисло
  )
)

;; Функція для векторного добутку (cross product) - для визначення ліво/право
;; Працює для 3D векторів, але для 2D можна використовувати Z-координату як 0.
(defun cross_product (v1 v2)
  (setq v1 (if (= (length v1) 2) (append v1 '(0.0)) v1)) ; Перетворюємо 2D в 3D
  (setq v2 (if (= (length v2) 2) (append v2 '(0.0)) v2)) ; Перетворюємо 2D в 3D
  (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
        (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
        (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
)