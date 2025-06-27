;; ============================================================
;; == Скрипт для побудови осі стрілочного переводу (марка 1/9 або 1/11) ==
;; == ПОВНІСТЮ В 2D (ІГНОРУВАННЯ Z-КООРДИНАТ) ДЛЯ ГЕОМЕТРИЧНИХ РОЗРАХУНКІВ ==
;; == Призначення:
;; ==   1. Користувач обирає 5 блоків-точок геодезичної зйомки.
;; ==   2. АВТОМАТИЧНО визначає марку хрестовини (1/9 або 1/11)
;; ==      за фактичним кутом відгалуження в площині XY.
;; ==   3. Всі геометричні розрахунки виконуються в 2D (Z=0).
;; ==   4. Будує пряму полілінію (P1-P2_proj-P4) в 2D.
;; ==   5. Визначає проекцію P3 на пряму вісь (в 2D).
;; ==   6. Розраховує Центр Стрілочного Переводу (ЦСП) на прямій осі (в 2D).
;; ==   7. Будує полілінію відгалуження від ЦСП з кутом,
;; ==      який відповідає визначеній марці (в 2D).
;; ==   8. Проектує оригінальний блок P2 на пряму вісь (в 2D) та переміщує його,
;; ==      ЗБЕРІГАЮЧИ ОРИГІНАЛЬНУ Z-КООРДИНАТУ блоку.
;; ==   9. Проектує оригінальний блок P5 на створену вісь відгалуження (в 2D) та переміщує його,
;; ==      ЗБЕРІГАЮЧИ ОРИГІНАЛЬНУ Z-КООРДИНАТУ блоку.
;; ==   10. Обрізає/продовжує осі до спроектованих точок (в 2D).
;; ==
;; == Виклик: DrawSwitchAxisPro
;; ============================================================

(vl-load-com) ; Переконатися, що VLISP функції доступні

;; Глобальні змінні для збереження оригінальних налаштувань AutoCAD
(setq *oldEcho* nil)
(setq *oldOsmode* nil)
(setq *oldCmdDia* nil)
(setq *oldOsmodeZ* nil)

;; Локальна функція обробки помилок для відновлення налаштувань
(defun *error* (msg)
  (if *oldEcho* (setvar "CMDECHO" *oldEcho*))
  (if *oldOsmode* (setvar "OSMODE" *oldOsmode*))
  (if *oldCmdDia* (setvar "CMDDIA" *oldCmdDia*))
  (if *oldOsmodeZ* (setvar "OSNAPZ" *oldOsmodeZ*))
  (sssetfirst nil nil) ; Зняти виділення при помилці або скасуванні
  (princ "\n*** Помилка LISP або скасовано: ")
  (if msg (princ msg))
  (princ " ***")
  (princ)
)

;; Допоміжна функція для отримання одиничного вектора
;; Працює з векторами будь-якої розмірності (2D або 3D)
(defun unit_vector (vec)
  (setq len (distance (apply 'list (mapcar (function (lambda (x) 0.0)) vec)) vec))
  (if (and (numberp len) (> len 0.000001))
    (mapcar '/ vec (list len len (if (= (length vec) 3) len 1.0)))
    (apply 'list (mapcar (function (lambda (x) 0.0)) vec)) ; Нульовий вектор
  )
)

;; Допоміжна функція для скалярного добутку векторів
(defun dot_product (v1 v2)
  (apply '+ (mapcar '* v1 v2))
)

;; Допоміжна функція для векторного добутку (cross product) - для визначення ліво/право
;; Працює для 3D векторів, але для 2D використовується Z-компонента результату
(defun cross_product (v1 v2)
  (setq v1 (if (= (length v1) 2) (append v1 '(0.0)) v1))
  (setq v2 (if (= (length v2) 2) (append v2 '(0.0)) v2))
  (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
        (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
        (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
)

;; Допоміжні функції для перетворення кутів
(defun dtr (a) (* pi (/ a 180.0))) ; Градуси в радіани
(defun rtd (a) (* 180.0 (/ a pi))) ; Радіани в градуси

;; Допоміжна функція для вибору блоку та отримання його точки вставки та VLA-об'єкта
;; Повертає список (координати_точки_вставки . VLA_об'єкт_блоку) або nil
(defun GetBlockInsertionPointAndVLA (prompt_msg / ent_data ent_name ent_list ent_type insertion_pt vla_obj)
  (setq ent_data (entsel prompt_msg))
  (if ent_data
    (progn
      (setq ent_name (car ent_data))
      (setq vla_obj (vlax-ename->vla-object ent_name))
      (setq ent_list (entget ent_name))
      (setq ent_type (cdr (assoc 0 ent_list)))

      (if (equal ent_type "INSERT")
        (progn
          (setq insertion_pt (cdr (assoc 10 ent_list))) ; Отримуємо 3D точку вставки з DXF-коду 10
          (princ (strcat "\nОбрано блок. Точка: " (vl-princ-to-string insertion_pt)))
          (cons insertion_pt vla_obj) ; Повертаємо пару: 3D координати . VLA-об'єкт
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

;; ============================================================
;; == ОСНОВНА ФУНКЦІЯ СКРИПТА ==
;; ============================================================
(defun c:DrawSwitchAxisPro ( / p1_data p2_data p4_data p3_data p5_data
                                 p1_orig_coords p2_orig_coords p4_orig_coords p3_orig_coords p5_orig_coords ; Оригінальні 3D координати
                                 p1_2d_coords p2_2d_coords p4_2d_coords p3_2d_coords p5_2d_coords ; 2D (Z=0) координати для геометрії
                                 straight_axis_obj
                                 proj_pt_p3 proj_pt_p2 csp_pt branch_angle branch_end_pt
                                 branch_axis_obj p2_projected_on_straight p5_projected_on_branch
                                 p2_block_vla p5_block_vla
                                 final_p2_target_pt final_p5_target_pt
                                 temp_straight_axis_ent temp_branch_axis_ent
                                 actual_branch_angle_rad actual_branch_angle_deg
                                 mark_1_9_angle_deg mark_1_11_angle_deg
                                 mark_1_9_dist_to_csp mark_1_11_dist_to_csp
                                 determined_mark
                                 dist_to_csp branch_angle_deg )
  
  ;; Зберегти поточні налаштування AutoCAD
  (setq *oldEcho* (getvar "CMDECHO"))
  (setq *oldOsmode* (getvar "OSMODE"))
  (setq *oldCmdDia* (getvar "CMDDIA"))
  (setq *oldOsmodeZ* (getvar "OSNAPZ"))
  
  ;; Встановити налаштування для скрипта
  (setvar "CMDECHO" 0) ; Вимикаємо ехо команд
  (setvar "CMDDIA" 0) ; Вимикаємо діалоги
  (setvar "OSNAPZ" 0) ; Тимчасово встановлюємо OSNAPZ в 0 для коректної роботи MOVE з 3D точками

  (princ "\n--- Побудова осі стрілочного переводу (Авто-визначення марки) ---")

  ;; 1. Запит блоків у користувача та отримання їхніх оригінальних 3D координат і VLA-об'єктів
  (setq p1_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки стику рамної рейки (P1): "))
  (if (not p1_data) (*error* "Вибір P1 скасовано."))
  (setq p1_orig_coords (car p1_data))

  (setq p2_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки початку вістря (P2): "))
  (if (not p2_data) (*error* "Вибір P2 скасовано."))
  (setq p2_orig_coords (car p2_data))
  (setq p2_block_vla (cdr p2_data))

  (setq p4_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по прямому напрямку (P4): "))
  (if (not p4_data) (*error* "Вибір P4 скасовано."))
  (setq p4_orig_coords (car p4_data))

  (setq p3_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки центру хрестовини (P3): "))
  (if (not p3_data) (*error* "Вибір P3 скасовано."))
  (setq p3_orig_coords (car p3_data))

  (setq p5_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по відгалуженню (P5): "))
  (if (not p5_data) (*error* "Вибір P5 скасовано."))
  (setq p5_orig_coords (car p5_data))
  (setq p5_block_vla (cdr p5_data))

  ;; === Перетворюємо всі оригінальні 3D-координати на 2D-координати (Z=0) для всіх ГЕОМЕТРИЧНИХ РОЗРАХУНКІВ ===
  (setq p1_2d_coords (list (car p1_orig_coords) (cadr p1_orig_coords) 0.0))
  (setq p2_2d_coords (list (car p2_orig_coords) (cadr p2_orig_coords) 0.0))
  (setq p3_2d_coords (list (car p3_orig_coords) (cadr p3_orig_coords) 0.0))
  (setq p4_2d_coords (list (car p4_orig_coords) (cadr p4_orig_coords) 0.0))
  (setq p5_2d_coords (list (car p5_orig_coords) (cadr p5_orig_coords) 0.0))
  
  (princ (strcat "\nDBG: P3_2D: " (vl-princ-to-string p3_2d_coords)))
  (princ (strcat "\nDBG: P4_2D: " (vl-princ-to-string p4_2d_coords)))
  (princ (strcat "\nDBG: P5_2D: " (vl-princ-to-string p5_2d_coords)))


  ;; ===================================================================
  ;; == АВТОМАТИЧНЕ ВИЗНАЧЕННЯ МАРКИ ХРЕСТОВИНИ (ТЕПЕР ЧЕРЕЗ angle() ДЛЯ 2D!) ==
  ;; ===================================================================
  (princ "\nВизначення марки хрестовини за фактичним кутом (XY, через функцію angle())...")
  
  ;; Використовуємо функцію (angle) для отримання кутів відносно осі X для 2D-векторів
  (setq angle_p3_p4 (angle p3_2d_coords p4_2d_coords)) ; Кут вектора P3->P4 відносно X-осі
  (setq angle_p3_p5 (angle p3_2d_coords p5_2d_coords)) ; Кут вектора P3->P5 відносно X-осі

  ;; Обчислюємо абсолютну різницю кутів (завжди позитивна і менше 360 градусів)
  (setq actual_branch_angle_rad (abs (- angle_p3_p4 angle_p3_p5)))
  
  ;; Обробка випадку, коли кут більший за PI (180 градусів), щоб взяти меншу дугу
  (if (> actual_branch_angle_rad pi)
    (setq actual_branch_angle_rad (- (* 2 pi) actual_branch_angle_rad)) ; 360 - кут
  )

  (setq actual_branch_angle_deg (rtd actual_branch_angle_rad))
  (princ (strcat "\nФактичний кут відгалуження (градуси, XY): " (rtos actual_branch_angle_deg 2 8)))

  ;; Еталонні кути для порівняння (в градусах)
  (setq mark_1_9_angle_deg 6.340277778)   ; 6°20'25"
  (setq mark_1_11_angle_deg 5.194444444) ; 5°11'40"

  ;; Визначаємо, яка марка ближча
  (setq diff_9 (abs (- actual_branch_angle_deg mark_1_9_angle_deg)))
  (setq diff_11 (abs (- actual_branch_angle_deg mark_1_11_angle_deg)))

  (if (< diff_9 diff_11)
    (setq determined_mark "1/9")
    (setq determined_mark "1/11") ; За замовчуванням, якщо 11 ближче або однаково
  )
  (princ (strcat "\nАвтоматично визначена марка хрестовини: " determined_mark))

  ;; Встановлюємо параметри відповідно до визначеної марки
  (cond
    ((equal determined_mark "1/9")
     (setq dist_to_csp 13.68
           branch_angle_deg mark_1_9_angle_deg)
    )
    ((equal determined_mark "1/11")
     (setq dist_to_csp 16.72
           branch_angle_deg mark_1_11_angle_deg)
    )
  )
  ;; ===================================================================
  ;; == КІНЕЦЬ АВТОМАТИЧНОГО ВИЗНАЧЕННЯ МАРКИ ==
  ;; ===================================================================

  ;; 2. Побудова початкової прямої полілінії між P1 і P4 (тимчасова для проекції P2 і P3)
  ;; Використовуємо 2D-координати для створення поліліній
  (command "_.PLINE" p1_2d_coords p4_2d_coords "")
  (setq straight_axis_obj (vlax-ename->vla-object (entlast)))
  (setq temp_straight_axis_ent (entlast))

  ;; 3. Проектування P2 та P3 на тимчасову пряму вісь (в 2D)
  (setq proj_pt_p2 (vlax-curve-getClosestPointTo straight_axis_obj p2_2d_coords)) ; <--- Проектуємо P2 в 2D
  (setq proj_pt_p3 (vlax-curve-getClosestPointTo straight_axis_obj p3_2d_coords)) ; Проектуємо P3 в 2D

  ;; 4. Розрахунок Центру Стрілочного Переводу (ЦСП) на основі спроектованої P3 (в 2D)
  ;;    dist_to_csp вже визначено вище
  (setq vec_p1_proj (mapcar '- p1_2d_coords proj_pt_p3)) ; Вектор у 2D
  (setq vec_p1_proj_unit (unit_vector vec_p1_proj)) ; Одиничний вектор у 2D
  (setq csp_pt (mapcar '+ proj_pt_p3 (mapcar '* vec_p1_proj_unit (list dist_to_csp dist_to_csp 0.0)))) ; ЦСП у 2D

  ;; --- Переміщення блоку P2 на спроектовану точку (ЗБЕРІГАЮЧИ ОРИГІНАЛЬНУ Z!) ---
  (if p2_block_vla
    (progn
      ;; final_p2_target_pt формується з XY спроектованої 2D точки та ОРИГІНАЛЬНОЇ Z блоку P2
      (setq final_p2_target_pt (list (car proj_pt_p2)
                                     (cadr proj_pt_p2)
                                     (caddr p2_orig_coords))) ; <--- ЗБЕРІГАЄМО ОРИГІНАЛЬНУ Z
      
      (command "_move" (vlax-vla-object->ename p2_block_vla) "" 
               "_none" p2_orig_coords ; Оригінальна 3D точка P2 як база для переміщення
               "_none" final_p2_target_pt)
      (princ "\nБлок P2 переміщено на спроектовану точку, ЗБЕРІГАЮЧИ оригінальну Z-координату.")
    )
    (princ "\nПомилка: Не вдалося перемістити блок P2, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини ПРЯМОЇ осі (P1 - P2_proj - P4) ---
  (if temp_straight_axis_ent
    (progn
      (vla-delete (vlax-ename->vla-object temp_straight_axis_ent))
      (setq straight_axis_obj nil) ; Очищаємо змінну
      (princ "\nТимчасова пряма вісь видалена.")
      
      ;; Створюємо нову пряму полілінію P1 - P2_proj - P4 в 2D
      ;; Z-координати для полілінії будуть 0.0, оскільки ми працюємо в 2D.
      (setq p2_proj_for_pline (list (car proj_pt_p2)
                                    (cadr proj_pt_p2)
                                    0.0)) ; Z = 0.0 для полілінії
      
      (command "_.PLINE" p1_2d_coords p2_proj_for_pline p4_2d_coords "") ; <--- Нова пряма полілінія з 2D-координатами
      (setq straight_axis_obj (vlax-ename->vla-object (entlast))) ; Оновлюємо VLA-об'єкт прямої осі
      (princ "\nНова пряма вісь (P1-P2_proj-P4) створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити пряму вісь.")
  )

  ;; 5. Визначення напрямку відгалуження (вліво/вправо)
  ;; Використовуємо 2D-координати для цього розрахунку
  (setq vec_line (mapcar '- p4_2d_coords p1_2d_coords))
  (setq vec_test (mapcar '- p5_2d_coords p1_2d_coords))
  (setq cross_z (caddr (cross_product vec_line vec_test)))
  (setq is_left (if (> cross_z 0) T nil))

  ;; 6. Побудова осі відгалуження від ЦСП (початкова, тимчасова)
  (setq branch_length 20.0) ; Початкова довжина
  ;;    branch_angle_deg вже визначено вище
  (setq branch_angle_rad (dtr branch_angle_deg))
  (setq straight_line_angle (angle p1_2d_coords p4_2d_coords)) ; Кут прямої осі в 2D

  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad))
    (setq final_branch_angle (- straight_line_angle branch_angle_rad))
  )

  (setq temp_branch_end_pt (polar csp_pt final_branch_angle branch_length)) ; ЦСП і кут вже 2D
  (command "_.PLINE" csp_pt temp_branch_end_pt "") ; Креслимо полілінію в 2D
  (setq temp_branch_axis_ent (entlast))

  ;; --- Проектування P5 на вісь відгалуження (в 2D) ---
  (setq branch_axis_obj (vlax-ename->vla-object temp_branch_axis_ent))
  (setq p5_projected_on_branch (vlax-curve-getClosestPointTo branch_axis_obj p5_2d_coords))

  ;; --- Переміщення блоку P5 на спроектовану точку (ЗБЕРІГАЮЧИ ОРИГІНАЛЬНУ Z!) ---
  (if p5_block_vla
    (progn
      ;; final_p5_target_pt формується з XY спроектованої 2D точки та ОРИГІНАЛЬНОЇ Z блоку P5
      (setq final_p5_target_pt (list (car p5_projected_on_branch)
                                     (cadr p5_projected_on_branch)
                                     (caddr p5_orig_coords))) ; <--- ЗБЕРІГАЄМО ОРИГІНАЛЬНУ Z
      
      (command "_move" (vlax-vla-object->ename p5_block_vla) "" 
               "_none" p5_orig_coords ; Оригінальна 3D точка P5 як база для переміщення
               "_none" final_p5_target_pt)
      (princ "\nБлок P5 переміщено на спроектовану точку, ЗБЕРІГАЮЧИ оригінальну Z-координату.")
    )
    (princ "\nПомилка: Не вдалося перемістити блок P5, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини осі відгалуження (надійний підхід, в 2D) ---
  (if (and temp_branch_axis_ent branch_axis_obj)
    (progn
      (vla-delete branch_axis_obj)
      (setq branch_axis_obj nil) ; Очищаємо змінну
      (princ "\nТимчасова вісь відгалуження видалена.")

      ;; Створюємо нову, коректну полілінію від ЦСП до спроектованої P5 (в 2D)
      ;; Z-координата для полілінії буде 0.0, оскільки ми працюємо в 2D.
      (setq p5_proj_for_pline_branch (list (car p5_projected_on_branch)
                                            (cadr p5_projected_on_branch)
                                            0.0)) ; Z = 0.0 для полілінії
      
      (command "_.PLINE" csp_pt p5_proj_for_pline_branch "") ; Створюємо нову полілінію в 2D
      (setq branch_axis_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт нової, коректної полілінії
      (princ "\nНова вісь відгалуження від ЦСП до спроектованої P5 створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити вісь відгалуження.")
  )

  (princ (strcat "\n--- Осі стрілочного переводу марки " determined_mark " побудовано, блоки переміщено та осі скориговано! ---"))
  (princ "\nСкрипт завершено.")
  (princ)

  ;; Відновити оригінальні налаштування AutoCAD
  (setq *oldEcho* nil)
  (setq *oldOsmode* nil)
  (setq *oldCmdDia* nil)
  (setq *oldOsmodeZ* nil)

) ; кінець defun c:DrawSwitchAxisPro

;; ============================================================
;; == Повідомлення про завантаження ==
;; ============================================================
(princ "\nLISP 'DrawSwitchAxisPro' завантажено.")
(princ "\nДля запуску введіть DrawSwitchAxisPro.")
(princ)