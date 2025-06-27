;; ============================================================
;; == Скрипт для побудови осі стрілочного переводу (марка 1/9 або 1/11) ==
;; == Призначення:
;; ==   1. Користувач обирає 5 блоків-точок геодезичної зйомки:
;; ==      - P1: Стик рамної рейки
;; ==      - P2: Початок вістря
;; ==      - P4: Хвіст хрестовини по прямому напрямку
;; ==      - P3: Центр хрестовини
;; ==      - P5: Хвіст хрестовини по відгалуженню
;; ==   2. АВТОМАТИЧНО визначає марку хрестовини (1/9 або 1/11)
;; ==      за фактичним кутом між напрямками P3-P4 та P3-P5.
;; ==   3. Будує пряму полілінію (P1-P2_proj-P4).
;; ==   4. Визначає проекцію P3 на пряму вісь.
;; ==   5. Розраховує Центр Стрілочного Переводу (ЦСП) на прямій осі.
;; ==   6. Будує полілінію відгалуження від ЦСП з кутом,
;; ==      який відповідає визначеній марці.
;; ==   7. Проектує оригінальний блок P2 на пряму вісь та переміщує його, зберігаючи Z.
;; ==   8. Обрізає/продовжує пряму вісь, додаючи спроектовану точку P2 як вершину.
;; ==   9. Проектує оригінальний блок P5 на створену вісь відгалуження та переміщує його, зберігаючи Z.
;; ==   10. Обрізає/продовжує вісь відгалуження до спроектованої точки P5.
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
(defun unit_vector (vec)
  (setq len (distance '(0 0 0) vec))
  (if (and (numberp len) (> len 0.00000001))
    (mapcar '/ vec (list len len (if (caddr vec) len 1.0)))
    '(0.0 0.0 0.0)
  )
)

;; Допоміжна функція для скалярного добутку векторів
(defun dot_product (v1 v2)
  (apply '+ (mapcar '* v1 v2))
)

;; Допоміжна функція для векторного добутку (cross product) - для визначення ліво/право
(defun cross_product (v1 v2)
  (setq v1 (if (= (length v1) 2) (append v1 '(0.0)) v1)) ; Перетворюємо 2D в 3D
  (setq v2 (if (= (length v2) 2) (append v2 '(0.0)) v2)) ; Перетворюємо 2D в 3D
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
          (setq insertion_pt (cdr (assoc 10 ent_list))) ; Отримуємо точку вставки з DXF-коду 10
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

;; ============================================================
;; == ОСНОВНА ФУНКЦІЯ СКРИПТА ==
;; ============================================================
(defun c:DrawSwitchAxisPro ( / p1_data p2_data p4_data p3_data p5_data
                                 p1_coords p2_coords p4_coords p3_coords p5_coords
                                 straight_axis_obj
                                 proj_pt_p3 proj_pt_p2 csp_pt branch_angle branch_end_pt
                                 branch_axis_obj p2_projected_on_straight p5_projected_on_branch
                                 p2_block_vla p5_block_vla
                                 final_p2_target_pt final_p5_target_pt
                                 temp_straight_axis_ent temp_branch_axis_ent
                                 vec_p3_p4 vec_p3_p5 actual_branch_angle_rad actual_branch_angle_deg
                                 mark_1_9_angle_deg mark_1_11_angle_deg
                                 mark_1_9_dist_to_csp mark_1_11_dist_to_csp
                                 determined_mark
                                 dist_to_csp branch_angle_deg ) ; <--- Змінні для динамічних параметрів
  
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

  ;; 1. Запит блоків у користувача та отримання їхніх координат і VLA-об'єктів
  (setq p1_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки стику рамної рейки (P1): "))
  (if (not p1_data) (*error* "Вибір P1 скасовано."))
  (setq p1_coords (car p1_data))

  (setq p2_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки початку вістря (P2): "))
  (if (not p2_data) (*error* "Вибір P2 скасовано."))
  (setq p2_coords (car p2_data))
  (setq p2_block_vla (cdr p2_data))

  (setq p4_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по прямому напрямку (P4): "))
  (if (not p4_data) (*error* "Вибір P4 скасовано."))
  (setq p4_coords (car p4_data))

  (setq p3_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки центру хрестовини (P3): "))
  (if (not p3_data) (*error* "Вибір P3 скасовано."))
  (setq p3_coords (car p3_data))

  (setq p5_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по відгалуженню (P5): "))
  (if (not p5_data) (*error* "Вибір P5 скасовано."))
  (setq p5_coords (car p5_data))
  (setq p5_block_vla (cdr p5_data))

  ;; ===================================================================
  ;; == АВТОМАТИЧНЕ ВИЗНАЧЕННЯ МАРКИ ХРЕСТОВИНИ ==
  ;; ===================================================================
  (princ "\nВизначення марки хрестовини за фактичним кутом...")
  
  ;; Визначаємо вектори від центру хрестовини (P3)
  (setq vec_p3_p4 (mapcar '- p4_coords p3_coords)) ; Вектор P3 -> P4
  (setq vec_p3_p5 (mapcar '- p5_coords p3_coords)) ; Вектор P3 -> P5

  ;; Обчислюємо довжини векторів
  (setq len_p3_p4 (distance '(0 0 0) vec_p3_p4))
  (setq len_p3_p5 (distance '(0 0 0) vec_p3_p5))

  ;; Обчислюємо скалярний добуток
  (setq dot_prod (dot_product vec_p3_p4 vec_p3_p5))

  ;; Обчислюємо кут між векторами (в радіанах)
  ;; Захист від ділення на нуль, якщо якісь точки збігаються
  (if (and (> len_p3_p4 0.000001) (> len_p3_p5 0.000001))
    (progn
      (setq cos_angle (/ dot_prod (* len_p3_p4 len_p3_p5)))
      ;; Обмеження cos_angle до діапазону [-1, 1] через можливі похибки плаваючої точки
      (if (> cos_angle 1.0) (setq cos_angle 1.0))
      (if (< cos_angle -1.0) (setq cos_angle -1.0))
      (setq actual_branch_angle_rad (acos cos_angle))
      (setq actual_branch_angle_deg (rtd actual_branch_angle_rad))
      (princ (strcat "\nФактичний кут відгалуження (градуси): " (rtos actual_branch_angle_deg 2 8)))

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
    )
    (progn
      (princ "\nПомилка: Точки P3, P4 або P5 збігаються. Неможливо визначити марку.")
      (*error* "Неможливо визначити марку хрестовини.")
    )
  )
  ;; ===================================================================
  ;; == КІНЕЦЬ АВТОМАТИЧНОГО ВИЗНАЧЕННЯ МАРКИ ==
  ;; ===================================================================

  ;; 2. Побудова початкової прямої полілінії між P1 і P4 (тимчасова для проекції P2 і P3)
  (command "_.PLINE" p1_coords p4_coords "")
  (setq straight_axis_obj (vlax-ename->vla-object (entlast)))
  (setq temp_straight_axis_ent (entlast))

  ;; 3. Проектування P2 та P3 на тимчасову пряму вісь
  (setq p2_projected_on_straight (vlax-curve-getClosestPointTo straight_axis_obj p2_coords))
  (setq proj_pt_p3 (vlax-curve-getClosestPointTo straight_axis_obj p3_coords))

  ;; 4. Розрахунок Центру Стрілочного Переводу (ЦСП) на основі спроектованої P3
  ;;    dist_to_csp вже визначено вище
  (setq vec_p1_proj (mapcar '- p1_coords proj_pt_p3))
  (setq vec_p1_proj_unit (unit_vector vec_p1_proj))
  (setq csp_pt (mapcar '+ proj_pt_p3 (mapcar '* vec_p1_proj_unit (list dist_to_csp dist_to_csp 0.0))))

  ;; --- Переміщення блоку P2 на спроектовану точку (ЗБЕРІГАЮЧИ Z!) ---
  (if p2_block_vla
    (progn
      (setq final_p2_target_pt (list (car p2_projected_on_straight)
                                     (cadr p2_projected_on_straight)
                                     (caddr p2_coords)))
      
      (command "_move" (vlax-vla-object->ename p2_block_vla) "" 
               "_none" p2_coords
               "_none" final_p2_target_pt)
      (princ "\nБлок P2 переміщено на спроектовану точку, ЗБЕРІГАЮЧИ оригінальну Z-координату.")
    )
    (princ "\nПомилка: Не вдалося перемістити блок P2, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини ПРЯМОЇ осі (P1 - P2_proj - P4) ---
  (if temp_straight_axis_ent
    (progn
      (vla-delete (vlax-ename->vla-object temp_straight_axis_ent))
      (setq straight_axis_obj nil)
      (princ "\nТимчасова пряма вісь видалена.")
      
      (setq p2_proj_for_pline (list (car p2_projected_on_straight)
                                    (cadr p2_projected_on_straight)
                                    (caddr p2_coords)))
      
      (command "_.PLINE" p1_coords p2_proj_for_pline p4_coords "")
      (setq straight_axis_obj (vlax-ename->vla-object (entlast)))
      (princ "\nНова пряма вісь (P1-P2_proj-P4) створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити пряму вісь.")
  )

  ;; 5. Визначення напрямку відгалуження (вліво/вправо)
  ;;    vec_line вже визначений вище як (P4-P1)
  (setq vec_line (mapcar '- p4_coords p1_coords)) ; Перевизначаємо на випадок, якщо пряма вісь була змінена
  (setq vec_test (mapcar '- p5_coords p1_coords))
  (setq cross_z (caddr (cross_product vec_line vec_test)))
  (setq is_left (if (> cross_z 0) T nil))

  ;; 6. Побудова осі відгалуження від ЦСП (початкова, тимчасова)
  (setq branch_length 20.0) ; Початкова довжина
  ;;    branch_angle_deg вже визначено вище
  (setq branch_angle_rad (dtr branch_angle_deg))
  (setq straight_line_angle (angle p1_coords p4_coords)) ; Кут прямої осі (тепер це нова полілінія P1-P2_proj-P4)

  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad))
    (setq final_branch_angle (- straight_line_angle branch_angle_rad))
  )

  (setq temp_branch_end_pt (polar csp_pt final_branch_angle branch_length))
  (command "_.PLINE" csp_pt temp_branch_end_pt "")
  (setq temp_branch_axis_ent (entlast))

  ;; --- Проектування P5 на вісь відгалуження ---
  (setq branch_axis_obj (vlax-ename->vla-object temp_branch_axis_ent))
  (setq p5_projected_on_branch (vlax-curve-getClosestPointTo branch_axis_obj p5_coords))

  ;; --- Переміщення блоку P5 на спроектовану точку (ЗБЕРІГАЮЧИ Z!) ---
  (if p5_block_vla
    (progn
      (setq final_p5_target_pt (list (car p5_projected_on_branch)
                                     (cadr p5_projected_on_branch)
                                     (caddr p5_coords)))
      
      (command "_move" (vlax-vla-object->ename p5_block_vla) "" 
               "_none" p5_coords
               "_none" final_p5_target_pt)
      (princ "\nБлок P5 переміщено на спроектовану точку, ЗБЕРІГАЮЧИ оригінальну Z-координату.")
    )
    (princ "\nПомилка: Не вдалося перемістити блок P5, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини осі відгалуження (надійний підхід) ---
  (if (and temp_branch_axis_ent branch_axis_obj)
    (progn
      (vla-delete branch_axis_obj)
      (setq branch_axis_obj nil)
      (princ "\nТимчасова вісь відгалуження видалена.")

      (setq p5_proj_for_pline_branch (list (car p5_projected_on_branch) (cadr p5_projected_on_branch) (caddr csp_pt)))
      
      (command "_.PLINE" csp_pt p5_proj_for_pline_branch "")
      (setq branch_axis_obj (vlax-ename->vla-object (entlast)))
      (princ "\nНова вісь відгалуження від ЦСП до спроектованої P5 створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити вісь відгалуження.")
  )

  (princ "\n--- Осі стрілочного переводу побудовано, блоки переміщено та осі скориговано! ---")
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