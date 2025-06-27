;; ============================================================
;; == Скрипт для побудови осі стрілочного переводу (марка 1/9 або 1/11) ==
;; == ... (попередній опис) ... ==
;; == ВИПРАВЛЕНО: КОМАНДА ЗАЛИВКИ (HATCH) ==
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
                                 p1_orig_coords p2_orig_coords p4_orig_coords p3_orig_coords p5_orig_coords
                                 p1_2d_coords p2_2d_coords p4_2d_coords p3_2d_coords p5_2d_coords
                                 straight_axis_obj
                                 proj_pt_p3 proj_pt_p2 csp_pt branch_angle branch_end_pt
                                 branch_axis_obj p2_projected_on_straight p5_projected_on_branch
                                 p2_block_vla p5_block_vla
                                 final_p2_target_pt final_p5_target_pt
                                 temp_straight_axis_ent temp_branch_axis_ent
                                 mark_1_9_angle_deg mark_1_11_angle_deg
                                 mark_1_9_dist_to_csp mark_1_11_dist_to_csp
                                 determined_mark
                                 dist_to_csp branch_angle_deg
                                 debug_file debug_file_path
                                 etalon_p2_p4_1_9 etalon_p2_p4_1_11 actual_p2_p4_length
                                 csp_marker_length csp_marker_lineweight
                                 straight_axis_main_angle perp_angle csp_marker_pt1 csp_marker_pt2 csp_marker_obj
                                 contour_length_along_straight contour_pt2 contour_pt3 contour_polyline_ent contour_polyline_obj hatch_ent hatch_obj )

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

  ;; --- START DEBUG FILE LOGGING ---
  (setq debug_file_path (strcat (getenv "TEMP") "\\SwitchAxisDebug.txt"))
  (setq debug_file (open debug_file_path "w"))
  (if debug_file
      (princ (strcat "\nДебаг-лог буде збережено у файлі: " debug_file_path))
      (princ "\nERROR: Could not open debug file for writing. Check path/permissions. Tried to write to TEMP directory.")
  )
  (if debug_file
      (progn
          (write-line (strcat "--- DEBUG LOG --- " (rtos (getvar "CDATE") 2 8)) debug_file)
          (write-line "" debug_file)
      )
  )
  ;; --- END DEBUG FILE LOGGING ---


  ;; 1. Запит блоків у користувача та отримання їхніх оригінальних 3D координат і VLA-об'єктів
  (setq p1_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки стику рамної рейки (P1): "))
  (if (not p1_data) (progn (*error* "Вибір P1 скасовано.") (exit)))
  (setq p1_orig_coords (car p1_data))

  (setq p2_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки початку вістря (P2): "))
  (if (not p2_data) (progn (*error* "Вибір P2 скасовано.") (exit)))
  (setq p2_orig_coords (car p2_data))
  (setq p2_block_vla (cdr p2_data))

  (setq p4_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по прямому напрямку (P4): "))
  (if (not p4_data) (progn (*error* "Вибір P4 скасовано.") (exit)))
  (setq p4_orig_coords (car p4_data))

  (setq p3_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки центру хрестовини (P3): "))
  (if (not p3_data) (progn (*error* "Вибір P3 скасовано.") (exit)))
  (setq p3_orig_coords (car p3_data))

  (setq p5_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по відгалуженню (P5): "))
  (if (not p5_data) (progn (*error* "Вибір P5 скасовано.") (exit)))
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

  ;; --- DEBUG FILE LOGGING ---
  (if debug_file
      (progn
          (write-line "--- Original 3D Coords (from selected blocks) ---" debug_file)
          (write-line (strcat "P1_orig_coords: " (rtos (car p1_orig_coords) 2 15) ", " (rtos (cadr p1_orig_coords) 2 15) ", " (rtos (caddr p1_orig_coords) 2 15)) debug_file)
          (write-line (strcat "P2_orig_coords: " (rtos (car p2_orig_coords) 2 15) ", " (rtos (cadr p2_orig_coords) 2 15) ", " (rtos (caddr p2_orig_coords) 2 15)) debug_file)
          (write-line (strcat "P3_orig_coords: " (rtos (car p3_orig_coords) 2 15) ", " (rtos (cadr p3_orig_coords) 2 15) ", " (rtos (caddr p3_orig_coords) 2 15)) debug_file)
          (write-line (strcat "P4_orig_coords: " (rtos (car p4_orig_coords) 2 15) ", " (rtos (cadr p4_orig_coords) 2 15) ", " (rtos (caddr p4_orig_coords) 2 15)) debug_file)
          (write-line (strcat "P5_orig_coords: " (rtos (car p5_orig_coords) 2 15) ", " (rtos (cadr p5_orig_coords) 2 15) ", " (rtos (caddr p5_orig_coords) 2 15)) debug_file)
          (write-line "" debug_file)
          (write-line "--- 2D Coords (Z=0) for calculations ---" debug_file)
          (write-line (strcat "P1_2d_coords: " (rtos (car p1_2d_coords) 2 15) ", " (rtos (cadr p1_2d_coords) 2 15) ", " (rtos (caddr p1_2d_coords) 2 15)) debug_file)
          (write-line (strcat "P2_2d_coords: " (rtos (car p2_2d_coords) 2 15) ", " (rtos (cadr p2_2d_coords) 2 15) ", " (rtos (caddr p2_2d_coords) 2 15)) debug_file)
          (write-line (strcat "P3_2d_coords: " (rtos (car p3_2d_coords) 2 15) ", " (rtos (cadr p3_2d_coords) 2 15) ", " (rtos (caddr p3_2d_coords) 2 15)) debug_file)
          (write-line (strcat "P4_2d_coords: " (rtos (car p4_2d_coords) 2 15) ", " (rtos (cadr p4_2d_coords) 2 15) ", " (rtos (caddr p4_2d_coords) 2 15)) debug_file)
          (write-line (strcat "P5_2d_coords: " (rtos (car p5_2d_coords) 2 15) ", " (rtos (cadr p5_2d_coords) 2 15) ", " (rtos (caddr p5_2d_coords) 2 15)) debug_file)
          (write-line "" debug_file)
      )
  )
  ;; --- END DEBUG FILE LOGGING ---


  ;; ===================================================================
  ;; == АВТОМАТИЧНЕ ВИЗНАЧЕННЯ МАРКИ ХРЕСТОВИНИ (ЗА ДОВЖИНОЮ P2-P4!) ==
  ;; ===================================================================
  (princ "\nВизначення марки хрестовини за еталонною довжиною P2-P4...")

  ;; Еталонні довжини P2-P4, надані користувачем (у метрах)
  (setq etalon_p2_p4_1_9 28.270)
  (setq etalon_p2_p4_1_11 30.598)

  ;; Еталонні кути для подальшої побудови (в градусах)
  (setq mark_1_9_angle_deg 6.340277778)   ; 6°20'25"
  (setq mark_1_11_angle_deg 5.194444444) ; 5°11'40"

  ;; Параметри dist_to_csp для подальшої побудови (у метрах)
  (setq mark_1_9_dist_to_csp 13.68)
  (setq mark_1_11_dist_to_csp 16.72)

  ;; Обчислюємо фактичну довжину P2-P4 в 2D-площині
  (setq actual_p2_p4_length (distance p2_2d_coords p4_2d_coords))
  (princ (strcat "\nФактична довжина P2-P4: " (rtos actual_p2_p4_length 2 8) " м"))

  ;; Визначаємо, яка еталонна довжина ближча
  (setq diff_to_1_9 (abs (- actual_p2_p4_length etalon_p2_p4_1_9)))
  (setq diff_to_1_11 (abs (- actual_p2_p4_length etalon_p2_p4_1_11)))

  (if (< diff_to_1_9 diff_to_1_11)
    (setq determined_mark "1/9")
    (setq determined_mark "1/11") ; За замовчуванням, якщо 11 ближче або однаково
  )
  (princ (strcat "\nАвтоматично визначена марка хрестовини: " determined_mark))

  ;; Встановлюємо параметри dist_to_csp та branch_angle_deg відповідно до визначеної марки
  (cond
    ((equal determined_mark "1/9")
     (setq dist_to_csp mark_1_9_dist_to_csp
           branch_angle_deg mark_1_9_angle_deg)
    )
    ((equal determined_mark "1/11")
     (setq dist_to_csp mark_1_11_dist_to_csp
           branch_angle_deg mark_1_11_angle_deg)
    )
  )

  ;; --- DEBUG FILE LOGGING ---
  (if debug_file
      (progn
          (write-line "--- Mark Determination Details (Length-based) ---" debug_file)
          (write-line (strcat "Actual P2-P4 Length: " (rtos actual_p2_p4_length 2 15) " m") debug_file)
          (write-line (strcat "Etalon 1/9 P2-P4 Length: " (rtos etalon_p2_p4_1_9 2 15) " m (Diff: " (rtos diff_to_1_9 2 15) ")") debug_file)
          (write-line (strcat "Etalon 1/11 P2-P4 Length: " (rtos etalon_p2_p4_1_11 2 15) " m (Diff: " (rtos diff_to_1_11 2 15) ")") debug_file)
          (write-line (strcat "Determined Mark: " determined_mark) debug_file)
          (write-line "" debug_file)
      )
  )
  ;; --- END DEBUG FILE LOGGING ---


  ;; === Побудова тимчасової прямої полілінії P1-P4 для проекцій (видаляємо її потім) ===
  ;; Це потрібно, щоб отримати proj_pt_p3 та proj_pt_p2
  (command "_.PLINE" p1_2d_coords p4_2d_coords "")
  (setq straight_axis_obj (vlax-ename->vla-object (entlast)))
  (setq temp_straight_axis_ent (entlast))


  ;; === Розрахунок проекції P3 на пряму вісь P1-P4 (для визначення ЦСП) ===
  (setq proj_pt_p3 (vlax-curve-getClosestPointTo straight_axis_obj p3_2d_coords))
  (setq proj_pt_p3 (list (car proj_pt_p3) (cadr proj_pt_p3) 0.0)) ; <--- ПРИМУСОВО ВСТАНОВЛЮЄМО Z=0.0
  

  ;; === Обчислюємо остаточну точку ЦСП, використовуючи визначену марку ===
  (setq vec_p1_proj_final (mapcar '- p1_2d_coords proj_pt_p3))
  (setq vec_p1_proj_unit_final (unit_vector vec_p1_proj_final))
  (setq csp_pt (mapcar '+ proj_pt_p3 (mapcar '* vec_p1_proj_unit_final (list dist_to_csp dist_to_csp 0.0))))
  
  ;; --- DEBUG FILE LOGGING (NEW) ---
  (if debug_file
      (progn
          (write-line "--- CSP Calculation Details ---" debug_file)
          (write-line (strcat "proj_pt_p3: " (rtos (car proj_pt_p3) 2 15) ", " (rtos (cadr proj_pt_p3) 2 15) ", " (rtos (caddr proj_pt_p3) 2 15)) debug_file)
          (write-line (strcat "vec_p1_proj_unit_final: " (rtos (car vec_p1_proj_unit_final) 2 15) ", " (rtos (cadr vec_p1_proj_unit_final) 2 15) ", " (rtos (caddr vec_p1_proj_unit_final) 2 15)) debug_file)
          (write-line (strcat "Desired dist_to_csp (used in calc): " (rtos dist_to_csp 2 15)) debug_file)
          (write-line (strcat "csp_pt (calculated): " (rtos (car csp_pt) 2 15) ", " (rtos (cadr csp_pt) 2 15) ", " (rtos (caddr csp_pt) 2 15)) debug_file)
          (write-line (strcat "ACTUAL MEASURED DISTANCE from proj_pt_p3 to csp_pt (by script): " (rtos (distance proj_pt_p3 csp_pt) 2 15)) debug_file)
          (write-line "" debug_file)
      )
  )
  ;; --- END DEBUG FILE LOGGING (NEW) ---

  ;; === Проектування P2 на пряму вісь P1-P4 ===
  (setq proj_pt_p2 (vlax-curve-getClosestPointTo straight_axis_obj p2_2d_coords))
  (setq proj_pt_p2 (list (car proj_pt_p2) (cadr proj_pt_p2) 0.0)) ; <--- ПРИМУСОВО ВСТАНОВЛЮЄМО Z=0.0


  ;; --- Переміщення блоку P2 на спроектовану точку (ЗБЕРІГАЮЧИ ОРИГІНАЛЬНУ Z!) ---
  (if p2_block_vla
    (progn
      (setq final_p2_target_pt (list (car proj_pt_p2)
                                     (cadr proj_pt_p2)
                                     (caddr p2_orig_coords)))
      
      (command "_move" (vlax-vla-object->ename p2_block_vla) "" 
               "_none" p2_orig_coords
               "_none" final_p2_target_pt)
      (princ "\nБлок P2 переміщено на спроектовану точку, ЗБЕРІГАЮЧИ оригінальну Z-координату.")
    )
    (princ "\nПомилка: Не вдалося перемістити блок P2, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини ПРЯМОЇ осі (P1 - P2_proj - P4) ---
  (if temp_straight_axis_ent
    (progn
      (vla-delete (vlax-ename->vla-object temp_straight_axis_ent)) ; Видаляємо тимчасову P1-P4
      (setq straight_axis_obj nil)
      (princ "\nТимчасова пряма вісь видалена (для перетворення в P1-P2_proj-P4).")
      
      ;; Створюємо нову пряму полілінію P1 - P2_proj - P4 в 2D
      (setq p2_proj_for_pline (list (car proj_pt_p2)
                                    (cadr proj_pt_p2)
                                    0.0))
      
      (command "_.PLINE" p1_2d_coords p2_proj_for_pline p4_2d_coords "")
      (setq straight_axis_obj (vlax-ename->vla-object (entlast)))
      (vla-put-Lineweight straight_axis_obj 30) ; <--- Встановлюємо вагу лінії (0.30 мм)
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
  (setq straight_line_angle (angle p1_2d_coords p4_2d_coords))

  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad))
    (setq final_branch_angle (- straight_line_angle branch_angle_rad))
  )

  (setq temp_branch_end_pt (polar csp_pt final_branch_angle branch_length))
  (command "_.PLINE" csp_pt temp_branch_end_pt "")
  (setq temp_branch_axis_ent (entlast))

  ;; --- Проектування P5 на вісь відгалуження (в 2D) ---
  (setq branch_axis_obj (vlax-ename->vla-object temp_branch_axis_ent))
  (setq p5_projected_on_branch (vlax-curve-getClosestPointTo branch_axis_obj p5_2d_coords))

  ;; --- Переміщення блоку P5 на спроектовану точку (ЗБЕРІГАЮЧИ ОРИГІНАЛЬНУ Z!) ---
  (if p5_block_vla
    (progn
      (setq final_p5_target_pt (list (car p5_projected_on_branch)
                                     (cadr p5_projected_on_branch)
                                     (caddr p5_orig_coords)))
      
      (command "_move" (vlax-vla-object->ename p5_block_vla) "" 
               "_none" p5_orig_coords
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
      (setq p5_proj_for_pline_branch (list (car p5_projected_on_branch)
                                            (cadr p5_projected_on_branch)
                                            0.0))
      
      (command "_.PLINE" csp_pt p5_proj_for_pline_branch "")
      (setq branch_axis_obj (vlax-ename->vla-object (entlast)))
      (vla-put-Lineweight branch_axis_obj 30) ; <--- Встановлюємо вагу лінії (0.30 мм)
      (princ "\nНова вісь відгалуження від ЦСП до спроектованої P5 створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити вісь відгалуження.")
  )

  ;; --- Накреслити маркер ЦСП ---
  (setq csp_marker_length 0.76)
  (setq csp_marker_lineweight 30) ; Вага лінії 0.30 мм
  
  ;; Кут прямої осі (використовуємо P1_2d та P4_2d для загального напрямку)
  (setq straight_axis_main_angle (angle p1_2d_coords p4_2d_coords)) 
  (setq perp_angle (+ straight_axis_main_angle (/ pi 2.0))) ; Перпендикулярний кут

  ;; Розраховуємо кінці маркера ЦСП, центруючи його на csp_pt
  (setq csp_marker_pt1 (polar csp_pt perp_angle (/ csp_marker_length 2.0)))
  (setq csp_marker_pt2 (polar csp_pt (+ perp_angle pi) (/ csp_marker_length 2.0))) ; Йдемо в протилежному напрямку

  (command "_.PLINE" csp_marker_pt1 csp_marker_pt2 "")
  (setq csp_marker_obj (vlax-ename->vla-object (entlast)))
  (if csp_marker_obj
      (progn
          (vla-put-Lineweight csp_marker_obj csp_marker_lineweight) ; Встановлюємо вагу лінії
          (vla-put-color csp_marker_obj 256) ; Встановлюємо колір на ByLayer (видалено червоний)
          (princ (strcat "\nDBG: Маркер ЦСП (EntName: " (vl-princ-to-string (vlax-vla-object->ename csp_marker_obj)) ") створено."))
      )
      (princ "\nERROR: Маркер ЦСП не вдалося створити.")
  )
  (princ "\nМаркер ЦСП накреслено.") ; Це повідомлення про успіх, якщо маркер створено

  ;; --- Створення замкнутого контуру з чорною заливкою ---
  (princ "\nСтворення замкнутого контуру та заливка чорним...")

  ;; 1. Розраховуємо точки контуру
  (setq contour_length_along_straight 15.0) ; Довжина від ЦСП вздовж прямої осі
  
  ;; Точка 1: ЦСП (csp_pt) - вже є
  ;; Точка 2: 15м від ЦСП в напрямку P4
  (setq contour_pt2 (polar csp_pt (angle csp_pt p4_2d_coords) contour_length_along_straight))

  ;; Точка 3: На лінії ЦСП-P5, перпендикуляр від Точки 2
  ;; Проектуємо contour_pt2 на лінію, що проходить через csp_pt і p5_projected_on_branch
  (setq contour_pt3 (vlax-curve-getClosestPointTo branch_axis_obj contour_pt2)) ; branch_axis_obj - це полілінія від ЦСП до p5_projected_on_branch

  ;; 2. Створюємо замкнуту полілінію
  (command "_.PLINE" csp_pt contour_pt2 contour_pt3 "_C") ; Створюємо замкнуту полілінію
  (setq contour_polyline_ent (entlast)) ; Отримуємо ім'я створеної полілінії
  (setq contour_polyline_obj (vlax-ename->vla-object contour_polyline_ent))

  ;; 3. Заливаємо контур чорним кольором
  (if contour_polyline_obj
      (progn
          ;; !!! ВИПРАВЛЕНО КОМАНДУ HATCH !!!
          ;; Передаємо об'єкт полілінії напряму, без опцій "_S" "_A"
          ;; HATCH приймає: (pattern_name) (object_entname) (ENTER to finish selection) (ENTER to accept scale/angle)
          (command "_.HATCH" "SOLID" contour_polyline_ent "") ; Вибираємо створену полілінію як межу
          (setq hatch_ent (entlast)) ; Отримуємо ім'я створеної заливки
          (setq hatch_obj (vlax-ename->vla-object hatch_ent))

          (if hatch_obj
              (progn
                  (vla-put-Color hatch_obj 7) ; Встановлюємо колір на чорний/білий (ACLISP index 7)
                  (princ "\nЗамкнутий контур залито чорним.")
              )
              (princ "\nERROR: Не вдалося створити об'єкт заливки.")
          )
      )
      (princ "\nERROR: Не вдалося створити контурну полілінію для заливки.")
  )

  ;; 4. Видаляємо контурну полілінію (залишаємо тільки заливку)
  (if (and contour_polyline_obj (not (vlax-object-released-p contour_polyline_obj)))
      (progn
          (vla-delete contour_polyline_obj)
          (princ "\nКонтурні лінії видалено, залишено лише заливку.")
      )
      (princ "\nУвага: Контурну полілінію не знайдено для видалення.")
  )

  (princ (strcat "\n--- Осі стрілочного переводу марки " determined_mark " побудовано, блоки переміщено та осі скориговано! ---"))
  (princ "\nСкрипт завершено.")
  (princ)

  (if debug_file (close debug_file))

  (setq *oldEcho* nil)
  (setq *oldOsmode* nil)
  (setq *oldCmdDia* nil)
  (setq *oldOsmodeZ* nil)

)

(princ "\nLISP 'DrawSwitchAxisPro' завантажено.")
(princ "\nДля запуску введіть DrawSwitchAxisPro.")
(princ)