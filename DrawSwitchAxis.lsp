;; ============================================================
;; == Скрипт для побудови осі стрілочного переводу (марка 1/9 або 1/11) ==
;; == З РОЗШИРЕНИМ ДЕБАГ-ЛОГУВАННЯМ У ФАЙЛ ==
;; == МАРКА ВИЗНАЧАЄТЬСЯ ЗА ЕТАЛОННОЮ ДОВЖИНОЮ ВІДРІЗКА P2-P4 ==
;; == ДОДАНО ДЕБАГ ДИСТАНЦІЇ proj_pt_p3 до csp_pt ==
;; ============================================================

(vl-load-com)

(setq *oldEcho* nil)
(setq *oldOsmode* nil)
(setq *oldCmdDia* nil)
(setq *oldOsmodeZ* nil)

(defun *error* (msg)
  (if *oldEcho* (setvar "CMDECHO" *oldEcho*))
  (if *oldOsmode* (setvar "OSMODE" *oldOsmode*))
  (if *oldCmdDia* (setvar "CMDDIA" *oldCmdDia*))
  (if *oldOsmodeZ* (setvar "OSNAPZ" *oldOsmodeZ*))
  (sssetfirst nil nil)
  (princ "\n*** Помилка LISP або скасовано: ")
  (if msg (princ msg))
  (princ " ***")
  (princ)
)

(defun unit_vector (vec)
  (setq len (distance (apply 'list (mapcar (function (lambda (x) 0.0)) vec)) vec))
  (if (and (numberp len) (> len 0.000001))
    (mapcar '/ vec (list len len (if (= (length vec) 3) len 1.0)))
    (apply 'list (mapcar (function (lambda (x) 0.0)) vec))
  )
)

(defun dot_product (v1 v2)
  (apply '+ (mapcar '* v1 v2))
)

(defun cross_product (v1 v2)
  (setq v1 (if (= (length v1) 2) (append v1 '(0.0)) v1))
  (setq v2 (if (= (length v2) 2) (append v2 '(0.0)) v2))
  (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
        (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
        (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
)

(defun dtr (a) (* pi (/ a 180.0)))
(defun rtd (a) (* 180.0 (/ a pi)))

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
          (setq insertion_pt (cdr (assoc 10 ent_list)))
          (princ (strcat "\nОбрано блок. Точка: " (vl-princ-to-string insertion_pt)))
          (cons insertion_pt vla_obj)
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
                                 etalon_p2_p4_1_9 etalon_p2_p4_1_11 actual_p2_p4_length )

  (setq *oldEcho* (getvar "CMDECHO"))
  (setq *oldOsmode* (getvar "OSMODE"))
  (setq *oldCmdDia* (getvar "CMDDIA"))
  (setq *oldOsmodeZ* (getvar "OSNAPZ"))
  
  (setvar "CMDECHO" 0)
  (setvar "CMDDIA" 0)
  (setvar "OSNAPZ" 0)

  (princ "\n--- Побудова осі стрілочного переводу (Авто-визначення марки) ---")

  (setq debug_file_path (strcat (getenv "TEMP") "\\SwitchAxisDebug.txt"))
  (setq debug_file (open debug_file_path "w"))
  (if debug_file
      (princ (strcat "\nДебаг-лог буде збережено у файлі: " debug_file_path))
      (princ "\nERROR: Could not open debug file for writing. Check path/permissions. Tried to write to TEMP directory.")
  )
  (if debug_file (write-line (strcat "--- DEBUG LOG --- " (rtos (getvar "CDATE") 2 8)) debug_file))
  (if debug_file (write-line "" debug_file))

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

  (setq p1_2d_coords (list (car p1_orig_coords) (cadr p1_orig_coords) 0.0))
  (setq p2_2d_coords (list (car p2_orig_coords) (cadr p2_orig_coords) 0.0))
  (setq p3_2d_coords (list (car p3_orig_coords) (cadr p3_orig_coords) 0.0))
  (setq p4_2d_coords (list (car p4_orig_coords) (cadr p4_orig_coords) 0.0))
  (setq p5_2d_coords (list (car p5_orig_coords) (cadr p5_orig_coords) 0.0))
  
  (princ (strcat "\nDBG: P3_2D: " (vl-princ-to-string p3_2d_coords)))
  (princ (strcat "\nDBG: P4_2D: " (vl-princ-to-string p4_2d_coords)))
  (princ (strcat "\nDBG: P5_2D: " (vl-princ-to-string p5_2d_coords)))

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

  (princ "\nВизначення марки хрестовини за еталонною довжиною P2-P4...")

  (setq etalon_p2_p4_1_9 28.270)
  (setq etalon_p2_p4_1_11 30.598)

  (setq mark_1_9_angle_deg 6.340277778)
  (setq mark_1_11_angle_deg 5.194444444)

  (setq mark_1_9_dist_to_csp 13.68)
  (setq mark_1_11_dist_to_csp 16.72)

  (setq actual_p2_p4_length (distance p2_2d_coords p4_2d_coords))
  (princ (strcat "\nФактична довжина P2-P4: " (rtos actual_p2_p4_length 2 8) " м"))

  (setq diff_to_1_9 (abs (- actual_p2_p4_length etalon_p2_p4_1_9)))
  (setq diff_to_1_11 (abs (- actual_p2_p4_length etalon_p2_p4_1_11)))

  (if (< diff_to_1_9 diff_to_1_11)
    (setq determined_mark "1/9")
    (setq determined_mark "1/11")
  )
  (princ (strcat "\nАвтоматично визначена марка хрестовини: " determined_mark))

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

  (command "_.PLINE" p1_2d_coords p4_2d_coords "")
  (setq straight_axis_obj (vlax-ename->vla-object (entlast)))
  (setq temp_straight_axis_ent (entlast))

  (setq proj_pt_p3 (vlax-curve-getClosestPointTo straight_axis_obj p3_2d_coords))

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
          (write-line (strcat "ACTUAL MEASURED DISTANCE from proj_pt_p3 to csp_pt (by script): " (rtos (distance proj_pt_p3 csp_pt) 2 15)) debug_file) ; <--- КЛЮЧОВИЙ РЯДОК ДЕБАГУ
          (write-line "" debug_file)
      )
  )
  ;; --- END DEBUG FILE LOGGING (NEW) ---

  (setq proj_pt_p2 (vlax-curve-getClosestPointTo straight_axis_obj p2_2d_coords))

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

  (if temp_straight_axis_ent
    (progn
      (vla-delete (vlax-ename->vla-object temp_straight_axis_ent))
      (setq straight_axis_obj nil)
      (princ "\nТимчасова пряма вісь видалена (для перетворення в P1-P2_proj-P4).")
      
      (setq p2_proj_for_pline (list (car proj_pt_p2)
                                    (cadr proj_pt_p2)
                                    0.0))
      
      (command "_.PLINE" p1_2d_coords p2_proj_for_pline p4_2d_coords "")
      (setq straight_axis_obj (vlax-ename->vla-object (entlast)))
      (princ "\nНова пряма вісь (P1-P2_proj-P4) створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити пряму вісь.")
  )

  (setq vec_line (mapcar '- p4_2d_coords p1_2d_coords))
  (setq vec_test (mapcar '- p5_2d_coords p1_2d_coords))
  (setq cross_z (caddr (cross_product vec_line vec_test)))
  (setq is_left (if (> cross_z 0) T nil))

  (setq branch_length 20.0)
  (setq branch_angle_rad (dtr branch_angle_deg))
  (setq straight_line_angle (angle p1_2d_coords p4_2d_coords))

  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad))
    (setq final_branch_angle (- straight_line_angle branch_angle_rad))
  )

  (setq temp_branch_end_pt (polar csp_pt final_branch_angle branch_length))
  (command "_.PLINE" csp_pt temp_branch_end_pt "")
  (setq temp_branch_axis_ent (entlast))

  (setq branch_axis_obj (vlax-ename->vla-object temp_branch_axis_ent))
  (setq p5_projected_on_branch (vlax-curve-getClosestPointTo branch_axis_obj p5_2d_coords))

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

  (if (and temp_branch_axis_ent branch_axis_obj)
    (progn
      (vla-delete branch_axis_obj)
      (setq branch_axis_obj nil)
      (princ "\nТимчасова вісь відгалуження видалена.")

      (setq p5_proj_for_pline_branch (list (car p5_projected_on_branch) (cadr p5_projected_on_branch) 0.0))
      
      (command "_.PLINE" csp_pt p5_proj_for_pline_branch "")
      (setq branch_axis_obj (vlax-ename->vla-object (entlast)))
      (princ "\nНова вісь відгалуження від ЦСП до спроектованої P5 створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити вісь відгалуження.")
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