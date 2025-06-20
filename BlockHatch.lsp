;======================================================================
;== Скрипт для "штрихування" замкнутої полілінії блоками          ==
;== в шаховому порядку.                                            ==
;== ВЕРСІЯ 3: З виправленням системи координат (UCS/WCS).            ==
;== Команда для запуску: BLOCKHATCH                                ==
;======================================================================

;; --- Допоміжна функція: Перевіряє, чи точка знаходиться всередині контуру ---
;; Вона незмінна, оскільки працює з координатами, які їй передають.
(defun IsPointInside (pt pline_vla / ray_obj int_points int_count error_or_result mspace ray_start ray_end)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
  (setq ray_start (vlax-3d-point pt))
  (setq ray_end (vlax-3d-point (list (+ (car pt) 1.0e9) (cadr pt) 0.0))) 
  
  (setq ray_obj (vla-addLine mspace ray_start ray_end))
  
  (setq error_or_result (vl-catch-all-apply
    'vla-intersectwith
    (list pline_vla ray_obj acExtendNone)
  ))
  
  (vla-delete ray_obj)
  
  (if (vl-catch-all-error-p error_or_result)
    (setq int_count 0)
    (progn
      (setq int_points error_or_result)
      (setq int_count (/ (1+ (vlax-safearray-get-u-bound (vlax-variant-value int_points) 1)) 3))
    )
  )
  
  (if (= 1 (rem int_count 2)) T nil) 
)

;; --- Основна функція ---
(defun c:BLOCKHATCH (/ block_ent block_name pline_ent pline_vla h_space v_space min_pt_ucs max_pt_ucs min_pt_wcs max_pt_wcs min_x min_y max_x max_y cur_x cur_y row_idx is_odd_row offset block_count mspace test_point_wcs)
  
  (vl-load-com)
  (princ "\nЗапускаємо штриховку блоками (версія з корекцією UCS)...")

  ;; --- Крок 1: Отримати вхідні дані від користувача ---
  (setq block_ent (entsel "\nКлікни на блок, яким будемо заповнювати: "))
  (if (and block_ent (= "INSERT" (cdr (assoc 0 (entget (car block_ent))))))
    (progn
      (setq block_name (vla-get-effectivename (vlax-ename->vla-object (car block_ent))))
      
      (setq pline_ent (entsel "\nТепер вибери замкнуту полілінію-контур: "))
      (if (and pline_ent (wcmatch (cdr (assoc 0 (entget (car pline_ent)))) "*POLYLINE") (= 1 (logand 1 (cdr (assoc 70 (entget (car pline_ent)))))))
        (progn
          (setq pline_vla (vlax-ename->vla-object (car pline_ent)))
          
          (setq h_space (getdist "\nВведи відстань між центрами блоків по горизонталі (X): "))
          (setq v_space (getdist "\nВведи відстань між рядами по вертикалі (Y): "))
          
          (if (and (> h_space 1e-6) (> v_space 1e-6))
            (progn
              ;; --- Крок 2: Розрахунок та розміщення блоків ---
              
              ; Отримуємо габаритний контейнер. Координати повертаються в активній UCS!
              (vla-getboundingbox pline_vla 'min_pt_ucs 'max_pt_ucs)
              
              ; ВИПРАВЛЕНО: Трансформуємо координати з поточної UCS (код 1) у світову WCS (код 0).
              (setq min_pt_wcs (trans (vlax-safearray->list min_pt_ucs) 1 0)
                    max_pt_wcs (trans (vlax-safearray->list max_pt_ucs) 1 0))
              
              ; Тепер всі розрахунки ведемо у світових координатах (WCS).
              (setq min_x (car min_pt_wcs)
                    min_y (cadr min_pt_wcs)
                    max_x (car max_pt_wcs)
                    max_y (cadr max_pt_wcs))
              
              (setq cur_y min_y
                    row_idx 0
                    block_count 0
                    mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
              
              (princ "\nВиконую розрахунки... Це може зайняти деякий час.")
              (while (<= cur_y max_y)
                (setq is_odd_row (= 1 (rem row_idx 2)))
                (setq offset (if is_odd_row (/ h_space 2.0) 0))
                (setq cur_x (+ min_x offset))
                
                (while (<= cur_x max_x)
                  (setq test_point_wcs (list cur_x cur_y 0.0))
                  (if (IsPointInside test_point_wcs pline_vla)
                    (progn
                      ; Вставляємо блок, використовуючи точку в світових координатах (WCS)
                      (vla-insertblock mspace (vlax-3d-point test_point_wcs) block_name 1.0 1.0 1.0 0)
                      (setq block_count (1+ block_count))
                    )
                  )
                  (setq cur_x (+ cur_x h_space))
                )
                (setq cur_y (+ cur_y v_space)
                      row_idx (1+ row_idx))
              )
              (princ (strcat "\nГотово! Вставлено " (itoa block_count) " блоків."))
            )
            (princ "\nПомилка: відстані мають бути більшими за нуль.")
          )
        )
        (princ "\nПомилка: це не замкнута полілінія! Будь ласка, вибери правильний об'єкт.")
      )
    )
    (princ "\nПомилка: це не блок! Будь ласка, вибери блок для заповнення.")
  )
  (princ)
)