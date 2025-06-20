;======================================================================
;== Скрипт для "штрихування" замкнутої полілінії блоками          ==
;== в шаховому порядку.                                            ==
;== Команда для запуску: BLOCKHATCH                                ==
;======================================================================

;; --- Допоміжна функція: Перевіряє, чи точка знаходиться всередині контуру ---
;; Використовує метод "променю": пускає промінь з точки і рахує перетини.
;; Непарна кількість перетинів = точка всередині.
(defun IsPointInside (pt pline_vla / ray_obj int_points int_count error_catch)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
  (setq ray_start (vlax-3d-point pt))
  (setq ray_end (vlax-3d-point (list (+ (car pt) 1.0e9) (cadr pt) 0.0))) ; Промінь далеко вправо
  
  (setq ray_obj (vla-addLine mspace ray_start ray_end))
  
  (setq error_catch (vl-catch-all-apply
    'vla-intersectwith
    (list pline_vla ray_obj acExtendNone)
  ))
  
  (vla-delete ray_obj) ; Видаляємо тимчасовий промінь
  
  (if (vl-catch-all-error-p error_catch)
    (setq int_count 0) ; Якщо помилка (немає перетинів), то кількість = 0
    (progn
      (setq int_points (vl-catch-all-value error_catch))
      (setq int_count (/ (vlax-safearray-get-u-bound (vlax-variant-value int_points) 1) 3))
    )
  )
  
  (if (= 1 (rem int_count 2)) T nil) ; Якщо непарне число перетинів - повернути T (істина)
)

;; --- Основна функція ---
(defun c:BLOCKHATCH (/ block_ent block_name pline_ent pline_vla h_space v_space min_pt max_pt min_x min_y max_x max_y cur_x cur_y row_idx is_odd_row offset block_count)
  
  (vl-load-com)
  (princ "\n🚀 Запускаємо штриховку блоками...")

  ;; --- Крок 1: Отримати вхідні дані від користувача ---
  (setq block_ent (entsel "\n👉 Клікни на блок, яким будемо заповнювати: "))
  (if (and block_ent (= "INSERT" (cdr (assoc 0 (entget (car block_ent))))))
    (progn
      (setq block_name (vla-get-effectivename (vlax-ename->vla-object (car block_ent))))
      
      (setq pline_ent (entsel "\n👉 Тепер вибери замкнуту полілінію-контур: "))
      (if (and pline_ent (wcmatch (cdr (assoc 0 (entget (car pline_ent)))) "*POLYLINE") (= 1 (logand 1 (cdr (assoc 70 (entget (car pline_ent)))))))
        (progn
          (setq pline_vla (vlax-ename->vla-object (car pline_ent)))
          
          (setq h_space (getdist "\n↔️  Введи відстань між центрами блоків по горизонталі (X): "))
          (setq v_space (getdist "\n↕️  Введи відстань між рядами по вертикалі (Y): "))
          
          (if (> h_space 0) (> v_space 0)
            (progn
              ;; --- Крок 2: Розрахунок та розміщення блоків ---
              (vla-getboundingbox pline_vla 'min_pt 'max_pt)
              (setq min_x (car (vlax-safearray->list min_pt))
                    min_y (cadr (vlax-safearray->list min_pt))
                    max_x (car (vlax-safearray->list max_pt))
                    max_y (cadr (vlax-safearray->list max_pt)))
              
              (setq cur_y min_y
                    row_idx 0
                    block_count 0)
              
              (while (<= cur_y max_y)
                (setq is_odd_row (= 1 (rem row_idx 2)))
                (setq offset (if is_odd_row (/ h_space 2.0) 0))
                (setq cur_x (+ min_x offset))
                
                (while (<= cur_x max_x)
                  (if (IsPointInside (list cur_x cur_y 0.0) pline_vla)
                    (progn
                      (vla-insertblock (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))) (vlax-3d-point (list cur_x cur_y 0.0)) block_name 1.0 1.0 1.0 0)
                      (setq block_count (1+ block_count))
                    )
                  )
                  (setq cur_x (+ cur_x h_space))
                )
                (setq cur_y (+ cur_y v_space)
                      row_idx (1+ row_idx))
              )
              (princ (strcat "\n🎉 Готово! Вставлено " (itoa block_count) " блоків. ✨"))
            )
            (princ "\n😥 Відстані мають бути більшими за нуль.")
          )
        )
        (princ "\n😥 Це не замкнута полілінія! Будь ласка, вибери правильний об'єкт.")
      )
    )
    (princ "\n😥 Це не блок! Будь ласка, вибери блок для заповнення.")
  )
  (princ)
)