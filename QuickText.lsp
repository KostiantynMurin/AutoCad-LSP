;; =================================================================================================
;; |                                                                                               |
;; |                      СКРИПТ ДЛЯ ШВИДКОГО СТВОРЕННЯ ТА РОЗМІЩЕННЯ ТЕКСТУ                       |
;; |                                                                                               |
;; | Версія: 1.2-diag (Діагностична: МАКСИМАЛЬНО СПРОЩЕНИЙ ЦИКЛ ПЕРЕМІЩЕННЯ)                        |
;; |                                                                                               |
;; =================================================================================================

;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; --- Основна функція команди ---
(defun c:QuickText ( / *error* old_cmdecho p1 p2 text_str text_style text_height text_color 
                       text_angle pi_val text_ent text_vla_obj 
                       gr_result gr_code gr_pt done last_pt
                    )
  
  ;; Функція обробки помилок
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if (and text_vla_obj (not (vlax-erased-p text_vla_obj))) (vla-delete text_vla_obj))
    (if (not (member msg '("Function cancelled" "quit / exit abort" nil)))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ)
  )

  ;; --- Ініціалізація та налаштування ---
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq text_style "Д-431"
        text_height 1.0
        text_color 5
        pi_val 3.14159265358979
  )

  ;; === ОСНОВНИЙ ЦИКЛ ПРОГРАМИ ===
  (while
    (progn
      (setq p1 (getpoint "\nВкажіть першу точку для визначення кута (Esc для виходу):"))
      (if p1
        (setq p2 (getpoint p1 "\nВкажійте другу точку:"))
      )

      (if (and p1 p2)
        (progn
          (setq text_angle (angle p1 p2))
          (if (and (> text_angle (/ pi_val 2.0)) (< text_angle (* 1.5 pi_val)))
            (setq text_angle (+ text_angle pi_val))
          )
          
          (setq text_str (getstring T "\nВведіть текст: "))
          
          (if (/= text_str "")
            (progn
              (setq text_ent nil text_vla_obj nil)
              (entmake 
                (list
                  '(0 . "TEXT") (cons 1 text_str) (cons 40 text_height) (cons 7 text_style)
                  (cons 62 text_color) (cons 50 text_angle)
                  (cons 10 p2) (cons 11 p2) '(72 . 1) '(73 . 2)
                )
              )
              (setq text_ent (entlast) text_vla_obj (vlax-ename->vla-object text_ent))
              
              ;; === МАКСИМАЛЬНО СПРОЩЕНИЙ ЦИКЛ ПЕРЕМІЩЕННЯ ===
              (princ (strcat "\n[Тест] Переміщуйте курсор. Ліва кнопка - вставити."))
              (setq last_pt p2 done nil)
              (while (not done)
                (setq gr_result (grread T))
                (setq gr_code (car gr_result) gr_pt (cadr gr_result))
                
                ;; Перевіряємо, чи отримали ми точку
                (if (and gr_pt (= (type gr_pt) 'LIST))
                  ;; Якщо так, просто рухаємо об'єкт
                  (progn
                     (vla-move text_vla_obj (vlax-3d-point last_pt) (vlax-3d-point gr_pt))
                     (setq last_pt gr_pt)
                  )
                )
                
                ;; Перевіряємо кліки та клавіші
                (cond
                  ((= gr_code 3) ; ТІЛЬКИ Ліва кнопка миші
                   (setq done T text_vla_obj nil)
                  )
                  ((= gr_code 2) ; Клавіатура
                   (if (= (cadr gr_result) 27) ; Клавіша Escape
                     (progn
                       (vla-delete text_vla_obj)
                       (princ "\nСтворення тексту скасовано.")
                       (setq done T)
                     )
                   )
                  )
                )
              ) ; кінець while grread
            )
            (princ "\nПропущено. Введено порожній рядок.")
          )
        )
        (princ "\nПропущено. Не вказано обидві точки для визначення кута.")
      )
      p1
    )
  )
  
  (setvar "CMDECHO" old_cmdecho)
  (princ "\nРоботу завершено.")
  (princ)
)

(defun c:QT () (c:QuickText))

(princ "\nКоманду 'QuickText' (v1.2, діагностична) завантажено.")
(princ)