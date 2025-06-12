;; =================================================================================================
;; |                                                                                               |
;; |                      СКРИПТ ДЛЯ ШВИДКОГО СТВОРЕННЯ ТА РОЗМІЩЕННЯ ТЕКСТУ                       |
;; |                                                                                               |
;; | Версія: 1.0                                                                                   |
;; |                                                                                               |
;; | Опис:                                                                                         |
;; | 1. Користувач вказує напрямок двома точками.                                                  |
;; | 2. Вводить текст.                                                                             |
;; | 3. Текст "прилипає" до курсора для візуального розміщення.                                    |
;; | 4. Права кнопка миші обертає текст на 180 градусів.                                           |
;; | 5. Ліва кнопка миші фіксує текст на кресленні.                                                 |
;; |                                                                                               |
;; | Команди для запуску: QuickText або QT                                                         |
;; |                                                                                               |
;; =================================================================================================

;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; --- Основна функція команди ---
(defun c:QuickText ( / *error* old_cmdecho p1 p2 text_str text_style text_height text_color 
                       text_angle pi_val text_ent text_vla_obj 
                       gr_result gr_code gr_pt done current_angle last_pt
                       snapped_pt effective_pt ; Оголошення необхідних змінних
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
      ;; --- Крок 1: Визначення кута ---
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
          
          ;; --- Крок 2: Введення тексту ---
          (setq text_str (getstring T "\nВведіть текст: "))
          
          (if (/= text_str "")
            (progn
              ;; --- Крок 3: Створення тексту та інтерактивне розміщення ---
              (setq text_ent nil text_vla_obj nil)
              (entmake 
                (list
                  '(0 . "TEXT") (cons 1 text_str) (cons 40 text_height) (cons 7 text_style)
                  (cons 62 text_color) (cons 50 text_angle)
                  (cons 10 p2) (cons 11 p2) '(72 . 1) '(73 . 2)
                )
              )
              (setq text_ent (entlast) text_vla_obj (vlax-ename->vla-object text_ent))
              
              (princ (strcat "\nСтворено текст. Переміщуйте курсор. Ліва кнопка - вставити, Права - повернути."))
              (setq last_pt p2 done nil)
              (while (not done)
                (setq gr_result (grread T))
                (setq gr_code (car gr_result) gr_pt (cadr gr_result))
                (if (and gr_pt (= (type gr_pt) 'LIST))
                  (progn
                    (setq snapped_pt (osnap gr_pt))
                    (setq effective_pt (if snapped_pt snapped_pt gr_pt))
                    (cond
                      ((= gr_code 5)
                       (vla-move text_vla_obj (vlax-3d-point last_pt) (vlax-3d-point effective_pt))
                       (setq last_pt effective_pt)
                      )
                      ((= gr_code 25)
                       (setq current_angle (vla-get-Rotation text_vla_obj))
                       (vla-put-Rotation text_vla_obj (+ current_angle pi_val))
                       (princ "\nТекст повернуто на 180°.")
                      )
                      ((= gr_code 3)
                       (vla-move text_vla_obj (vlax-3d-point last_pt) (vlax-3d-point effective_pt))
                       (setq done T text_vla_obj nil)
                      )
                      ((= gr_code 2)
                       (if (= (cadr gr_result) 27)
                         (progn
                           (vla-delete text_vla_obj)
                           (princ "\nСтворення тексту скасовано.")
                           (setq done T)
                         )
                       )
                      )
                    )
                  )
                )
              )
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

;; Створення короткого псевдоніма (аліаса) для команди
(defun c:QT () (c:QuickText))

(princ "\nКоманду 'QuickText' (або 'QT') завантажено.")
(princ)