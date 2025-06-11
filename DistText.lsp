;; =================================================================================================
;; |                                                                                               |
;; |                      СКРИПТ ДЛЯ ВИМІРЮВАННЯ ВІДСТАНІ ТА ВСТАВКИ ТЕКСТУ                         |
;; |                                                                                               |
;; | Версія: 6.0 (Додано інтерактивний поворот тексту правою кнопкою миші)                         |
;; | Дата: 11.06.2025                                                                              |
;; |                                                                                               |
;; | Опис:                                                                                         |
;; | Скрипт вимірює відстань і створює перпендикулярний текст.                                      |
;; | Під час переміщення тексту права кнопка миші обертає його на 180 градусів.                     |
;; | Результат завжди форматується до двох знаків після коми (напр. 2.20).                         |
;; |                                                                                               |
;; | Команди для запуску: DistText або DT                                                          |
;; |                                                                                               |
;; =================================================================================================

;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; --- Основна функція команди ---
(defun c:DistText ( / *error* old_cmdecho p1 p2 dist dist_str text_style text_height text_color 
                      text_angle pi_val temp_str decimal_pos text_ent text_vla_obj 
                      gr_result gr_code gr_pt done current_angle last_pt
                   )
  
  ;; Функція обробки помилок
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if (not (member msg '("Function cancelled" "quit / exit abort" nil)))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ)
  )

  ;; Збереження системних змінних
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; --- Налаштування параметрів тексту ---
  (setq text_style "Д-431")
  (setq text_height 1.0)
  (setq text_color 5)
  (setq pi_val 3.14159265358979)

  ;; Основний цикл програми
  (while 
    (setq p1 (getpoint "\nВкажіть першу точку (Esc для виходу): "))
    
    (if (setq p2 (getpoint p1 "\nВкажійте другу точку: "))
      (progn ; Виконується, якщо вказано обидві точки
        
        ;; 1. Обчислення відстані
        (setq dist (distance p1 p2))

        ;; === ВИПРАВЛЕНИЙ БЛОК ФОРМАТУВАННЯ РЯДКА ===
        (setq temp_str (rtos dist 2 2))
        (setq decimal_pos (vl-string-search "." temp_str))
        (cond
          ((not decimal_pos) ; Якщо крапки немає (ціле число)
           (setq dist_str (strcat temp_str ".00"))
          )
          ((= 1 (- (strlen temp_str) (1+ decimal_pos))) ; Якщо після крапки один знак
           (setq dist_str (strcat temp_str "0"))
          )
          (T ; Якщо знаків два (або більше)
           (setq dist_str temp_str)
          )
        )
        ;; ============================================

        ;; 2. Блок обчислення перпендикулярного кута
        (setq text_angle (angle p1 p2))
        (if (and (> text_angle (/ pi_val 2.0)) (< text_angle (* 1.5 pi_val)))
          (setq text_angle (+ text_angle pi_val))
        )
        (setq text_angle (+ text_angle (/ pi_val 2.0)))
        
        ;; 3. Створення текстового об'єкта у тимчасовій точці
        (entmake 
          (list
            '(0 . "TEXT") (cons 1 dist_str) (cons 40 text_height) (cons 7 text_style)
            (cons 62 text_color) (cons 50 text_angle)
            (cons 10 p1) (cons 11 p1) '(72 . 1) '(73 . 2)
          )
        )
        (setq text_ent (entlast)
              text_vla_obj (vlax-ename->vla-object text_ent)
        )
        
        ;; === НОВИЙ БЛОК: Власний цикл переміщення з реакцією на кліки ===
        (princ (strcat "\nСтворено текст: \"" dist_str "\". Переміщуйте курсор. Ліва кнопка - вставити, Права - повернути на 180°."))
        (setq last_pt p1 done nil)
        (while (not done)
          (setq gr_result (grread T 15 0)
                gr_code (car gr_result)
                gr_pt (cadr gr_result)
          )
          (cond
            ;; Рух миші
            ((= gr_code 5)
              (vla-move text_vla_obj (vlax-3d-point last_pt) (vlax-3d-point gr_pt))
              (setq last_pt gr_pt)
            )
            ;; Права кнопка миші - Поворот
            ((= gr_code 25)
              (setq current_angle (vla-get-Rotation text_vla_obj))
              (vla-put-Rotation text_vla_obj (+ current_angle pi_val))
              (princ "\nТекст повернуто на 180°.")
            )
            ;; Ліва кнопка миші - Вставка
            ((= gr_code 3)
              (setq done T)
            )
            ;; Клавіатура (Esc)
            ((= gr_code 2)
              (if (= (cadr gr_result) 27) ; 27 - код клавіші Escape
                (progn
                  (vla-delete text_vla_obj)
                  (princ "\nСтворення тексту скасовано.")
                  (setq done T)
                )
              )
            )
          )
        )
        ;; =============================================================
        
      )
      (princ "\nВибір другої точки скасовано.")
    )
  ) ; Кінець циклу while

  ;; Відновлення системних змінних
  (setvar "CMDECHO" old_cmdecho)
  
  (princ "\nРоботу завершено.")
  (princ)
)

;; Створення короткого псевдоніма (аліаса) для команди
(defun c:DT () (c:DistText))

;; Повідомлення про успішне завантаження скрипта
(princ "\nКоманду 'DistText' v6.0 (з інтерактивним поворотом) завантажено.")
(princ)

;;; --- КІНЕЦЬ ФАЙЛУ ---