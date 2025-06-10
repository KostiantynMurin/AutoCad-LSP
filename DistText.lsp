;; =================================================================================================
;; |                                                                                               |
;; |                      СКРИПТ ДЛЯ ВИМІРЮВАННЯ ВІДСТАНІ ТА ВСТАВКИ ТЕКСТУ                         |
;; |                                                                                               |
;; | Версія: 5.1 (Додано форматування до двох знаків після коми)                                   |
;; | Дата: 10.06.2025                                                                              |
;; |                                                                                               |
;; | Опис:                                                                                         |
;; | Скрипт вимірює відстань і створює текст, повернений перпендикулярно до лінії.                  |
;; | Після створення текст "прилипає" до курсора для візуального розміщення.                       |
;; | Результат завжди форматується до двох знаків після коми (напр. 2.20).                         |
;; |                                                                                               |
;; | Команди для запуску: DistText або DT                                                          |
;; |                                                                                               |
;; =================================================================================================

;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; --- Основна функція команди ---
(defun c:DistText ( / *error* old_cmdecho p1 p2 dist dist_str text_ins_pt text_style text_height text_color text_angle pi_val temp_str decimal_pos)
  
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

        ;; === НОВИЙ БЛОК: Форматування рядка до двох знаків після коми ===
        (setq temp_str (rtos dist 2 2))
        (setq decimal_pos (vl-string-search "." temp_str))
        (if decimal_pos
          ;; Якщо є десяткова крапка
          (if (= 1 (- (strlen temp_str) (1+ decimal_pos)))
            (setq dist_str (strcat temp_str "0")) ; Додаємо один нуль
            (setq dist_str temp_str)              ; Вже є два знаки
          )
          ;; Якщо десяткової крапки немає (ціле число)
          (setq dist_str (strcat temp_str ".00")) ; Додаємо .00
        )
        ;; ==============================================================

        ;; 2. Блок обчислення перпендикулярного кута
        (setq text_angle (angle p1 p2))
        (if (and (> text_angle (/ pi_val 2.0)) (< text_angle (* 1.5 pi_val)))
          (setq text_angle (+ text_angle pi_val))
        )
        (setq text_angle (+ text_angle (/ pi_val 2.0)))
        
        ;; 3. Створення та інтерактивне переміщення
        (entmake 
          (list
            '(0 . "TEXT")
            (cons 1 dist_str)
            (cons 40 text_height)
            (cons 7 text_style)
            (cons 62 text_color)
            (cons 50 text_angle)
            (cons 10 p1) (cons 11 p1)
            '(72 . 1) '(73 . 2)
          )
        )
        (princ (strcat "\nСтворено текст: \"" dist_str "\". Вкажіть його кінцеве положення."))
        (command "_MOVE" "_LAST" "" p1 PAUSE)
        
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
(princ "\nКоманду 'DistText' v5.1 (з форматуванням нулів) завантажено.")
(princ)

;;; --- КІНЕЦЬ ФАЙЛУ ---