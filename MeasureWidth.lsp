;; =================================================================== ;;
;; Скрипт для вимірювання ширини між двома точками.                    ;;
;; Версія 8.0: Надійний метод "Створити і Перемістити".                ;;
;; Команда для запуску: WW                                               ;;
;; =================================================================== ;;

(defun c:WW ( / *error* old_cmdecho old_osmode p1 p2 dist dist_text mid_p text-style text-height text-color text-angle cur-layer new_text_ent)
  
  ;; Функція для коректної обробки помилок та натискання Esc
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nПомилка: " msg))
    )
    (princ)
  )

  ;; Зберігаємо поточні налаштування AutoCAD
  (setq old_cmdecho (getvar "CMDECHO")
        old_osmode  (getvar "OSMODE")
  )
  
  ;; Налаштування параметрів тексту
  (setq text-style  "Д-431"
        text-height 1.0     ; Висота тексту. Змініть, якщо потрібно.
        text-color  5       ; Колір (5 = Синій/Blue)
        text-angle  0.0     ; Кут повороту
  )
  
  (princ "\nЗапуск скрипту вимірювання. Натисніть ESC для виходу.")

  (if (not (tblsearch "STYLE" text-style))
    (command "_.-STYLE" text-style "romans.shx" "0" "1" "0" "N" "N")
  )

  (while
    (progn
      (setvar "CMDECHO" 0)
      (setq p1 (getpoint "\nВкажіть першу точку: "))
      (if p1 (setq p2 (getpoint p1 "\nВкажіть другу точку: ")))
    )

    (if (and p1 p2)
      (progn
        (setq dist (distance p1 p2))
        (setq dist_text (rtos dist 2 2))
        (setq dist_text (vl-string-subst "." "," dist_text))
        
        ;; Обчислюємо середню точку для тимчасової вставки
        (setq mid_p (list
                      (/ (+ (car p1) (car p2)) 2.0)
                      (/ (+ (cadr p1) (cadr p2)) 2.0)
                      (/ (+ (caddr p1) (caddr p2)) 2.0)
                    )
        )
        
        (setq cur-layer (getvar "CLAYER"))
        
        ;; КРОК 1: Створюємо текст в середній точці
        (entmake
          (list
            '(0 . "TEXT") (cons 1 dist_text) (cons 10 mid_p) (cons 40 text-height)
            (cons 50 text-angle) (cons 7 text-style) (cons 8 cur-layer)
            (cons 62 text-color) '(72 . 4) '(73 . 2)
          )
        )
        (setq new_text_ent (entlast)) ; Запам'ятовуємо щойно створений текст

        ;; КРОК 2: Запускаємо для користувача команду ПЕРЕМІСТИТИ
        (setvar "CMDECHO" 1) ; Вмикаємо відображення, щоб було видно підказки команди MOVE
        (princ "\nТекст створено. Перемістіть його в потрібне місце.")
        (command "_MOVE" new_text_ent "" mid_p pause)
        
      )
    )
  )
  
  ;; Відновлюємо збережені налаштування AutoCAD
  (setvar "CMDECHO" old_cmdecho)
  (setvar "OSMODE" old_osmode)
  (princ)
)

(princ "\nСкрипт 'WW' (v8.0, з переміщенням) завантажено. Введіть команду WW.")
(princ)