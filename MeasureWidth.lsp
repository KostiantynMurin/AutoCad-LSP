;; =================================================================== ;;
;; Скрипт для вимірювання ширини між двома точками.                    ;;
;; Версія 5.0: Робочий процес вставки тексту як у скрипті PPON.        ;;
;; Команда для запуску: WW                                               ;;
;; =================================================================== ;;

(defun c:WW ( / p1 p2 dist dist_text text_ins_pt text-style text-height text-color text-angle cur-layer)
  
  ;; --- Налаштування параметрів тексту ---
  (setq text-style  "Д-431" ; Назва стилю
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
      (setq p1 (getpoint "\nВкажіть першу точку: "))
      (if p1 (setq p2 (getpoint p1 "\nВкажіть другу точку: ")))
    )

    ;; Продовжуємо, тільки якщо обидві точки були вказані
    (if (and p1 p2)
      (progn
        (setq dist (distance p1 p2))
        (setq dist_text (rtos dist 2 2))
        (setq dist_text (vl-string-subst "." "," dist_text))
        
        ;; -- ПОВЕРНЕНО РУЧНУ ВСТАВКУ ТЕКСТУ (як у скрипті PPON) --
        (setq text_ins_pt (getpoint "\nВкажіть місце для тексту результату: "))
        
        ;; Створюємо текст, тільки якщо користувач вказав точку
        (if text_ins_pt
          (progn
            (setq cur-layer (getvar "CLAYER"))
            
            (entmake
              (list
                '(0 . "TEXT")                     
                (cons 1 dist_text)               
                (cons 10 text_ins_pt) ; Використовуємо вказану користувачем точку
                (cons 40 text-height)            
                (cons 50 text-angle)             
                (cons 7 text-style)              
                (cons 8 cur-layer)               
                (cons 62 text-color)             
                '(72 . 4)      ; Вирівнювання по горизонталі: Middle
                '(73 . 2)      ; Вирівнювання по вертикалі: Middle
              )
            )
            (princ (strcat "\nСтворено текст: \"" dist_text "\"."))
          )
          ;; Повідомлення, якщо користувач натиснув Esc
          (princ "\nВставку тексту скасовано.")
        )
      )
    )
  );; =================================================================== ;;
;; Скрипт для вимірювання ширини між двома точками.                    ;;
;; Версія 7.0: Підвищена надійність за зразком PPON.                   ;;
;; Команда для запуску: WW                                               ;;
;; =================================================================== ;;

(defun c:WW ( / *error* old_cmdecho old_osmode p1 p2 dist dist_text text_ins_pt text-style text-height text-color text-angle cur-layer)
  
  ;; --- Функція для коректної обробки помилок та натискання Esc ---
  ;; Вона відновлює збережені налаштування AutoCAD
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nПомилка: " msg))
    )
    (princ)
  )

  ;; --- Зберігаємо поточні налаштування AutoCAD ---
  (setq old_cmdecho (getvar "CMDECHO")
        old_osmode  (getvar "OSMODE")
  )
  (setvar "CMDECHO" 0) ; Вимикаємо відображення команд у консолі

  ;; --- Налаштування параметрів тексту ---
  (setq text-style  "Д-431"
        text-height 1.0     
        text-color  5       
        text-angle  0.0     
  )
  
  (princ "\nЗапуск скрипту вимірювання. Натисніть ESC для виходу.")

  (if (not (tblsearch "STYLE" text-style))
    (command "_.-STYLE" text-style "romans.shx" "0" "1" "0" "N" "N")
  )

  (while
    (progn
      (setq p1 (getpoint "\nВкажіть першу точку: "))
      (if p1 (setq p2 (getpoint p1 "\nВкажіть другу точку: ")))
    )

    (if (and p1 p2)
      (progn
        (setq dist (distance p1 p2))
        (setq dist_text (rtos dist 2 2))
        (setq dist_text (vl-string-subst "." "," dist_text))
        
        (setq text_ins_pt (getpoint "\nВкажіть місце для тексту результату: "))
        
        (if text_ins_pt
          (progn
            (setq cur-layer (getvar "CLAYER"))
            (entmake
              (list '(0 . "TEXT") (cons 1 dist_text) (cons 10 text_ins_pt)
                    (cons 40 text-height) (cons 50 text-angle) (cons 7 text-style)
                    (cons 8 cur-layer) (cons 62 text-color) '(72 . 4) '(73 . 2)
              )
            )
            (princ (strcat "\nСтворено текст: \"" dist_text "\"."))
          )
          (princ "\nВставку тексту скасовано.")
        )
      )
    )
  )
  
  ;; --- Відновлюємо збережені налаштування AutoCAD ---
  (setvar "CMDECHO" old_cmdecho)
  (setvar "OSMODE" old_osmode)
  (princ)
)

(princ "\nСкрипт 'WW' (v7.0 Надійний) завантажено. Введіть команду WW.")
(princ)
  (princ)
)

(princ "\nСкрипт 'WW' (v5.0) завантажено. Введіть команду WW для запуску.")
(princ)