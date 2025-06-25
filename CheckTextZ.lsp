;;;------------------------------------------------------------------------------;;;
;;; Команда: CheckTextZ                                                        ;;;
;;; Опис: Перевіряє Z-координату обраних текстових об'єктів (TEXT, MTEXT).      ;;;
;;; Порівнює фактичну Z-координату з числовим значенням, вказаним у самому      ;;;
;;; тексті. Якщо є розбіжність, пропонує виправити положення об'єктів.         ;;;
;;; Версія: 1.1 (Виправлено потенційну проблему з ключовими словами)           ;;;
;;;------------------------------------------------------------------------------;;;
(defun c:CheckTextZ (/ *error* ss i ent edata pt z_actual text_content z_from_text tolerance mismatch_list response moved_count)

  ;; Функція для обробки помилок (наприклад, якщо користувач натисне ESC)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nПомилка: " msg))
    )
    (princ) ; "Тихе" завершення
  )

  ;; --- Налаштування ---
  (setq tolerance 0.5) ; Допуск. Якщо різниця Z більша, об'єкт вважається невірним.

  (princ "\nОберіть текстові об'єкти для перевірки Z-координати: ")
  
  ;; Фільтруємо вибір, щоб користувач міг обрати тільки TEXT та MTEXT
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))

  (if ss
    (progn
      (setq i 0)
      (setq mismatch_list '()) ; Створюємо пустий список для "неправильних" об'єктів
      
      ;; Починаємо цикл перевірки кожного обраного об'єкта
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq edata (entget ent))
        (setq pt (cdr (assoc 10 edata)))
        (setq z_actual (caddr pt))
        (setq text_content (cdr (assoc 1 edata)))
        (setq z_from_text (atof text_content))
        
        ;; Порівнюємо фактичну Z з Z із тексту з урахуванням допуску
        (if (> (abs (- z_actual z_from_text)) tolerance)
          ;; Якщо є розбіжність, додаємо об'єкт до списку на виправлення
          (setq mismatch_list (cons ent mismatch_list))
        )
        (setq i (1+ i))
      )
      
      ;; Після перевірки всіх об'єктів аналізуємо результат
      (if mismatch_list
        (progn
          ;; Використовуємо англійські ключові слова для надійності
          (initget "Yes No")
          (setq response
                 (getkword
                   (strcat
                     "\nЗнайдено "
                     (itoa (length mismatch_list))
                     " об'єктів з невірною Z-координатою. Змінити їх положення? [Yes/No] <Yes>: "
                   )
                 )
          )
          
          ;; Якщо відповідь "Yes" або користувач просто натиснув Enter
          (if (or (not response) (= response "Yes"))
            (progn
              (setq moved_count 0)
              ;; Запускаємо цикл для виправлення кожного об'єкта зі списку
              (foreach entity mismatch_list
                (setq edata (entget entity))
                (setq pt (cdr (assoc 10 edata)))
                (setq text_content (cdr (assoc 1 edata)))
                (setq z_from_text (atof text_content))
                
                (setq new_pt (list (car pt) (cadr pt) z_from_text))
                (setq edata (subst (cons 10 new_pt) (assoc 10 edata) edata))
                (entmod edata)
                (setq moved_count (1+ moved_count))
              )
              (princ (strcat "\nГотово! Змінено положення для " (itoa moved_count) " об'єктів."))
            )
            (princ "\nДобре, жодних змін не було зроблено.")
          )
        )
        (princ "\nУсі обрані об'єкти мають правильну Z-координату.")
      )
    )
    (princ "\nНе обрано жодного об'єкта. Спробуйте ще раз.")
  )
  (princ)
)