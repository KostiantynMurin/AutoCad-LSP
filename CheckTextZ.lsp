;;;------------------------------------------------------------------------------;;;
;;; Команда: CheckTextZ                                                        ;;;
;;; Опис: Перевіряє Z-координату обраних текстових об'єктів (TEXT, MTEXT).      ;;;
;;; Порівнює фактичну Z-координату з числовим значенням, вказаним у самому      ;;;
;;; тексті. Якщо є розбіжність, пропонує виправити положення об'єктів.         ;;;
;;;------------------------------------------------------------------------------;;;
(defun  (/ *error* ss i ent edata pt z_actual text_content z_from_text tolerance mismatch_list response moved_count)

  ;; Функція для обробки помилок (наприклад, якщо користувач натисне ESC)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nПомилка: " msg))
    )
    (princ) ; "тихе" завершення
  )

  ;; --- Налаштування ---
  (setq tolerance 0.5) ; Встановіть допуск. Якщо різниця Z більша за це значення, об'єкт вважається невірним.

  (princ "\nБудь ласка, оберіть текстові об'єкти для перевірки Z-координати 🧐: ")
  
  ;; Фільтруємо вибір, щоб користувач міг обрати тільки TEXT та MTEXT
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))

  (if ss
    (progn
      (setq i 0)
      (setq mismatch_list '()) ; Створюємо пустий список для "неправильних" об'єктів
      
      ;; Починаємо цикл перевірки кожного обраного об'єкта
      (repeat (sslength ss)
        (setq ent (ssname ss i))     ; Отримуємо ім'я об'єкта
        (setq edata (entget ent))    ; Отримуємо його властивості (DXF-коди)
        
        (setq pt (cdr (assoc 10 edata)))         ; Точка вставки (X Y Z)
        (setq z_actual (caddr pt))              ; Фактична Z-координата
        
        (setq text_content (cdr (assoc 1 edata))) ; Вміст тексту (його "содержимое")
        (setq z_from_text (atof text_content))    ; Конвертуємо текст в число
        
        ;; Порівнюємо фактичну Z з Z із тексту з урахуванням допуску
        (if (> (abs (- z_actual z_from_text)) tolerance)
          ;; Якщо є розбіжність, додаємо об'єкт до списку на виправлення
          (setq mismatch_list (cons ent mismatch_list))
        )
        (setq i (1+ i)) ; Переходимо до наступного об'єкта
      )
      
      ;; Після перевірки всіх об'єктів аналізуємо результат
      (if mismatch_list
        (progn
          ;; Якщо список не порожній, запитуємо користувача про виправлення
          (initget "Так Ні") ; Встановлюємо ключові слова для відповіді
          (setq response
                 (getkword
                   (strcat
                     "\n❗ Знайдено "
                     (itoa (length mismatch_list))
                     " об'єктів з невірною Z-координатою. Змінити їх положення? [Так/Ні] <Так>: "
                   )
                 )
          )
          
          ;; Якщо відповідь "Так" або користувач просто натиснув Enter (за замовчуванням "Так")
          (if (or (not response) (= response "Так"))
            (progn
              (setq moved_count 0)
              ;; Запускаємо цикл для виправлення кожного об'єкта зі списку
              (foreach entity mismatch_list
                (setq edata (entget entity))
                (setq pt (cdr (assoc 10 edata)))
                (setq text_content (cdr (assoc 1 edata)))
                (setq z_from_text (atof text_content))
                
                ;; Створюємо нову точку вставки з правильною Z-координатою
                (setq new_pt (list (car pt) (cadr pt) z_from_text))
                
                ;; Замінюємо стару точку вставки (DXF-код 10) на нову
                (setq edata (subst (cons 10 new_pt) (assoc 10 edata) edata))
                
                ;; Оновлюємо об'єкт в кресленні
                (entmod edata)
                (setq moved_count (1+ moved_count))
              )
              (princ (strcat "\n✅ Готово! Змінено положення для " (itoa moved_count) " об'єктів. "))
            )
            (princ "\nДобре, жодних змін не було зроблено. 😉")
          )
        )
        (princ "\n✅ Чудово! Усі обрані об'єкти мають правильну Z-координату.")
      )
    )
    (princ "\nНе обрано жодного об'єкта. Спробуйте ще раз. 😊")
  )
  (princ) ; "Чисте" завершення команди без виводу nil
)