;;;------------------------------------------------------------------------------;;;
;;; Команда: MoveTextToBlockByName                                             ;;;
;;; Опис: Знаходить найближчий блок із заданим іменем до кожного обраного       ;;;
;;;       текстового об'єкта. Пропонує перемістити текст в точку вставки        ;;;
;;;       (X, Y) знайденого блоку, зберігаючи вихідну Z-координату тексту.      ;;;
;;; Версія: 1.1 (Додано фільтр за іменем блоку)                                 ;;;
;;;------------------------------------------------------------------------------;;;
(defun c:MoveTextToBlockByName (/ *error* ss_blocks ss_text fix_list i j text_ent text_pt min_dist nearest_block_pt block_ent block_pt dist_2d response moved_count pair new_pt old_pt block_name)

  ;; Функція для обробки помилок
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nПомилка: " msg))
    )
    (princ)
  )

  ;; --- Налаштування ---
  (setq block_name "SYMBOL4867472") ; <--- ВКАЖІТЬ ТУТ ІМ'Я ПОТРІБНОГО БЛОКУ

  ;; Крок 1: Знайти всі блоки в кресленні з конкретним іменем
  ;; DXF-код 2 відповідає за ім'я блоку.
  (setq ss_blocks (ssget "_X" (list (cons 0 "INSERT") (cons 2 block_name))))

  (if (not ss_blocks)
    (princ (strcat "\nВ поточному кресленні не знайдено блоків з іменем '" block_name "'."))
    (progn
      ;; Крок 2: Запросити користувача вибрати текстові об'єкти
      (princ "\nОберіть текстові об'єкти для переміщення: ")
      (setq ss_text (ssget '((0 . "TEXT,MTEXT"))))

      (if ss_text
        (progn
          (setq i 0)
          (setq fix_list '())

          (repeat (sslength ss_text)
            (setq text_ent (ssname ss_text i))
            (setq text_pt (cdr (assoc 10 (entget text_ent))))
            (setq min_dist -1.0)
            (setq nearest_block_pt nil)
            (setq j 0)

            (repeat (sslength ss_blocks)
              (setq block_ent (ssname ss_blocks j))
              (setq block_pt (cdr (assoc 10 (entget block_ent))))
              (setq dist_2d (distance (list (car text_pt) (cadr text_pt))
                                      (list (car block_pt) (cadr block_pt))))
              
              (if (or (< min_dist 0) (< dist_2d min_dist))
                (progn
                  (setq min_dist dist_2d)
                  (setq nearest_block_pt block_pt)
                )
              )
              (setq j (1+ j))
            )
            
            (if nearest_block_pt
              (setq fix_list (cons (list text_ent nearest_block_pt) fix_list))
            )
            (setq i (1+ i))
          )
          
          (if fix_list
            (progn
              (initget "Yes No")
              (setq response
                (getkword
                  (strcat
                    "\nЗнайдено найближчі блоки для "
                    (itoa (length fix_list))
                    " текстових об'єктів. Перемістити їх? [Yes/No] <Yes>: "
                  )
                )
              )

              (if (or (not response) (= response "Yes"))
                (progn
                  (setq moved_count 0)
                  (foreach pair fix_list
                    (setq text_ent (car pair))
                    (setq nearest_block_pt (cadr pair))
                    (setq old_pt (cdr (assoc 10 (entget text_ent))))

                    (setq new_pt (list (car nearest_block_pt)
                                       (cadr nearest_block_pt)
                                       (caddr old_pt)
                                 )
                    )
                    (entmod (subst (cons 10 new_pt) (assoc 10 (entget text_ent)) (entget text_ent)))
                    (setq moved_count (1+ moved_count))
                  )
                  (princ (strcat "\nГотово! Переміщено " (itoa moved_count) " текстових об'єктів."))
                )
                (princ "\nДобре, жодних змін не було зроблено.")
              )
            )
            (princ "\nНе вдалося знайти відповідності для обраних текстів.")
          )
        )
        (princ "\nНе обрано жодного текстового об'єкта.")
      )
    )
  )
  (princ)
)