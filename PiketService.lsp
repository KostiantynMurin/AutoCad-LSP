;; ===================================================================================
;; |                                                                                 |
;; |      СКРИПТ ДЛЯ КОРЕКЦІЇ ЗНАЧЕНЬ АТРИБУТА "ОТМЕТКА" В БЛОКАХ "PIKET"            |
;; |                                                                                 |
;; | Версія: 2.1 (Додано візуальне виділення знайдених блоків)                       |
;; |                                                                                 |
;; | Опис:                                                                           |
;; | Скрипт знаходить блоки "PIKET" з відміткою > 300, виділяє їх на кресленні,       |
;; | показує список, запитує підтвердження і лише після цього вносить зміни.         |
;; |                                                                                 |
;; | Команда для запуску: AdjustPiketOtmetka                                         |
;; |                                                                                 |
;; ===================================================================================

(defun c:AdjustPiketOtmetka ( / *error* old_cmdecho doc ss_pikets i piket_vla 
                               attributes att_obj current_value_str current_value_num
                               new_value_num new_value_str changed_count
                               nomera_val_str otmetka_val_str blocks_to_change_list user_choice
                               ss_to_highlight ; Нова змінна
                            )

  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if doc (if (= 8 (logand 8 (getvar "UNDOCTL"))) (vla-EndUndoMark doc)))
    (if (not (member msg '("Function cancelled" "quit / exit abort" nil)))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ)
  )

  (vl-load-com)
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  (princ "\nПошук блоків 'PIKET' для обробки...")
  
  (setq ss_pikets (ssget "_X" '((0 . "INSERT") (2 . "PIKET") (66 . 1))))

  (if ss_pikets
    (progn
      (setq i 0)
      (setq blocks_to_change_list '())
      (setq ss_to_highlight (ssadd)) ; Створюємо порожній набір для підсвічування
      (princ (strcat "\nЗнайдено " (itoa (sslength ss_pikets)) " блоків. Аналізую дані..."))

      (repeat (sslength ss_pikets)
        (setq piket_vla (vlax-ename->vla-object (ssname ss_pikets i)))
        (if (= (vla-get-HasAttributes piket_vla) :vlax-true)
          (progn
            (setq attributes (vlax-invoke piket_vla 'GetAttributes)
                  nomera_val_str nil
                  otmetka_val_str nil
            )
            (foreach att_obj attributes
              (cond
                ((= (strcase (vla-get-TagString att_obj)) "НОМЕРА")
                 (setq nomera_val_str (vla-get-TextString att_obj))
                )
                ((= (strcase (vla-get-TagString att_obj)) "ОТМЕТКА")
                 (setq otmetka_val_str (vla-get-TextString att_obj))
                 (setq otmetka_obj att_obj)
                )
              )
            )
            (if (and nomera_val_str otmetka_val_str
                     (setq current_value_num (distof otmetka_val_str))
                     (> current_value_num 300.0)
                )
              (progn
                (setq blocks_to_change_list (cons (list nomera_val_str otmetka_val_str otmetka_obj) blocks_to_change_list))
                (ssadd (ssname ss_pikets i) ss_to_highlight) ; Додаємо блок у набір для підсвічування
              )
            )
          )
        )
        (setq i (1+ i))
      )

      (if (> (length blocks_to_change_list) 0)
        (progn
          (sssetfirst nil ss_to_highlight) ; Підсвічуємо знайдені блоки
          (princ "\n\n(Знайдені блоки виділено на кресленні для наочності)")
          (princ "\nНаступні блоки будуть змінені:")
          (foreach item (reverse blocks_to_change_list)
            (princ (strcat "\n - Номер: " (nth 0 item) " - Відмітка: " (nth 1 item)))
          )
          
          (initget "Так Ні")
          (setq user_choice (getkword "\n\nВнести ці зміни? [Так/Ні] <Так>: "))
          
          (if (or (not user_choice) (= user_choice "Так"))
            (progn
              (vla-StartUndoMark doc)
              (setq changed_count 0)
              (princ "\nВнесення змін...")
              
              (foreach item blocks_to_change_list
                (setq otmetka_obj (nth 2 item)
                      current_value_num (distof (nth 1 item))
                )
                (setq new_value_num (- current_value_num 30.34))
                (setq new_value_str (rtos new_value_num 2 2))
                (vla-put-TextString otmetka_obj new_value_str)
                (setq changed_count (1+ changed_count))
              )
              
              (vla-EndUndoMark doc)
              (princ (strcat "\n\nОбробку завершено. Змінено значення у " (itoa changed_count) " блоках."))
              (princ "\nПорада: Ви можете скасувати всі зміни однією командою 'U' (UNDO).")
            )
            (princ "\nЗміни скасовано користувачем.")
          )
        )
        (princ "\n\nБлоків, що відповідають умові (відмітка > 300), не знайдено.")
      )
    )
    (princ "\nБлоків з іменем 'PIKET' на кресленні не знайдено.")
  )

  (setvar "CMDECHO" old_cmdecho)
  (princ)
)

(princ "\nКоманду 'AdjustPiketOtmetka' (v2.1) завантажено. Введіть її для запуску.")
(princ)