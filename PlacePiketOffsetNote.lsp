;; =================================================================================================
;; |                                                                                               |
;; |                      СКРИПТ ДЛЯ АВТОМАТИЧНОЇ РОЗСТАНОВКИ ПРИМІТОК                             |
;; |                                                                                               |
;; | Версія: 11.0 (Виправлено всі імена змінних. Фінальна версія)                                  |
;; |                                                                                               |
;; =================================================================================================

;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; --- Допоміжна функція для вилучення числового значення ---
(defun Helper:GetGValueFromString (str_val / pos S valid_num_str char val index len has_minus has_dot temp_char_code search_len)
  (if (setq pos (vl-string-search (if (vl-string-search "g-" str_val) "g-" "g") str_val))
    (progn
      (setq search_len (if (vl-string-search "g-" str_val) (strlen "g-") (strlen "g")))
      (setq S (substr str_val (+ pos 1 search_len)))
      (setq len (strlen S) index 1 valid_num_str "" has_minus nil has_dot nil val nil)
      (while (<= index len)
        (setq char (substr S index 1))
        (setq temp_char_code (ascii char))
        (cond
          ((and (= char "-") (not has_minus) (= (strlen valid_num_str) 0)) (setq valid_num_str (strcat valid_num_str char) has_minus T))
          ((and (= char ".") (not has_dot)) (setq valid_num_str (strcat valid_num_str char) has_dot T))
          ((and (>= temp_char_code (ascii "0")) (<= temp_char_code (ascii "9"))) (setq valid_num_str (strcat valid_num_str char)))
          (T (setq index (1+ len)))
        )
        (setq index (1+ index))
      )
      (if (and (> (strlen valid_num_str) 0) (not (equal valid_num_str "-")) (not (equal valid_num_str ".")) (not (equal valid_num_str "-.")) (if has_dot (wcmatch valid_num_str "*[0-9]*") (if has_minus (> (strlen valid_num_str) 1) T)))
        (setq val (distof valid_num_str))
      )
      val
    )
    nil
  )
)

;; --- Допоміжна функція для переміщення об'єкта на шар ---
(defun Helper:MoveEntityToLayer (ent layer_name / doc layers layer_obj vla_ent_obj)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        layers (vla-get-Layers doc)
  )
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-Item (list layers layer_name)))
    (progn (setq layer_obj (vla-Add layers layer_name)) (princ (strcat "\nСтворено новий шар: '" layer_name "'.")))
  )
  (if (and ent (setq vla_ent_obj (vlax-ename->vla-object ent)))
    (vla-put-Layer vla_ent_obj layer_name)
  )
  (princ)
)

;; --- Основна функція команди ---
(defun c:PlacePiketOffsetNote ( / *error* old_osmode old_cmdecho doc блок_select ent_block data_block 
                                  att_entity data_att att_tag att_value base_value calculated_value block_ins_pt
                                  text_str text_height text_style cur_layer att_found p1_angle p2_angle
                                  num_str_period num_str_comma text_color temp_str decimal_pos text_angle pi_val
                                  text_ent text_vla_obj gr_result gr_code gr_pt done current_angle last_pt
                               )
  
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if doc (vla-EndUndoMark doc))
    (if (not (member msg '("Function cancelled" "quit / exit abort" nil)))
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ)
  )

  (setq old_cmdecho (getvar "CMDECHO") old_osmode (getvar "OSMODE") doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq text_style "Д-431" text_height 0.75 text_color 4)
  (setq pi_val 3.14159265358979)
  
  (vla-StartUndoMark doc)
  (setvar "CMDECHO" 0) 
  (princ "\nСкрипт для розстановки приміток. Обирайте блоки 'PIKET' послідовно.")

  (while (setq блок_select (entsel "\nОберіть блок 'PIKET' (або Enter для завершення): "))
    (setq ent_block (car блок_select))
    (setq data_block (entget ent_block))

    (if (and data_block (= (cdr (assoc 0 data_block)) "INSERT") (equal (strcase "PIKET") (strcase (cdr (assoc 2 data_block)))))
      (progn
        (setq base_value nil att_found nil att_value "") 
        (if (= (cdr (assoc 66 data_block)) 1)
          (progn (setq att_entity (entnext ent_block)) (while (and att_entity (not att_found)) (setq data_att (entget att_entity)) (if (and data_att (= (cdr (assoc 0 data_att)) "ATTRIB")) (progn (setq att_tag (cdr (assoc 2 data_att))) (if (equal (strcase "НОМЕРА") (strcase att_tag)) (progn (setq att_value (cdr (assoc 1 data_att))) (setq base_value (Helper:GetGValueFromString att_value)) (setq att_found T)))) (setq att_entity nil)) (if (and att_entity (not att_found)) (setq att_entity (entnext att_entity))))))
        (if (not att_found) (princ "\nПопередження: В обраному блоці 'PIKET' відсутній атрибут 'НОМЕРА'."))
        (if (and att_found (null base_value))
          (progn (princ (strcat "\nПідрядок 'g-число' не знайдено ...")) (setq base_value (getreal "\nВведіть базове числове значення: "))))
        
        (if base_value
          (progn
            (setq calculated_value (+ base_value 0.76))
            (setq temp_str (rtos calculated_value 2 2))
            (setq decimal_pos (vl-string-search "." temp_str))
            (if decimal_pos
              (if (= 1 (- (strlen temp_str) (1+ decimal_pos)))
                (setq num_str_period (strcat temp_str "0"))
                (setq num_str_period temp_str)
              )
              (setq num_str_period (strcat temp_str ".00"))
            )
            (setq num_str_comma (vl-string-subst "," "." num_str_period))
            (setq text_str (strcat "г-" num_str_comma))
            (setq block_ins_pt (cdr (assoc 10 data_block)))

            (if (setq p1_angle (getpoint "\nВкажіть ПЕРШУ точку на лінії для визначення напрямку:"))
              (if (setq p2_angle (getpoint p1_angle "\nВкажіть ДРУГУ точку на тій же лінії:"))
                (progn
                  (setq text_angle (angle p1_angle p2_angle))
                  (if (and (> text_angle (/ pi_val 2.0)) (< text_angle (* 1.5 pi_val)))
                    (setq text_angle (+ text_angle pi_val))
                  )
                  
                  (setq cur_layer (getvar "CLAYER"))
                  (entmake
                    (list
                      '(0 . "TEXT") (cons 1 text_str) (cons 40 text_height) (cons 7 text_style)
                      (cons 8 cur_layer) (cons 62 text_color)
                      (cons 50 text_angle) (cons 10 block_ins_pt) (cons 11 block_ins_pt) '(72 . 1) '(73 . 2)
                    )
                  )
                  (setq text_ent (entlast) text_vla_obj (vlax-ename->vla-object text_ent))
                  
                  (princ (strcat "\nСтворено текст. Переміщуйте курсор. Ліва кнопка - вставити, Права - повернути на 180°."))
                  (setq last_pt block_ins_pt done nil)
                  (while (not done)
                    (setq gr_result (grread T 15 0)
                          gr_code (car gr_result)
                          gr_pt (cadr gr_result)
                    )
                    (cond
                      ((= gr_code 5)
                        (vla-move text_vla_obj (vlax-3d-point last_pt) (vlax-3d-point gr_pt))
                        (setq last_pt gr_pt)
                      )
                      ((= gr_code 25)
                        (setq current_angle (vla-get-Rotation text_vla_obj))
                        (vla-put-Rotation text_vla_obj (+ current_angle pi_val))
                        (princ "\nТекст повернуто на 180°.")
                      )
                      ((= gr_code 3)
                        (setq done T)
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
                  (if (and text_vla_obj (not (vlax-erased-p text_vla_obj)))
                    (progn
                      (Helper:MoveEntityToLayer ent_block "22 ГЕОДЕЗИЧНА ОСНОВА")
                      (princ (strcat " Блок переміщено на шар '22 ГЕОДЕЗИЧНА ОСНОВА'."))
                    )
                  )
                )
                (princ "\nПропущено. Друга точка для напрямку не вказана.")
              )
              (princ "\nПропущено. Перша точка для напрямку не вказана.")
            )
          )
          (princ "\nПропущено. Не вдалося отримати базове значення.")
        )
      )
      (princ "\nОбраний об'єкт не є блоком 'PIKET'.")
    )
  )
  
  (princ "\n\nРоботу завершено.")
  (setvar "CMDECHO" old_cmdecho)
  (setvar "OSMODE" old_osmode)
  (vla-EndUndoMark doc)
  (princ)
)

(defun c:PPON () (c:PlacePiketOffsetNote))

(princ "\nОновлену команду 'PlacePiketOffsetNote' (v11.0, фінальна) завантажено.")
(princ)