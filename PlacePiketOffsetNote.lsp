;; =================================================================================================
;; |                                                                                               |
;; |                      СКРИПТ ДЛЯ АВТОМАТИЧНОЇ РОЗСТАНОВКИ ПРИМІТОК                             |
;; |                                                                                               |
;; | Версія: 9.0 (Напрямок тексту задається вручну двома точками)                                  |
;; |                                                                                               |
;; | Опис:                                                                                         |
;; | Після вибору блока 'PIKET', скрипт просить вказати дві точки на будь-якій лінії,              |
;; | щоб задати паралельний напрямок для створюваного тексту.                                      |
;; |                                                                                               |
;; | Команди для запуску: PlacePiketOffsetNote або PPON                                            |
;; |                                                                                               |
;; =================================================================================================

;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)


;; --- Допоміжна функція для вилучення числового значення після "g-" або "g" ---
(defun Helper:GetGValueFromString (str-val / pos S valid_num_str char val index len has_minus has_dot temp_char_code search_len)
  ;; ... (код цієї функції не змінюється) ...
  (if (setq pos (vl-string-search (if (vl-string-search "g-" str-val) "g-" "g") str-val))
    (progn
      (setq search_len (if (vl-string-search "g-" str-val) (strlen "g-") (strlen "g")))
      (setq S (substr str-val (+ pos 1 search_len)))
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


;; --- Допоміжна функція для переміщення об'єкта на вказаний шар ---
(defun Helper:MoveEntityToLayer (ent layer-name / doc layers layer-obj vla-ent-obj)
  ;; ... (код цієї функції не змінюється) ...
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) layers (vla-get-Layers doc))
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-Item (list layers layer-name)))
    (progn (setq layer-obj (vla-Add layers layer-name)) (princ (strcat "\nСтворено новий шар: '" layer-name "'.")))
  )
  (if (and ent (setq vla-ent-obj (vlax-ename->vla-object ent)))
    (vla-put-Layer vla-ent-obj layer-name)
  )
  (princ)
)


;; --- Основна функція команди ---
(defun c:PlacePiketOffsetNote ( / *error* old-osmode old-cmdecho doc блок-select ent-block data-block 
                                  att-entity data-att att-tag att-value base-value calculated-value block_ins_pt
                                  text-str text-height text-style cur-layer att-found p1_angle p2_angle
                                  num-str-period num-str-comma text-color temp_str decimal_pos text_angle pi_val
                               )
  
  (defun *error* (msg)
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho)) (if old-osmode (setvar "OSMODE" old-osmode)) (if doc (vla-EndUndoMark doc)) (if (not (member msg '("Function cancelled" "quit / exit abort" nil))) (princ (strcat "\nПомилка виконання: " msg))) (princ))

  (setq old-cmdecho (getvar "CMDECHO") old-osmode (getvar "OSMODE") doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq text-style "Д-431" text-height 0.75 text-color 4)
  (setq pi_val 3.14159265358979)
  
  (vla-StartUndoMark doc)
  (setvar "CMDECHO" 0) 
  (princ "\nСкрипт для розстановки приміток. Обирайте блоки 'PIKET' послідовно.")

  (while (setq блок-select (entsel "\nОберіть блок 'PIKET' (або Enter для завершення): "))
    (setq ent-block (car блок-select))
    (setq data-block (entget ent-block))

    (if (and data-block (= (cdr (assoc 0 data-block)) "INSERT") (equal (strcase "PIKET") (strcase (cdr (assoc 2 data-block)))))
      (progn
        (setq base-value nil att-found nil att-value "") 
        (if (= (cdr (assoc 66 data-block)) 1)
          (progn (setq att-entity (entnext ent-block)) (while (and att-entity (not att-found)) (setq data-att (entget att-entity)) (if (and data-att (= (cdr (assoc 0 data-att)) "ATTRIB")) (progn (setq att-tag (cdr (assoc 2 data-att))) (if (equal (strcase "НОМЕРА") (strcase att-tag)) (progn (setq att-value (cdr (assoc 1 data-att))) (setq base-value (Helper:GetGValueFromString att-value)) (setq att-found T)))) (setq att-entity nil)) (if (and att-entity (not att-found)) (setq att-entity (entnext att-entity))))))
        (if (not att-found) (princ "\nПопередження: В обраному блоці 'PIKET' відсутній атрибут 'НОМЕРА'."))
        (if (and att-found (null base-value))
          (progn (princ (strcat "\nПідрядок 'g-число' не знайдено ...")) (setq base-value (getreal "\nВведіть базове числове значення: "))))
        
        (if base-value
          (progn
            (setq calculated-value (+ base-value 0.76))
            (setq temp_str (rtos calculated-value 2 2)) (setq decimal_pos (vl-string-search "." temp_str)) (if decimal_pos (if (= 1 (- (strlen temp_str) (1+ decimal_pos))) (setq num-str-period (strcat temp_str "0")) (setq num-str-period temp_str) ) (setq num-str-period (strcat temp_str ".00")))
            (setq num-str-comma (vl-string-subst "," "." num-str-period)) (setq text-str (strcat "г-" num-str-comma))
            (setq block_ins_pt (cdr (assoc 10 data-block)))

            ;; === НОВИЙ БЛОК: Визначення кута за двома точками, вказаними вручну ===
            (if (setq p1_angle (getpoint "\nВкажіть ПЕРШУ точку на лінії для визначення напрямку:"))
              (if (setq p2_angle (getpoint p1_angle "\nВкажіть ДРУГУ точку на тій же лінії:"))
                (progn
                  (setq text_angle (angle p1_angle p2_angle))
                  ;; Перевірка на читабельність
                  (if (and (> text_angle (/ pi_val 2.0)) (< text_angle (* 1.5 pi_val)))
                    (setq text_angle (+ text_angle pi_val))
                  )
                  
                  (setq cur-layer (getvar "CLAYER"))
                  (entmake
                    (list
                      '(0 . "TEXT") (cons 1 text-str) (cons 40 text-height) (cons 7 text-style)
                      (cons 8 cur-layer) (cons 62 text-color)
                      (cons 50 text_angle) ; <-- Застосовуємо обчислений кут
                      (cons 10 block_ins_pt) (cons 11 block_ins_pt) '(72 . 1) '(73 . 2)
                    )
                  )
                  
                  (princ (strcat "\nСтворено текст: \"" text-str "\". Вкажіть його кінцеве положення."))
                  (command "_MOVE" "_LAST" "" block_ins_pt PAUSE)
                  
                  (Helper:MoveEntityToLayer ent-block "22 ГЕОДЕЗИЧНА ОСНОВА")
                  (princ (strcat " Блок переміщено на шар '22 ГЕОДЕЗИЧНА ОСНОВА'."))
                )
                (princ "\nПропущено. Друга точка для напрямку не вказана.")
              )
              (princ "\nПропущено. Перша точка для напрямку не вказана.")
            )
            ;; ====================================================================
          )
          (princ "\nПропущено. Не вдалося отримати базове значення.")
        )
      )
      (princ "\nОбраний об'єкт не є блоком 'PIKET'.")
    )
  )
  
  (princ "\n\nРоботу завершено.")
  (setvar "CMDECHO" old-cmdecho) (setvar "OSMODE" old-osmode) (vla-EndUndoMark doc) (princ) 
)

(defun c:PPON () (c:PlacePiketOffsetNote))

(princ "\nОновлену команду 'PlacePiketOffsetNote' (v9.0, ручний напрямок) завантажено.")
(princ)