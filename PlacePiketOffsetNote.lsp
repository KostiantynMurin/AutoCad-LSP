;; =================================================================================================
;; |                                                                                               |
;; |                      СКРИПТ ДЛЯ АВТОМАТИЧНОЇ РОЗСТАНОВКИ ПРИМІТОК                             |
;; |                                                                                               |
;; | Версія: 9.1 (Додано вибір режиму повороту тексту)                                             |
;; |                                                                                               |
;; | Опис:                                                                                         |
;; | Додано можливість перемикати режим орієнтації тексту (Автоматична читабельність / Як є).      |
;; | Для перемикання введіть "Р" у відповідь на запит про вибір блока. Скрипт запам'ятовує вибір.   |
;; |                                                                                               |
;; | Команди для запуску: PlacePiketOffsetNote або PPON                                            |
;; |                                                                                               |
;; =================================================================================================

;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; === НОВЕ: Глобальна змінна для зберігання режиму повороту ===
;; Якщо змінна ще не існує, створюємо її зі значенням "Авто"
(if (not *PPON-Rotation-Mode*)
  (setq *PPON-Rotation-Mode* "Авто")
)
;; =============================================================


;; --- Допоміжна функція для вилучення числового значення ---
(defun Helper:GetGValueFromString (str-val / ...)
  ;; ... (код цієї функції не змінюється) ...
)


;; --- Допоміжна функція для переміщення об'єкта на шар ---
(defun Helper:MoveEntityToLayer (ent layer-name / ...)
  ;; ... (код цієї функції не змінюється) ...
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
  (princ "\nСкрипт для розстановки приміток. Поточний режим повороту: ") (princ *PPON-Rotation-Mode*)

  ;; === ЗМІНЕНО: Основний цикл з можливістю зміни режиму ===
  (while 
    (progn
      (initget "Режим_повороту")
      (setq блок-select (entsel (strcat "\nОберіть блок 'PIKET' або [Режим_повороту]: ")))
    )
    
    (cond
      ;; Випадок 1: Користувач обрав змінити режим
      ((= блок-select "Режим_повороту")
        (if (equal *PPON-Rotation-Mode* "Авто")
          (setq *PPON-Rotation-Mode* "Як є")
          (setq *PPON-Rotation-Mode* "Авто")
        )
        (princ (strcat "\nРежим повороту змінено на: " *PPON-Rotation-Mode*))
      )
      
      ;; Випадок 2: Користувач обрав блок
      ((listp блок-select)
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

                (if (setq p1_angle (getpoint "\nВкажіть ПЕРШУ точку на лінії для визначення напрямку:"))
                  (if (setq p2_angle (getpoint p1_angle "\nВкажіть ДРУГУ точку на тій же лінії:"))
                    (progn
                      (setq text_angle (angle p1_angle p2_angle))
                      
                      ;; === ЗМІНЕНО: Перевірка на читабельність залежить від режиму ===
                      (if (equal *PPON-Rotation-Mode* "Авто")
                        (if (and (> text_angle (/ pi_val 2.0)) (< text_angle (* 1.5 pi_val)))
                          (setq text_angle (+ text_angle pi_val))
                        )
                      )
                      ;; Якщо режим "Як є", нічого не робимо, залишаємо кут як є.
                      
                      (setq cur-layer (getvar "CLAYER"))
                      (entmake
                        (list
                          '(0 . "TEXT") (cons 1 text_str) (cons 40 text_height) (cons 7 text_style)
                          (cons 8 cur-layer) (cons 62 text_color)
                          (cons 50 text_angle)
                          (cons 10 block_ins_pt) (cons 11 block_ins_pt) '(72 . 1) '(73 . 2)
                        )
                      )
                      
                      (princ (strcat "\nСтворено текст: \"" text_str "\". Вкажіть його кінцеве положення."))
                      (command "_MOVE" "_LAST" "" block_ins_pt PAUSE)
                      
                      (Helper:MoveEntityToLayer ent-block "22 ГЕОДЕЗИЧНА ОСНОВА")
                      (princ (strcat " Блок переміщено на шар '22 ГЕОДЕЗИЧНА ОСНОВА'."))
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
    )
  )
  
  (princ "\n\nРоботу завершено.")
  (setvar "CMDECHO" old_cmdecho) (setvar "OSMODE" old-osmode) (vla-EndUndoMark doc) (princ) 
)

(defun c:PPON () (c:PlacePiketOffsetNote))

(princ "\nОновлену команду 'PlacePiketOffsetNote' (v9.1, з вибором режиму) завантажено.")
(princ)