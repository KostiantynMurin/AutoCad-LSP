;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; --- Допоміжна функція для вилучення числового значення після першого знайденого "g-" або "g" ---
;; Вхід: str-val - рядок для аналізу
;; Повертає: числове значення або nil, якщо "g-"/"g" не знайдено або після них немає коректного числа
(defun Helper:GetGValueFromString (str-val / pos S valid_num_str char val index len has_minus has_dot temp_char_code g_found)
  (setq g_found nil
        pos (vl-string-search "g-" str-val)
  )
  (if pos
    (setq S (substr str-val (+ pos 2))
          g_found T)
    (setq pos (vl-string-search "g" str-val))
    (if pos
      (setq S (substr str-val (+ pos 1))
            g_found T)
    )
  )

  (if g_found
    (progn
      (setq len (strlen S)
            index 1
            valid_num_str ""
            has_minus nil
            has_dot nil
            val nil ; Ініціалізуємо результат як nil
      )
      ;; Проходимо по символах рядка S, доки не зустрінемо нечисловий символ (крім першого '-')
      (while (<= index len)
        (setq char (substr S index 1))
        (setq temp_char_code (ascii char)) ; ASCII-код поточного символу

        (cond
          ;; Дозволяємо один мінус на самому початку числа
          ((and (= char "-") (not has_minus) (= (strlen valid_num_str) 0))
           (setq valid_num_str (strcat valid_num_str char)
                 has_minus T
           )
          )
          ;; Дозволяємо одну десяткову крапку
          ((and (= char ".") (not has_dot))
           (setq valid_num_str (strcat valid_num_str char)
                 has_dot T
           )
          )
          ;; Якщо символ - цифра
          ((and (>= temp_char_code (ascii "0")) (<= temp_char_code (ascii "9")))
           (setq valid_num_str (strcat valid_num_str char))
          )
          ;; Інший символ - вважаємо, що числова частина закінчилася
          (T
           (setq index (1+ len)) ; Примусово завершуємо цикл while
          )
        )
        (setq index (1+ index))
      )

      ;; Перевіряємо, чи сформований valid_num_str є коректним представленням числа
      (if (and (> (strlen valid_num_str) 0) ; Не порожній
               (not (equal valid_num_str "-")) ; Не просто "-"
               (not (equal valid_num_str ".")) ; Не просто "."
               (not (equal valid_num_str "-.")) ; Не просто "-."
               (if has_dot
                 (wcmatch valid_num_str "*[0-9]*")
                 (if has_minus
                   (> (strlen valid_num_str) 1)
                   T
                 )
               )
          )
        (setq val (distof valid_num_str)) ; Конвертуємо рядок в число
      )
      val ; Повертаємо число або nil
    )
    nil ; "g-" або "g" не знайдено в рядку
  )
)

;; --- Основна функція команди ---
(defun c:PlacePiketOffsetNote ( / *error* old-osmode old-cmdecho doc блок-select ent-block data-block block-name 
                                 att-entity data-att att-tag att-value base-value 
                                 ;; Змінні для полілінії видалені: плінія-select ent-pline data-pline pline-type
                                 calculated-value text-ins-pt 
                                 text-angle text-str text-height text-style cur-layer att-found
                                 num-str-period num-str-comma text-color
                                 new-text-ename new-text-vla-obj acadDoc currentScale currentScaleResult
                               )
  
  ;; Локальна функція обробки помилок
  (defun *error* (msg)
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho)) 
    (if old-osmode (setvar "OSMODE" old-osmode))   
    (if doc (vla-EndUndoMark doc))                 
    (if (not (member msg '("Function cancelled" "quit / exit abort" nil))) 
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ) 
  )

  (setq old-cmdecho (getvar "CMDECHO")
        old-osmode (getvar "OSMODE")
        doc (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  
  ;; Встановлення параметрів стилізації
  (setq text-style "Д-431")
  (setq text-height 0.75) ; Паперова висота для анотативного тексту
  (setq text-color 4)    ; Блакитний (Cyan)
  
  (vla-StartUndoMark doc)
  (setvar "CMDECHO" 0) 

  (princ "\nСкрипт для розстановки примітки з розрахунковою відстанню (текст буде горизонтальним).")

  ;; 1. Вибір блоку "PIKET"
  (setq блок-select (entsel "\nОберіть блок 'PIKET': "))
  (if (null блок-select) 
    (progn (princ "\nНе обрано жодного об'єкта.") (*error* "Вибір блоку скасовано"))
  )
  (setq ent-block (car блок-select))
  (setq data-block (entget ent-block))

  (if (not (and data-block (= (cdr (assoc 0 data-block)) "INSERT")))
    (progn (princ "\nОбраний об'єкт не є блоком.") (*error* "Невірний тип об'єкта (не блок)"))
  )
  (setq block-name (cdr (assoc 2 data-block)))
  (if (not (equal (strcase "PIKET") (strcase block-name))) 
    (progn (princ (strcat "\nОбрано блок '" block-name "', але очікувався блок 'PIKET'.")) (*error* "Невірне ім'я блоку"))
  )

  ;; 2. Обробка атрибута "НОМЕРА"
  (setq base-value nil att-found nil att-value "") 
  (if (= (cdr (assoc 66 data-block)) 1) 
    (progn
      (setq att-entity (entnext ent-block)) 
      (while (and att-entity (not att-found)) 
        (setq data-att (entget att-entity))
        (if (and data-att (= (cdr (assoc 0 data-att)) "ATTRIB")) 
          (progn
            (setq att-tag (cdr (assoc 2 data-att))) 
            (if (equal (strcase "НОМЕРА") (strcase att-tag))
              (progn
                (setq att-value (cdr (assoc 1 data-att))) 
                (setq base-value (Helper:GetGValueFromString att-value)) 
                (setq att-found T) 
              )
            )
          )
          (setq att-entity nil) 
        )
        (if (and att-entity (not att-found)) 
            (setq att-entity (entnext att-entity)) 
        )
      )
      (if (not att-found)
        (progn (princ "\nВ обраному блоці 'PIKET' відсутній атрибут 'НОМЕРА'.") (*error* "Атрибут 'НОМЕРА' не знайдено"))
      )
    )
    (progn (princ "\nВ обраному блоці 'PIKET' відсутні атрибути.") (*error* "Атрибути відсутні у блоці"))
  )

  (if (and att-found (null base-value))
    (progn
      (princ (strcat "\nПідрядок 'g-число' не знайдено або некоректний в атрибуті 'НОМЕРА' (значення: \"" att-value "\")."))
      (setq base-value (getreal "\nВведіть базове числове значення для розрахунку: "))
      (if (null base-value) 
        (*error* "Значення для розрахунку не введено користувачем")
      )
    )
  )
  
  ;; !!! БЛОК ВИБОРУ ПОЛІЛІНІЇ ВИДАЛЕНО !!!
  
  ;; 3. Розрахунок значення для тексту та форматування рядка "г-0,00" (раніше був крок 4)
  (setq calculated-value (+ base-value 0.76))
  (setq num-str-period (rtos calculated-value 2 2)) 
  (setq num-str-comma (vl-string-subst "," "." num-str-period)) 
  (setq text-str (strcat "г-" num-str-comma)) 
  
  ;; 4. Точка вставки тексту (раніше був крок 5)
  (setq text-ins-pt (getpoint "\nВкажіть точку вставки для тексту: "))
  (if (null text-ins-pt)
    (*error* "Точку вставки тексту не вказано")
  )
  
  ;; Встановлюємо кут тексту ЗАВЖДИ 0.0 (горизонтальний)
  (setq text-angle 0.0) 
  
  ;; Створення текстового об'єкта      
  (setq cur-layer (getvar "CLAYER"))          

  (entmake
    (list
      '(0 . "TEXT")                           
      (cons 1 text-str)                      
      (cons 10 text-ins-pt)                  
      (cons 40 text-height)                
      (cons 50 text-angle)                   
      (cons 7 text-style)                    
      (cons 8 cur-layer)                     
      (cons 62 text-color) 
      '(72 . 0)                               
      '(73 . 0)                               
    )
  )
  
  (setq new-text-ename (entlast)) 

  ;; Робимо текст анотативним та додаємо поточний масштаб
  (if new-text-ename
      (progn
          (setq new-text-vla-obj (vlax-ename->vla-object new-text-ename))
          (vl-catch-all-apply 'vlax-put-property (list new-text-vla-obj 'Annotative :vlax-true))
          (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
          (setq currentScaleResult (vl-catch-all-apply 'vla-get-CurrentAnnotationScale (list acadDoc)))
          (if (not (vl-catch-all-error-p currentScaleResult))
            (progn
              (setq currentScale currentScaleResult) 
              (if (and currentScale new-text-vla-obj (vlax-method-applicable-p new-text-vla-obj 'addscale))
                (vl-catch-all-apply 'vla-addscale (list new-text-vla-obj currentScale))
                (princ "\nПопередження: Не вдалося додати поточний анотативний масштаб до тексту (об'єкт або метод 'addscale' не доступний).")
              )
            )
            (princ (strcat "\nПопередження: Не вдалося отримати поточний анотативний масштаб: " (vl-catch-all-error-message currentScaleResult)))
          )
      )
      (princ "\nПопередження: Не вдалося отримати щойно створений текстовий об'єкт для налаштування анотативності.")
  )
  
  (princ (strcat "\nСтворено анотативний горизонтальний текст: \"" text-str "\" стилем '" text-style "'.")) 
  
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE" old-osmode)
  (vla-EndUndoMark doc)
  (princ) 
)

(defun c:PPON () (c:PlacePiketOffsetNote))

;; Повідомлення про завантаження команди
(princ "\nКоманду 'PlacePiketOffsetNote' (версія без вибору полілінії) завантажено. Введіть PlacePiketOffsetNote або PPON для запуску.")
(princ)