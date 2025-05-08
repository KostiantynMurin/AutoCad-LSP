;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; --- Допоміжна функція для вилучення числового значення після "g-" ---
;; Вхід: str-val - рядок для аналізу
;; Повертає: числове значення або nil, якщо "g-" або коректне число не знайдено
(defun Helper:GetGValueFromString (str-val / pos S valid_num_str char val index len has_minus has_dot temp_char_code)
  (if (setq pos (vl-string-search "g-" str-val)) 
    (progn
      (setq S (substr str-val (+ pos 1 (strlen "g-")))) 
      (setq len (strlen S)
            index 1
            valid_num_str ""
            has_minus nil
            has_dot nil
            val nil 
      )
      (while (<= index len)
        (setq char (substr S index 1))
        (setq temp_char_code (ascii char))
        (cond
          ((and (= char "-") (not has_minus) (= (strlen valid_num_str) 0))
           (setq valid_num_str (strcat valid_num_str char) has_minus T)
          )
          ((and (= char ".") (not has_dot))
           (setq valid_num_str (strcat valid_num_str char) has_dot T)
          )
          ((and (>= temp_char_code (ascii "0")) (<= temp_char_code (ascii "9")))
           (setq valid_num_str (strcat valid_num_str char))
          )
          (T (setq index (1+ len)))
        )
        (setq index (1+ index))
      )
      (if (and (> (strlen valid_num_str) 0) 
               (not (equal valid_num_str "-")) (not (equal valid_num_str ".")) (not (equal valid_num_str "-."))
               (if has_dot (wcmatch valid_num_str "*[0-9]*") (if has_minus (> (strlen valid_num_str) 1) T ) )
          )
        (setq val (distof valid_num_str)) 
      )
      val 
    )
    nil 
  )
)

;; --- Основна функція команди ---
(defun c:PlacePiketOffsetNote ( / *error* old-osmode old-cmdecho doc блок-select ent-block data-block block-name 
                                 att-entity data-att att-tag att-value base-value плінія-select ent-pline 
                                 data-pline pline-type calculated-value text-ins-pt 
                                 text-angle text-str text-height text-style cur-layer att-found
                                 gr-box-id gr_result gr_status gr_pt) ; Додано змінні для grread
  
  ;; Локальна функція обробки помилок
  (defun *error* (msg)
    (if gr-box-id (grtext gr-box-id)) ; Стерти тимчасовий текст, якщо він є при помилці
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho)) 
    (if old-osmode (setvar "OSMODE" old-osmode))   
    (if doc (vla-EndUndoMark doc))                 
    (if (not (member msg '("Function cancelled" "quit / exit abort" nil "Вибір точки вставки скасовано користувачем."))) 
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ) 
  )

  (setq old-cmdecho (getvar "CMDECHO")
        old-osmode (getvar "OSMODE")
        doc (vla-get-ActiveDocument (vlax-get-acad-object))
        gr-box-id -1 ; Ініціалізація ідентифікатора для grtext (-1 зазвичай вільний)
  )
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
  
  ;; 3. Вибір полілінії або сплайна
  (setq плінія-select (entsel "\nОберіть полілінію або сплайн (текст буде розміщено горизонтально): "))
  (if (null плінія-select)
    (progn (princ "\nНе обрано полілінію/сплайн.") (*error* "Вибір полілінії/сплайну скасовано"))
  )
  (setq ent-pline (car плінія-select)) 
  (setq data-pline (entget ent-pline))
  (setq pline-type (cdr (assoc 0 data-pline)))
  (if (not (member pline-type '("LWPOLYLINE" "POLYLINE" "SPLINE"))) 
    (progn (princ (strcat "\nОбраний об'єкт ('" pline-type "') не є полілінією або сплайном.")) (*error* "Невірний тип об'єкта для лінії"))
  )
  
  ;; 4. Розрахунок значення для тексту та підготовка рядка тексту
  (setq calculated-value (+ base-value 0.76))
  (setq text-str (rtos calculated-value 2 2)) 
  
  ;; 5. Динамічний вибір точки вставки тексту з попереднім переглядом
  (princ "\nВкажіть точку вставки для тексту (Esc для відміни):")
  (setq text-ins-pt nil)
  ;; (setq gr-box-id 0) ; Використовуємо -1, щоб не конфліктувати з можливими стандартними боксами 0-7
  
  (setvar "osmode" 0) ; Тимчасово вимикаємо OSNAP для плавності перегляду

  (setq gr_result (grread T 8 0)) ; Початкове зчитування події (T - чекати, 8 - RSG_GETUSED, 0 - без бітів керування курсором)

  (while (not text-ins-pt) ; Цикл, поки точка не обрана або не скасовано
    (setq gr_status (car gr_result)
          gr_pt     (cadr gr_result)
    )
    (cond
      ((= gr_status 3) ; Клік миші (точка обрана)
       (grtext gr-box-id) ; Стерти останній тимчасовий текст
       (setq text-ins-pt gr_pt) ; Зберегти точку
      )
      ((= gr_status 5) ; Рух курсору
       (grtext gr-box-id) ; Стерти попередній тимчасовий текст
       (grtext gr-box-id text-str 2) ; Малюємо текст біля курсору (режим 2 для grtext)
      )
      ((and (= gr_status 2) (= (cadr gr_result) 27)) ; Введення з клавіатури - клавіша Esc (ASCII 27)
       (grtext gr-box-id) ; Стерти останній тимчасовий текст
       (setq text-ins-pt :USER_CANCELLED) ; Встановити прапорець скасування
      )
    )
    (if (not text-ins-pt) ; Якщо точка ще не обрана і не скасовано
      (setq gr_result (grread T 8 0)) ; Отримати наступну подію
    )
  )
  (setvar "osmode" old-osmode) ; Відновити попереднє значення OSMODE

  ;; Обробка результату вибору точки
  (if (or (null text-ins-pt) (eq text-ins-pt :USER_CANCELLED))
    (*error* "Вибір точки вставки скасовано користувачем.") 
    ; Це викличе *error* і завершить роботу, якщо точка не вказана або скасовано.
  )
  
  ;; Якщо дійшли сюди, text-ins-pt є валідною точкою
  (setq text-angle 0.0) ; Встановлюємо кут тексту ЗАВЖДИ 0.0 (горизонтальний)
  
  (setq text-height (getvar "TEXTSIZE"))       
  (setq text-style (getvar "TEXTSTYLE"))      
  (setq cur-layer (getvar "CLAYER"))          

  (entmake
    (list
      '(0 . "TEXT")                           
      (cons 1 text-str)                      
      (cons 10 text-ins-pt)                  
      (cons 40 text-height)                  
      (cons 50 text-angle) ; Завжди буде 0.0                   
      (cons 7 text-style)                    
      (cons 8 cur-layer)                     
      '(72 . 0)                               
      '(73 . 0)                               
    )
  )
  
  (princ (strcat "\nСтворено горизонтальний текст: \"" text-str "\".")) 
  
  (*error* nil) ; Нормальне завершення через обробник помилок для очищення (відновлення змінних, EndUndoMark)
)

(defun c:PPON () (c:PlacePiketOffsetNote))

;; Повідомлення про завантаження команди
(princ "\nКоманду 'PlacePiketOffsetNote' завантажено. Введіть PlacePiketOffsetNote в командному рядку для запуску.")
(princ) ; Для чистоти виводу в консоль після завантаження