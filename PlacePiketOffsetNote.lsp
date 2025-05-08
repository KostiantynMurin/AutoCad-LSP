;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; --- Допоміжна функція для вилучення числового значення після "g-" ---
;; Вхід: str-val - рядок для аналізу
;; Повертає: числове значення або nil, якщо "g-" або коректне число не знайдено
(defun Helper:GetGValueFromString (str-val / pos S valid_num_str char val index len has_minus has_dot temp_char_code)
  (if (setq pos (vl-string-search "g-" str-val)) 
    (progn
      (setq S (substr str-val (+ pos 1 (strlen "g-")))) 
      (setq len (strlen S) index 1 valid_num_str "" has_minus nil has_dot nil val nil )
      (while (<= index len)
        (setq char (substr S index 1)) (setq temp_char_code (ascii char))
        (cond
          ((and (= char "-") (not has_minus) (= (strlen valid_num_str) 0)) (setq valid_num_str (strcat valid_num_str char) has_minus T))
          ((and (= char ".") (not has_dot)) (setq valid_num_str (strcat valid_num_str char) has_dot T))
          ((and (>= temp_char_code (ascii "0")) (<= temp_char_code (ascii "9"))) (setq valid_num_str (strcat valid_num_str char)))
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
                                 gr-box-id gr_result gr_status gr_pt
                                 ;; Нові змінні для стилізації та анотативності
                                 num-str-period num-str-comma text-color
                                 new-text-ename new-text-vla-obj acadDoc currentScale currentScaleResult
                               ) 
  
  (defun *error* (msg)
    (if gr-box-id (grtext gr-box-id)) 
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
        gr-box-id -1 
        ;; Встановлення параметрів стилізації
        text-style "Д-431"
        text-height 1.5 ; Паперова висота для анотативного тексту
        text-color 4    ; Блакитний (Cyan)
  )
  (vla-StartUndoMark doc)
  (setvar "CMDECHO" 0) 

  (princ "\nСкрипт для розстановки анотативної примітки (стиль Д-431, блакитний).")

  ;; 1. Вибір блоку "PIKET"
  (setq блок-select (entsel "\nОберіть блок 'PIKET': "))
  (if (null блок-select) (progn (princ "\nНе обрано жодного об'єкта.") (*error* "Вибір блоку скасовано")))
  (setq ent-block (car блок-select) data-block (entget ent-block))
  (if (not (and data-block (= "INSERT" (cdr (assoc 0 data-block))))) (progn (princ "\nОбраний об'єкт не є блоком.") (*error* "Невірний тип об'єкта (не блок)")))
  (setq block-name (cdr (assoc 2 data-block)))
  (if (not (equal (strcase "PIKET") (strcase block-name))) (progn (princ (strcat "\nОбрано блок '" block-name "', але очікувався блок 'PIKET'.")) (*error* "Невірне ім'я блоку")))

  ;; 2. Обробка атрибута "НОМЕРА"
  (setq base-value nil att-found nil att-value "") 
  (if (= 1 (cdr (assoc 66 data-block))) 
    (progn
      (setq att-entity (entnext ent-block)) 
      (while (and att-entity (not att-found)) 
        (setq data-att (entget att-entity))
        (if (and data-att (= "ATTRIB" (cdr (assoc 0 data-att)))) 
          (progn
            (setq att-tag (cdr (assoc 2 data-att))) 
            (if (equal (strcase "НОМЕРА") (strcase att-tag))
              (progn (setq att-value (cdr (assoc 1 data-att)) base-value (Helper:GetGValueFromString att-value) att-found T ))))
          (setq att-entity nil) )
        (if (and att-entity (not att-found)) (setq att-entity (entnext att-entity))) )
      (if (not att-found) (progn (princ "\nВ обраному блоці 'PIKET' відсутній атрибут 'НОМЕРА'.") (*error* "Атрибут 'НОМЕРА' не знайдено")))
    )
    (progn (princ "\nВ обраному блоці 'PIKET' відсутні атрибути.") (*error* "Атрибути відсутні у блоці")))
  (if (and att-found (null base-value))
    (progn
      (princ (strcat "\nПідрядок 'g-число' не знайдено або некоректний в атрибуті 'НОМЕРА' (значення: \"" att-value "\")."))
      (setq base-value (getreal "\nВведіть базове числове значення для розрахунку: "))
      (if (null base-value) (*error* "Значення для розрахунку не введено користувачем"))))
  
  ;; 3. Вибір полілінії або сплайна
  (setq плінія-select (entsel "\nОберіть полілінію або сплайн (текст буде розміщено горизонтально): "))
  (if (null плінія-select) (progn (princ "\nНе обрано полілінію/сплайн.") (*error* "Вибір полілінії/сплайну скасовано")))
  (setq ent-pline (car плінія-select) data-pline (entget ent-pline) pline-type (cdr (assoc 0 data-pline)))
  (if (not (member pline-type '("LWPOLYLINE" "POLYLINE" "SPLINE"))) (progn (princ (strcat "\nОбраний об'єкт ('" pline-type "') не є полілінією або сплайном.")) (*error* "Невірний тип об'єкта для лінії")))
  
  ;; 4. Розрахунок значення для тексту та форматування рядка "г-0,00"
  (setq calculated-value (+ base-value 0.76))
  (setq num-str-period (rtos calculated-value 2 2)) ; Число в рядок з крапкою, 2 знаки після крапки
  (setq num-str-comma (vl-string-subst "," "." num-str-period)) ; Заміна крапки на кому
  (setq text-str (strcat "г-" num-str-comma)) ; Додавання префікса "г-"
  
  ;; 5. Динамічний вибір точки вставки тексту з попереднім переглядом
  (princ "\nВкажіть точку вставки для тексту (Esc для відміни):")
  (setq text-ins-pt nil)
  (setvar "osmode" 0) 
  (setq gr_result (grread T 8 0)) 
  (while (not text-ins-pt) 
    (setq gr_status (car gr_result) gr_pt (cadr gr_result))
    (cond
      ((= gr_status 3) (if gr-box-id (grtext gr-box-id)) (setq text-ins-pt gr_pt))
      ((= gr_status 5) (if gr-box-id (grtext gr-box-id)) (grtext gr-box-id text-str 2))
      ((and (= gr_status 2) (= 27 (cadr gr_result))) (if gr-box-id (grtext gr-box-id)) (setq text-ins-pt :USER_CANCELLED)))
    (if (not text-ins-pt) (setq gr_result (grread T 8 0))))
  (setvar "osmode" old-osmode) 

  (if (or (null text-ins-pt) (eq text-ins-pt :USER_CANCELLED))
    (*error* "Вибір точки вставки скасовано користувачем.") )
  
  (setq text-angle 0.0) 
  (setq cur-layer (getvar "CLAYER"))          

  (entmake
    (list
      '(0 . "TEXT")                           
      (cons 1 text-str)                      
      (cons 10 text-ins-pt)                  
      (cons 40 text-height) ; Задана паперова висота 1.5                 
      (cons 50 text-angle) ; Завжди 0.0                   
      (cons 7 text-style)  ; Стиль "Д-431"                  
      (cons 8 cur-layer)                     
      (cons 62 text-color) ; Колір 4 (Cyan)
      '(72 . 0)                               
      '(73 . 0)                               
    )
  )
  (setq new-text-ename (entlast)) ; Отримуємо ім'я щойно створеного текстового об'єкта

  ;; Робимо текст анотативним та додаємо поточний масштаб
  (if new-text-ename
      (progn
          (setq new-text-vla-obj (vlax-ename->vla-object new-text-ename))
          ;; Встановлюємо властивість Annotative
          (vl-catch-all-apply 'vlax-put-property (list new-text-vla-obj 'Annotative :vlax-true))
          
          ;; Додаємо поточний анотативний масштаб
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
  
  (*error* nil) 
)

(defun c:PPON () (c:PlacePiketOffsetNote))

;; Повідомлення про завантаження команди
(princ "\nКоманду 'PlacePiketOffsetNote' завантажено. Введіть PlacePiketOffsetNote або PPON в командному рядку для запуску.")
(princ) ; Для чистоти виводу в консоль після завантаження