;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)

;; --- Допоміжна функція для вилучення числового значення після "g-" ---
;; Вхід: str-val - рядок для аналізу
;; Повертає: числове значення або nil, якщо "g-" або коректне число не знайдено
(defun Helper:GetGValueFromString (str-val / pos S valid_num_str char val index len has_minus has_dot temp_char_code)
  (if (setq pos (vl-string-search "g-" str-val)) ; Шукаємо "g-"
    (progn
      (setq S (substr str-val (+ pos 1 (strlen "g-")))) ; Рядок, що йде після "g-"
      (setq len (strlen S)
            index 1
            valid_num_str ""
            has_minus nil
            has_dot nil
            val nil ; Ініціалізуємо результат як nil
      )
      ;; Проходимо по символах рядка S
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
    nil ; "g-" не знайдено в початковому рядку
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
  ;; Зі списку локальних змінних видалено: obj-pline, closest-pt, param, deriv, param-shifted
  
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
  (setq text-height 1.5) ; Паперова висота для анотативного тексту
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
  
  ;; 3. Вибір полілінії або сплайна
  ;; Примітка: полілінія обирається, але її геометрія більше не використовується для визначення кута тексту.
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
  
  ;; 4. Розрахунок значення для тексту та форматування рядка "г-0,00"
  (setq calculated-value (+ base-value 0.76))
  (setq num-str-period (rtos calculated-value 2 2)) ; Число в рядок з крапкою, 2 знаки після крапки
  (setq num-str-comma (vl-string-subst "," "." num-str-period)) ; Заміна крапки на кому
  (setq text-str (strcat "г-" num-str-comma)) ; Додавання префікса "г-"
  
  ;; 5. Точка вставки тексту
  (setq text-ins-pt (getpoint "\nВкажіть точку вставки для тексту: "))
  (if (null text-ins-pt)
    (*error* "Точку вставки тексту не вказано")
  )
  
  ;; Встановлюємо кут тексту ЗАВЖДИ 0.0 (горизонтальний)
  (setq text-angle 0.0) 
  
  ;; !!! Увесь блок коду для розрахунку кута на основі геометрії полілінії ВИДАЛЕНО !!!
  
  ;; Створення текстового об'єкта      
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
  
  (princ (strcat "\nСтворено горизонтальний текст: \"" text-str "\".")) 
  
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE" old-osmode)
  (vla-EndUndoMark doc)
  (princ) 
)

(defun c:PPON () (c:PlacePiketOffsetNote))

;; Повідомлення про завантаження команди
(princ "\nКоманду 'PlacePiketOffsetNote' завантажено. Введіть PlacePiketOffsetNote в командному рядку для запуску.")
(princ) ; Для чистоти виводу в консоль після завантаження