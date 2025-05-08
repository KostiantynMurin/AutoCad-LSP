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
               ;; Якщо є крапка, то має бути хоча б одна цифра поруч,
               ;; або це має бути формат типу "1." чи ".1" (distof це обробить)
               (if has_dot
                 (wcmatch valid_num_str "*[0-9]*") ; Чи є хоч одна цифра в рядку
                 ;; Якщо крапки немає, але є мінус, то має бути щось після мінуса
                 (if has_minus
                   (> (strlen valid_num_str) 1)
                   T ; Якщо немає ні крапки, ні мінуса, просто перевіряємо, що не порожній (вже зроблено)
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
                                 data-pline pline-type obj-pline calculated-value text-ins-pt closest-pt 
                                 param deriv text-angle text-str text-height text-style cur-layer att-found param-shifted)
  
  ;; Локальна функція обробки помилок
  (defun *error* (msg)
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho)) ; Відновлення попереднього значення CMDECHO
    (if old-osmode (setvar "OSMODE" old-osmode))   ; Відновлення попереднього значення OSMODE
    (if doc (vla-EndUndoMark doc))                 ; Завершення групи відміни AutoCAD
    (if (not (member msg '("Function cancelled" "quit / exit abort" nil))) ; Не виводити повідомлення для стандартних відмін
      (princ (strcat "\nПомилка виконання: " msg))
    )
    (princ) ; Тихий вихід з функції *error*
  )

  ;; Збереження поточних налаштувань та початок групи відміни
  (setq old-cmdecho (getvar "CMDECHO")
        old-osmode (getvar "OSMODE")
        doc (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  (vla-StartUndoMark doc)
  (setvar "CMDECHO" 0) ; Вимкнення виводу команд в командний рядок

  (princ "\nСкрипт для розстановки примітки з розрахунковою відстанню.")

  ;; 1. Вибір блоку "PIKET"
  (setq блок-select (entsel "\nОберіть блок 'PIKET': "))
  (if (null блок-select) ; Якщо користувач нічого не вибрав (натиснув Esc)
    (progn (princ "\nНе обрано жодного об'єкта.") (*error* "Вибір блоку скасовано"))
  )
  (setq ent-block (car блок-select))
  (setq data-block (entget ent-block))

  ;; Перевірка, чи обраний об'єкт - блок
  (if (not (and data-block (= (cdr (assoc 0 data-block)) "INSERT")))
    (progn (princ "\nОбраний об'єкт не є блоком.") (*error* "Невірний тип об'єкта (не блок)"))
  )
  ;; Перевірка імені блоку (без урахування регістру)
  (setq block-name (cdr (assoc 2 data-block)))
  (if (not (equal (strcase "PIKET") (strcase block-name))) 
    (progn (princ (strcat "\nОбрано блок '" block-name "', але очікувався блок 'PIKET'.")) (*error* "Невірне ім'я блоку"))
  )

  ;; 2. Обробка атрибута "НОМЕРА"
  (setq base-value nil att-found nil att-value "") ; Ініціалізація змінних
  (if (= (cdr (assoc 66 data-block)) 1) ; Перевірка, чи є у блоку атрибути (DXF код 66)
    (progn
      (setq att-entity (entnext ent-block)) ; Отримуємо перший атрибут
      ;; Цикл по всіх атрибутах блоку
      (while (and att-entity (not att-found)) ; Продовжуємо, поки є атрибути І потрібний не знайдено
        (setq data-att (entget att-entity))
        (if (and data-att (= (cdr (assoc 0 data-att)) "ATTRIB")) ; Перевірка, чи це дійсно атрибут
          (progn
            (setq att-tag (cdr (assoc 2 data-att))) ; Ім'я (тег) атрибута
            ;; Порівняння тегу з "НОМЕРА" (без урахування регістру)
            (if (equal (strcase "НОМЕРА") (strcase att-tag))
              (progn
                (setq att-value (cdr (assoc 1 data-att))) ; Значення атрибута
                (setq base-value (Helper:GetGValueFromString att-value)) ; Спроба вилучити "g-число"
                (setq att-found T) ; Позначка, що атрибут знайдено
              )
            )
          )
          (setq att-entity nil) ; Якщо це не атрибут (напр. кінець послідовності sub-entities), зупинити цикл
        )
        (if (and att-entity (not att-found)) ; Якщо ще не знайдено і є куди йти
            (setq att-entity (entnext att-entity)) ; Перехід до наступного атрибута
        )
      )
      ;; Якщо після циклу атрибут "НОМЕРА" так і не було знайдено
      (if (not att-found)
        (progn (princ "\nВ обраному блоці 'PIKET' відсутній атрибут 'НОМЕРА'.") (*error* "Атрибут 'НОМЕРА' не знайдено"))
      )
    )
    ;; Якщо у блоку взагалі немає атрибутів
    (progn (princ "\nВ обраному блоці 'PIKET' відсутні атрибути.") (*error* "Атрибути відсутні у блоці"))
  )

  ;; Якщо атрибут "НОМЕРА" було знайдено, але base-value = nil (тобто "g-число" не знайдено/некоректне)
  (if (and att-found (null base-value))
    (progn
      (princ (strcat "\nПідрядок 'g-число' не знайдено або некоректний в атрибуті 'НОМЕРА' (значення: \"" att-value "\")."))
      (setq base-value (getreal "\nВведіть базове числове значення для розрахунку: "))
      (if (null base-value) ; Якщо користувач не ввів значення
        (*error* "Значення для розрахунку не введено користувачем")
      )
    )
  )
  
  ;; 3. Вибір полілінії або сплайна
  (setq плінія-select (entsel "\nОберіть полілінію або сплайн: "))
  (if (null плінія-select)
    (progn (princ "\nНе обрано полілінію/сплайн.") (*error* "Вибір полілінії/сплайну скасовано"))
  )
  (setq ent-pline (car плінія-select))
  (setq data-pline (entget ent-pline))
  (setq pline-type (cdr (assoc 0 data-pline)))
  ;; Перевірка типу обраного об'єкта
  (if (not (member pline-type '("LWPOLYLINE" "POLYLINE" "SPLINE"))) 
    (progn (princ (strcat "\nОбраний об'єкт ('" pline-type "') не є полілінією або сплайном.")) (*error* "Невірний тип об'єкта для лінії"))
  )
  
  ;; 4. Розрахунок значення для тексту (додавання 0.76)
  (setq calculated-value (+ base-value 0.76))
  
  ;; 5. Точка вставки тексту
  (setq text-ins-pt (getpoint "\nВкажіть точку вставки для тексту: "))
  (if (null text-ins-pt)
    (*error* "Точку вставки тексту не вказано")
  )
  
  ;; Визначення кута нахилу тексту паралельно до полілінії/сплайна
  (setq text-angle 0.0) ; Кут за замовчуванням (горизонтальний)
  (setq obj-pline (vlax-ename->vla-object ent-pline)) ; Конвертація в VLA-об'єкт
  
  (if (and obj-pline (vlax-method-applicable-p obj-pline 'getClosestPointTo)) ; Чи підтримує об'єкт потрібні методи
    (progn
      ;; Використовуємо vl-catch-all-apply для безпечного виклику функцій роботи з кривими
      (setq closest-pt (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj-pline text-ins-pt)))
      (if (not (vl-catch-all-error-p closest-pt)) ; Якщо не було помилки при пошуку найближчої точки
        (progn
          (setq param (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list obj-pline closest-pt)))
          (if (not (vl-catch-all-error-p param)) ; Якщо не було помилки при отриманні параметра
            (progn
              (setq deriv (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list obj-pline param))) ; Перша похідна (тангента)
              ;; Перевірка, чи похідна валідна і не нульовий вектор
              (if (and (not (vl-catch-all-error-p deriv)) deriv (not (equal deriv '(0.0 0.0 0.0) 1e-9)))
                (setq text-angle (angle '(0.0 0.0 0.0) deriv)) ; Кут тангенти
                (progn ; Якщо похідна нульова, спробувати невелике зміщення параметра
                  (setq param-shifted (if (< param (vlax-curve-getEndParam obj-pline)) (+ param 1e-6) (- param 1e-6)))
                  (setq deriv (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list obj-pline param-shifted)))
                  (if (and (not (vl-catch-all-error-p deriv)) deriv (not (equal deriv '(0.0 0.0 0.0) 1e-9)))
                    (setq text-angle (angle '(0.0 0.0 0.0) deriv))
                    (princ "\nПопередження: Не вдалося точно визначити напрямок лінії в точці. Текст буде горизонтальним.")
                  )
                )
              )
            )
            (princ (strcat "\nПопередження: Не вдалося отримати параметр на кривій. Текст буде горизонтальним. (" (vl-catch-all-error-message param) ")"))
          )
        )
        (princ (strcat "\nПопередження: Не вдалося знайти найближчу точку на кривій. Текст буде горизонтальним. (" (vl-catch-all-error-message closest-pt) ")"))
      )
    )
    (princ "\nПопередження: Обраний тип об'єкта не підтримує автоматичне визначення кута. Текст буде горизонтальним.")
  )
  
  ;; Створення текстового об'єкта
  (setq text-str (rtos calculated-value 2 2)) ; Перетворення числа в рядок (2 знаки після коми)
  (setq text-height (getvar "TEXTSIZE"))       ; Поточна висота тексту
  (setq text-style (getvar "TEXTSTYLE"))      ; Поточний стиль тексту
  (setq cur-layer (getvar "CLAYER"))          ; Поточний шар

  (entmake
    (list
      '(0 . "TEXT")                           ; Тип об'єкта - Текст
      (cons 1 text-str)                      ; Текстовий рядок
      (cons 10 text-ins-pt)                  ; Точка вставки (DXF код 10)
      (cons 40 text-height)                  ; Висота тексту (DXF код 40)
      (cons 50 text-angle)                   ; Кут повороту тексту в радіанах (DXF код 50)
      (cons 7 text-style)                    ; Стиль тексту (DXF код 7)
      (cons 8 cur-layer)                     ; Шар (DXF код 8)
      '(72 . 0)                               ; Горизонтальне вирівнювання - Ліворуч (DXF 72)
      '(73 . 0)                               ; Вертикальне вирівнювання - Базова лінія (DXF 73)
    )
  )
  
  (princ (strcat "\nСтворено текст: \"" text-str "\" з кутом: " (angtos text-angle 0 4) " градусів.")) ; Вивід кута в градусах
  
  ;; Відновлення попередніх налаштувань AutoCAD та завершення групи відміни
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE" old-osmode)
  (vla-EndUndoMark doc)
  (princ) ; Тихий вихід з команди
)

(defun c:PPON () (c:PlacePiketOffsetNote))

;; Повідомлення про завантаження команди
(princ "\nКоманду 'PlacePiketOffsetNote' завантажено. Введіть PlacePiketOffsetNote в командному рядку для запуску.")
(princ) ; Для чистоти виводу в консоль після завантаження