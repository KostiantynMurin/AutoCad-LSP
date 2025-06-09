;; =================================================================================================
;; |                                                                                               |
;; |          СКРИПТ ДЛЯ АВТОМАТИЧНОЇ РОЗСТАНОВКИ ПРИМІТОК З РОЗРАХУНКОВИХ ВІДСТАНЕЙ                  |
;; |                                                                                               |
;; | Версія: 3.0                                                                            |
;; |                                                                                               |
;; | Опис:                                                                                         |
;; | Скрипт працює в циклічному режимі. Користувач послідовно обирає блоки "PIKET".                  |
;; | Для кожного блоку:                                                                            |
;; | 1. З атрибута "НОМЕРА" вилучається базове значення (формату "г-число").                         |
;; | 2. До нього додається фіксоване значення 0.76.                                                 |
;; | 3. Користувач вказує точку для вставки нового анотативного тексту.                             |
;; | 4. Опрацьований блок "PIKET" автоматично переміщується на шар "22 ГЕОДЕЗИЧНА ОСНОВА".           |
;; |                                                                                               |
;; | Команди для запуску: PlacePiketOffsetNote або PPON                                            |
;; |                                                                                               |
;; =================================================================================================

;; Завантаження функцій Visual LISP, якщо ще не завантажені
(vl-load-com)


;; --- Допоміжна функція для вилучення числового значення після "g-" або "g" ---
;; Вхід: str-val - рядок для аналізу
;; Повертає: числове значення або nil, якщо "g-"/"g" або коректне число не знайдено
(defun Helper:GetGValueFromString (str-val / pos S valid_num_str char val index len has_minus has_dot temp_char_code search_len)
  (if (setq pos (vl-string-search (if (vl-string-search "g-" str-val) "g-" "g") str-val)) ; Шукаємо "g-" АБО "g"
    (progn
      (setq search_len (if (vl-string-search "g-" str-val) (strlen "g-") (strlen "g")))
      (setq S (substr str-val (+ pos 1 search_len))) ; Рядок, що йде після "g-" або "g"
      (setq len (strlen S) index 1 valid_num_str "" has_minus nil has_dot nil val nil)
      ;; Проходимо по символах рядка S
      (while (<= index len)
        (setq char (substr S index 1))
        (setq temp_char_code (ascii char)) ; ASCII-код поточного символу
        (cond
          ;; Дозволяємо один мінус на самому початку числа
          ((and (= char "-") (not has_minus) (= (strlen valid_num_str) 0))
           (setq valid_num_str (strcat valid_num_str char) has_minus T)
          )
          ;; Дозволяємо одну десяткову крапку
          ((and (= char ".") (not has_dot))
           (setq valid_num_str (strcat valid_num_str char) has_dot T)
          )
          ;; Якщо символ - цифра
          ((and (>= temp_char_code (ascii "0")) (<= temp_char_code (ascii "9")))
           (setq valid_num_str (strcat valid_num_str char))
          )
          ;; Інший символ - вважаємо, що числова частина закінчилася
          (T (setq index (1+ len))) ; Примусово завершуємо цикл while
        )
        (setq index (1+ index))
      )
      ;; Перевіряємо, чи сформований valid_num_str є коректним представленням числа
      (if (and (> (strlen valid_num_str) 0) (not (equal valid_num_str "-")) (not (equal valid_num_str ".")) (not (equal valid_num_str "-."))
               (if has_dot (wcmatch valid_num_str "*[0-9]*") (if has_minus (> (strlen valid_num_str) 1) T))
          )
        (setq val (distof valid_num_str)) ; Конвертуємо рядок в число
      )
      val ; Повертаємо число або nil
    )
    nil ; "g-" або "g" не знайдено в початковому рядку
  )
)


;; --- Допоміжна функція для переміщення об'єкта на вказаний шар ---
;; Вхід: ent - ім'я сутності (об'єкта); layer-name - ім'я цільового шару
;; Перевіряє існування шару, створює його при необхідності, і переміщує об'єкт.
(defun Helper:MoveEntityToLayer (ent layer-name / doc layers layer-obj vla-ent-obj)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        layers (vla-get-Layers doc)
  )
  ;; Перевіряємо, чи існує шар. Якщо ні - створюємо його.
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-Item (list layers layer-name)))
    (progn
      (setq layer-obj (vla-Add layers layer-name))
      (princ (strcat "\nСтворено новий шар: '" layer-name "'."))
    )
  )
  ;; Переміщуємо об'єкт на шар
  (if (and ent (setq vla-ent-obj (vlax-ename->vla-object ent)))
    (vla-put-Layer vla-ent-obj layer-name)
  )
  (princ) ; Приховати повернення значення в командний рядок
)


;; --- Основна функція команди (ОНОВЛЕНА) ---
(defun c:PlacePiketOffsetNote ( / *error* old-osmode old-cmdecho doc блок-select ent-block data-block block-name 
                                  att-entity data-att att-tag att-value base-value calculated-value text-ins-pt 
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
  (setq text-style "Д-431"
        text-height 0.75 ; Паперова висота для анотативного тексту
        text-color 4     ; Блакитний (Cyan)
  )
  
  (vla-StartUndoMark doc)
  (setvar "CMDECHO" 0) 
  (princ "\nСкрипт для розстановки приміток. Обирайте блоки 'PIKET' послідовно.")

  ;; Початок основного циклу
  (while (setq блок-select (entsel "\nОберіть блок 'PIKET' (або Enter для завершення): "))
    (setq ent-block (car блок-select))
    (setq data-block (entget ent-block))

    ;; Перевірка, чи є обраний об'єкт блоком "PIKET"
    (if (and data-block 
             (= (cdr (assoc 0 data-block)) "INSERT")
             (equal (strcase "PIKET") (strcase (cdr (assoc 2 data-block))))
        )
      ;; Якщо так, виконуємо основну логіку
      (progn
        ;; Обробка атрибута "НОМЕРА"
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
          )
        )
        (if (not att-found) (princ "\nПопередження: В обраному блоці 'PIKET' відсутній атрибут 'НОМЕРА'."))
        
        ;; Якщо значення не знайдено автоматично, запитуємо користувача
        (if (and att-found (null base-value))
          (progn
            (princ (strcat "\nПідрядок 'g-число' не знайдено в атрибуті 'НОМЕРА' (значення: \"" att-value "\")."))
            (setq base-value (getreal "\nВведіть базове числове значення для розрахунку: "))
          )
        )
        
        ;; Якщо базове значення є, продовжуємо
        (if base-value
          (progn
            ;; Розрахунок і форматування тексту
            (setq calculated-value (+ base-value 0.76))
            (setq num-str-period (rtos calculated-value 2 2)) 
            (setq num-str-comma (vl-string-subst "," "." num-str-period)) 
            (setq text-str (strcat "г-" num-str-comma)) 
            
            ;; Запит точки вставки
            (setq text-ins-pt (getpoint "\nВкажіть точку вставки для тексту: "))
            
            ;; Якщо користувач вказав точку, створюємо текст
            (if text-ins-pt
              (progn
                (setq text-angle 0.0)
                (setq cur-layer (getvar "CLAYER"))
                (entmake
                  (list '(0 . "TEXT") (cons 1 text-str) (cons 10 text-ins-pt) (cons 40 text-height)
                        (cons 50 text-angle) (cons 7 text-style) (cons 8 cur-layer) (cons 62 text-color)
                        '(72 . 0) '(73 . 0)
                  )
                )
                ;; Робимо текст анотативним
                (setq new-text-ename (entlast)) 
                (if new-text-ename
                  (progn
                    (setq new-text-vla-obj (vlax-ename->vla-object new-text-ename))
                    (vl-catch-all-apply 'vlax-put-property (list new-text-vla-obj 'Annotative :vlax-true))
                    (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
                    (setq currentScaleResult (vl-catch-all-apply 'vla-get-CurrentAnnotationScale (list acadDoc)))
                    (if (not (vl-catch-all-error-p currentScaleResult))
                      (progn
                        (setq currentScale currentScaleResult) 
                        (vl-catch-all-apply 'vla-addscale (list new-text-vla-obj currentScale))
                      )
                    )
                  )
                )
                (princ (strcat "\nСтворено текст: \"" text-str "\"."))
                
                ;; Переміщуємо опрацьований блок на інший шар
                (Helper:MoveEntityToLayer ent-block "22 ГЕОДЕЗИЧНА ОСНОВА")
                (princ (strcat " Блок переміщено на шар '22 ГЕОДЕЗИЧНА ОСНОВА'."))

              )
              (princ "\nПропущено. Точку вставки не вказано.")
            )
          )
          (princ "\nПропущено. Не вдалося отримати базове значення для розрахунку.")
        )
      )
      ;; Повідомлення, якщо обраний об'єкт не є блоком "PIKET"
      (princ "\nОбраний об'єкт не є блоком 'PIKET'. Спробуйте ще раз.")
    )
  ) ; Кінець циклу while
  
  (princ "\n\nРоботу завершено.")
  
  ;; Відновлення початкових налаштувань
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE" old-osmode)
  (vla-EndUndoMark doc)
  (princ) 
)

;; Створення короткого псевдоніма (аліаса) для команди
(defun c:PPON () (c:PlacePiketOffsetNote))

;; Повідомлення про успішне завантаження скрипта
(princ "\nКоманду 'PlacePiketOffsetNote' (циклічна версія + переміщення блоків) завантажено. Введіть PlacePiketOffsetNote або PPON.")
(princ)

;;; --- КІНЕЦЬ ФАЙЛУ ---