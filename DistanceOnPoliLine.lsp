; Скрипт для вимірювання довжини сегмента полілінії між двома вказаними точками
; Команда для запуску: DPL (Distance on PolyLine)

(vl-load-com) ; Переконатись, що функції ActiveX/VLA завантажені

(defun c:DPL ( / *error* old_osmode old_cmdecho ent obj pt1 pt2 param1 param2 dist1 dist2 seg_length )

  ; Функція обробки помилок
  (defun *error* ( msg )
    (if old_osmode (setvar "OSMODE" old_osmode)) ; Відновити старі налаштування прив'язки
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho)) ; Відновити старі налаштування відображення команд
    (cond
      ((not msg)) ; Вихід без помилки (наприклад, натискання Esc)
      ((wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")) ; Нормальний вихід користувача
      (T (princ (strcat "\nПомилка: " msg))) ; Показати повідомлення про помилку
    )
    (princ) ; Тихий вихід
  )

  ; Зберегти поточні налаштування
  (setq old_osmode (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0) ; Вимкнути відображення команд для чистоти

  ; --- Вибір полілінії ---
  (setq ent nil)
  (while (not ent)
    (setq ent (entsel "\nВиберіть полілінію: "))
    (if ent
      (progn
        (setq obj (vlax-ename->vla-object (car ent)))
        (if (not (wcmatch (vla-get-ObjectName obj) "*Polyline")) ; Перевірка, чи це полілінія (LWPOLYLINE або POLYLINE 2D/3D)
          (progn
            (prompt "\nОбраний об'єкт не є полілінією. Спробуйте ще раз.")
            (setq ent nil) ; Скинути вибір, щоб цикл продовжився
          )
        )
      )
      (prompt "\nОб'єкт не вибрано.") ; Якщо користувач натиснув Enter/Esc без вибору
    )
  )

  (if ent ; Продовжувати тільки якщо полілінію вибрано
    (progn
      ; --- Вибір першої точки на полілінії ---
      (setvar "OSMODE" 512) ; Увімкнути прив'язку "Найближча" (NEArest)
      (setq pt1 (getpoint "\nВкажіть першу точку на полілінії (використовуйте прив'язку 'Найближча'): "))
      (if pt1
        (progn
          ; Переконатись, що точка точно на полілінії
          (setq pt1 (vlax-curve-getClosestPointTo obj pt1))
          ; Отримати відстань від початку полілінії до першої точки
          (setq dist1 (vlax-curve-getDistAtPoint obj pt1))

          ; --- Вибір другої точки на полілінії ---
          (setq pt2 (getpoint pt1 "\nВкажіть другу точку на полілінії (використовуйте прив'язку 'Найближча'): "))
          (if pt2
            (progn
              ; Переконатись, що точка точно на полілінії
              (setq pt2 (vlax-curve-getClosestPointTo obj pt2))
              ; Отримати відстань від початку полілінії до другої точки
              (setq dist2 (vlax-curve-getDistAtPoint obj pt2))

              ; --- Обчислення та виведення результату ---
              (setq seg_length (abs (- dist2 dist1))) ; Довжина - це різниця відстаней по модулю

              (alert (strcat "Довжина сегмента полілінії між точками: " (rtos seg_length 2 4))) ; Показати результат у вікні
              (prompt (strcat "\nДовжина сегмента полілінії між точками: " (rtos seg_length 2 4))) ; Показати результат в командному рядку
            )
            (prompt "\nДругу точку не вказано.")
          )
        )
        (prompt "\nПершу точку не вказано.")
      )
    )
    ; (prompt "\nПолілінію не вибрано або обрано невірний об'єкт.") ; Це повідомлення вже обробляється у циклі while
  )

  ; Відновити старі налаштування
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  (princ) ; Тихий вихід з функції
)

(princ "\nСкрипт DPL завантажено. Введіть DPL для запуску.") ; Оновлене повідомлення
(princ)