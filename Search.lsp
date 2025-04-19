;; --- LISP Скрипт для Пошуку Блоків та Наступної Вставки з Буфера (v2) ---
;; --- Додано функцію CHECKPOINTS (v3) ---

;; Глобальна змінна для зберігання результату пошуку
(setq *g_last_search_result* nil)

;; --- Допоміжна функція: Заміна всіх входжень підрядка (чутлива до регістру) ---
;; Замінює всі входження рядка 'find' на рядок 'replace' у рядку 'source'.
;; Повертає новий рядок зі змінами.
(defun str-replace (find replace source / len-f pos result rest)
  (setq len-f (strlen find))
  (if (= 0 len-f) ; Якщо шуканий рядок порожній, нічого не робити
      source
      (progn
        (setq result "" rest source)
        (while (setq pos (vl-string-search find rest 0)) ; Шукати find у залишку rest
          (setq result (strcat result (substr rest 1 pos) replace)) ; Додати частину до знахідки + заміну
          (setq rest (substr rest (+ pos len-f 1))) ; Оновити залишок (після знайденого рядка)
        )
        (strcat result rest) ; Додати залишок, де більше немає входжень find
      )
  )
)

;; ====================================================================
;; СКРИПТ 1: ПОШУК ТА ВИДІЛЕННЯ БЛОКІВ (v4 - Пошук за початком рядка)
;; ====================================================================
;; Команда: SEARCH
;; Шукає блоки "PIKET" за атрибутом "НОМЕРА", ВИДІЛЯЄ їх, якщо значення
;; атрибута ПОЧИНАЄТЬСЯ з ключового слова,
;; та ЗБЕРІГАЄ набір вибірки у глобальну змінну *g_last_search_result*.

(defun c:SEARCH ( / keyword ssAll i ename edata bname hasAttribs
                  attEname attEdata attTag attValue ssFound foundCount )
  ;; --- Отримати Введення Користувача ---
  (setq keyword (getstring T "\nВведіть ПОЧАТОК значення атрибуту 'НОМЕРА' для пошуку: ")) ; Змінено підказку

  ;; Скинути попередній результат перед новим пошуком
  (setq *g_last_search_result* nil)

  ;; --- Перевірити, чи Надано Ключове Слово ---
  (if (and keyword (/= "" keyword))
    (progn ;; Продовжити, тільки якщо ключове слово не порожнє
      (princ (strcat "\nШукаю блоки 'PIKET', де атрибут 'НОМЕРА' починається з '" keyword "'...")) ; Змінено повідомлення

      ;; --- Ініціалізація ---
      (setq ssFound (ssadd)) ;; Створити порожній набір вибірки для результатів
      (setq foundCount 0)    ;; Ініціалізувати лічильник знайдених

      ;; --- Отримати Всі Об'єкти ---
      (setq ssAll (ssget "_X"))

      ;; --- Пошук відповідних блоків ---
      (if ssAll
        (progn
          (setq i 0)
          (repeat (sslength ssAll)
            (setq ename (ssname ssAll i)) ;; Отримати ім'я об'єкта
            (if (not (null (entget ename))) ; Перевірка існування об'єкта
              (progn
                (setq edata (entget ename))   ;; Отримати дані об'єкта (DXF коди)
                (if (eq "INSERT" (cdr (assoc 0 edata)))
                  (progn
                    (setq bname (strcase (cdr (assoc 2 edata)))) ;; Назва блоку
                    (if (eq "PIKET" bname)
                      (progn
                        (setq hasAttribs (assoc 66 edata))
                        (if (and hasAttribs (= 1 (cdr hasAttribs)))
                          (progn
                            (setq attEname (entnext ename)) ;; Перший атрибут
                            (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                              (setq attTag (strcase (cdr (assoc 2 attEdata)))) ;; Тег
                              (setq attValue (cdr (assoc 1 attEdata)))       ;; Значення
                              (if (and (eq "НОМЕРА" attTag)
                                       attValue
                                       ;; ***** ПОЧАТОК ЗМІН: Використання wcmatch *****
                                       ;; Перевіряємо, чи значення атрибута (в верхньому регістрі)
                                       ;; відповідає шаблону "КЛЮЧОВЕСЛОВО*" (також в верхньому регістрі)
                                       (wcmatch (strcase attValue) (strcat (strcase keyword) "*"))
                                       ;; ***** КІНЕЦЬ ЗМІН *****
                                  )
                                (progn
                                  (ssadd ename ssFound) ;; Додати блок до вибірки
                                  (setq foundCount (1+ foundCount))
                                  (setq attEname nil) ; Зупинити цикл while для поточного блоку
                                )
                              )
                              (if attEname (setq attEname (entnext attEname))) ;; Наступний атрибут
                            ) ;; кінець while
                          ) ;; кінець progn (має атрибути)
                        ) ;; кінець if (має атрибути)
                      ) ;; кінець progn (ім'я PIKET)
                    ) ;; кінець if (ім'я PIKET)
                  ) ;; кінець progn (це INSERT)
                ) ;; кінець if (це INSERT)
              ) ; кінець progn (об'єкт існує)
            ) ; кінець if (об'єкт існує)
            (setq i (1+ i)) ;; Наступний об'єкт
          ) ;; кінець repeat
        ) ;; кінець progn (ssAll існує)
        (princ "\nУ кресленні немає об'єктів для пошуку.")
      ) ;; кінець if (ssAll існує)

      ;; --- Фіналізація: Виділення, Звіт та Збереження результату ---
      (if (> foundCount 0)
        (progn
          ;; Зберегти знайдений набір вибірки у глобальну змінну
          (setq *g_last_search_result* ssFound)
          ;; ВИДІЛИТИ знайдені об'єкти для користувача
          (sssetfirst nil ssFound)
          (princ (strcat "\nЗнайдено та ВИДІЛЕНО " (itoa foundCount) " блоків. Результат збережено для команди CHECKPOINTS, PASTEHERE або REPLACENAME."))
        )
        (progn ;; Якщо нічого не знайдено
           (setq *g_last_search_result* nil) ; Переконатися, що результат порожній
           (princ (strcat "\nБлоки 'PIKET', де атрибут 'НОМЕРА' починається з '" keyword "', не знайдено.")) ; Змінено повідомлення
        )
      )
    ) ;; кінець progn (ключове слово надано)
    (princ "\nКлючове слово не введено. Пошук скасовано.") ;; Повідомлення, якщо ключове слово порожнє
  ) ;; кінець if (перевірка ключового слова)
  (princ) ;; Чистий вихід
) ;; кінець defun c:SEARCH


;; ====================================================================
;; СКРИПТ 2: ВСТАВКА В ТОЧКИ ЗБЕРЕЖЕНОГО РЕЗУЛЬТАТУ ПОШУКУ
;; ====================================================================
;; Команда: PASTEHERE
;; Бере набір вибірки, збережений командою SEARCH у змінній *g_last_search_result*,
;; і вставляє в точку вставки кожного об'єкта з цього набору
;; об'єкт(и) з буфера обміну (які були скопійовані за допомогою COPYBASE).
;; (Код цієї функції залишається без змін)
(defun c:PASTEHERE ( / *error* ss i ename edata ipoint oldCmdEcho oldOsmode pasteCount )

  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho)) ; Відновити CMDECHO
    (if oldOsmode (setvar "OSMODE" oldOsmode))   ; Відновити OSMODE
    (command "_.UNDO" "_End")                    ; Завершити групування UNDO
    (cond ((not msg))                            ; Вихід без повідомлення (наприклад, ESC)
          ((vl-string-search "Function cancelled" msg)) ; Користувач скасував
          ((vl-string-search "quit / exit abort" msg))  ; Користувач скасував
          (T (princ (strcat "\nПомилка: " msg)))        ; Інша помилка
    )
    (setq *error* nil) ; Скинути обробник помилок
    (princ)
  )

  ;; --- Перевірка буфера обміну ---
  (if (= 0 (getvar "CLIPROPS")) ; Перевіряє, чи містить буфер дані AutoCAD
    (progn
      (alert "Буфер обміну порожній або не містить даних AutoCAD.\nСпочатку скопіюйте об'єкт(и) за допомогою 'Копіювати з базовою точкою' (_COPYBASE).")
      (exit) ; Вийти зі скрипта
    )
  )

  ;; --- Перевірка наявності та дійсності збережених результатів пошуку ---
  (if (and (boundp '*g_last_search_result*) ; Чи змінна існує?
           *g_last_search_result* ; Чи вона не nil?
           (= 'PICKSET (type *g_last_search_result*)) ; Чи це дійсно набір вибірки?
           (> (sslength *g_last_search_result*) 0) ; Чи набір не порожній?
      )
    (progn ;; Є дійсні результати пошуку
      (setq ss *g_last_search_result*) ;; Використати збережений набір вибірки
      (princ (strcat "\nВикористовується збережений результат пошуку з " (itoa (sslength ss)) " об'єктів."))
      (princ "\nВставлення об'єкта(ів) з буфера обміну в точки їх вставки...")

      ;; --- Налаштування середовища та UNDO ---
      (setq oldCmdEcho (getvar "CMDECHO"))
      (setq oldOsmode (getvar "OSMODE"))
      (setvar "CMDECHO" 0) ;; Вимкнути ехо команд
      (setvar "OSMODE" 0)  ;; Вимкнути об'єктну прив'язку
      (command "_.UNDO" "_Begin") ;; Почати групування UNDO
      (setq pasteCount 0) ;; Лічильник успішних вставок

      ;; --- Цикл по Збережених Об'єктах та Вставка ---
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i)) ;; Отримати ім'я об'єкта з набору вибірки
        (if (setq edata (entget ename)) ; Перевірити, чи об'єкт ще існує
          (progn
            ;; Перевірка, чи це блок (INSERT)
            (if (eq "INSERT" (cdr (assoc 0 edata)))
              (progn
                (setq ipoint (cdr (assoc 10 edata))) ;; Отримати точку вставки блоку
                (if ipoint
                  (progn
                    (command "_.PASTECLIP" ipoint) ;; Вставити з буфера в точку вставки
                    (setq pasteCount (1+ pasteCount)) ;; Збільшити лічильник
                  )
                  ;; Попередження, якщо не вдалося отримати точку вставки
                  (princ (strcat "\n Попередження: Не вдалося отримати точку вставки для об'єкта: " (vl-princ-to-string ename)))
                )
              )
              ;; Попередження, якщо об'єкт у вибірці - не блок
              (princ (strcat "\n Попередження: Об'єкт у збереженій вибірці не є блоком (INSERT): " (vl-princ-to-string ename)))
            )
          )
          ;; Попередження, якщо об'єкт з вибірки вже не існує
          (princ (strcat "\n Попередження: Об'єкт зі збереженої вибірки вже не існує: " (vl-princ-to-string ename)))
        )
        (setq i (1+ i)) ;; Наступний об'єкт у вибірці
      ) ;; кінець циклу repeat

      ;; --- Відновлення середовища та Завершення UNDO ---
      (setvar "CMDECHO" oldCmdEcho)
      (setvar "OSMODE" oldOsmode)
      (command "_.UNDO" "_End") ;; Завершити групування UNDO

      ;; --- Фінальне повідомлення ---
      (princ (strcat "\nЗавершено. Об'єкт(и) з буфера обміну вставлено " (itoa pasteCount) " раз(ів)."))

    )
    ;; --- Якщо Результатів Пошуку Немає або вони недійсні ---
    (princ "\nНе знайдено дійсних результатів попереднього пошуку. Спочатку виконайте команду SEARCH.")
  ) ;; кінець if (перевірка *g_last_search_result*)

  ;; --- Очистка та вихід ---
  (setq *error* nil) ; Скинути обробник помилок
  (princ) ;; Чистий вихід
) ;; кінець defun c:PASTEHERE

;; ====================================================================
;; СКРИПТ 3: ПЕРЕВІРКА ТА ВИПРАВЛЕННЯ КООРДИНАТИ Z ЗА АТРИБУТОМ "ОТМЕТКА"
;; ====================================================================
;; Команда: CHECKPOINTS
;; Бере набір вибірки, збережений командою SEARCH у змінній *g_last_search_result*.
;; Для кожного блоку "PIKET" у наборі порівнює значення атрибута "ОТМЕТКА"
;; з координатою Z точки вставки блоку.
;; Виводить звіт про розбіжності, ВИДІЛЯЄ тільки блоки з розбіжностями
;; та запитує дозвіл на виправлення Z координати.

(defun c:CHECKPOINTS ( / *error* ss totalCount i ename edata ipoint zCoord
                       attEname attEdata attTag otmetkaStr otmetkaNum
                       diffList diffCount fuzz modCount answer oldCmdecho
                       ssDiff ; <--- Додана локальна змінна для нового набору вибірки
                     )

  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    ;; Відновити CMDECHO, якщо воно було змінене
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    ;; Якщо група UNDO була розпочата, але не завершена коректно
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command "_.UNDO" "_End"))
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\nПомилка: " msg)))
    )
    (setq *error* nil)
    (princ)
  )

  ;; --- Ініціалізація ---
  (setq diffList nil         ; Список блоків з розбіжностями [(ename . (otmetkaStr . zCoord))]
        diffCount 0        ; Лічильник блоків з розбіжностями
        fuzz 1e-6          ; Допуск для порівняння дійсних чисел
        modCount 0         ; Лічильник змінених блоків
        oldCmdecho (getvar "CMDECHO")
  )
  (setvar "CMDECHO" 0) ; Тимчасово вимкнути ехо команд

  ;; --- Перевірка наявності та дійсності збережених результатів пошуку ---
  (if (and (boundp '*g_last_search_result*)
           *g_last_search_result*
           (= 'PICKSET (type *g_last_search_result*))
           (> (sslength *g_last_search_result*) 0)
      )
    (progn ;; Є дійсні результати пошуку
      (setq ss *g_last_search_result*)
      (setq totalCount (sslength ss))
      (princ (strcat "\nПеревірка " (itoa totalCount) " блоків зі збереженого результату пошуку..."))

      ;; --- Цикл по Збережених Об'єктах ---
      (setq i 0)
      (repeat totalCount
        (setq ename (ssname ss i))
        (if (setq edata (entget ename)) ; Перевірити, чи об'єкт ще існує
          (if (eq "INSERT" (cdr (assoc 0 edata))) ; Перевірити, чи це блок
            (progn
              (setq ipoint (cdr (assoc 10 edata))) ; Отримати точку вставки
              (setq zCoord (caddr ipoint))       ; Отримати Z координату
              (setq otmetkaStr nil)              ; Скинути значення атрибута для поточного блоку

              ;; --- Пошук атрибута "ОТМЕТКА" ---
              (if (and (assoc 66 edata) (= 1 (cdr (assoc 66 edata)))) ; Чи є атрибути?
                (progn
                  (setq attEname (entnext ename))
                  (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                    (setq attTag (strcase (cdr (assoc 2 attEdata))))
                    (if (eq "ОТМЕТКА" attTag)
                      (progn
                        (setq otmetkaStr (cdr (assoc 1 attEdata))) ; Зберегти значення атрибута
                        (setq attEname nil) ; Зупинити пошук атрибутів для цього блоку
                      )
                      (setq attEname (entnext attEname)) ; Наступний атрибут
                    )
                  ) ; end while
                )
              ) ; end if has attributes

              ;; --- Порівняння значення атрибута та Z координати ---
              (if otmetkaStr
                (progn
                  (setq otmetkaNum (distof otmetkaStr)) ; Спробувати конвертувати рядок в число
                  (if otmetkaNum ; Перевірка, чи вдалося конвертувати
                    ;; Порівняти з допуском
                    (if (not (equal zCoord otmetkaNum fuzz))
                      (progn
                        ;; Додати інформацію про блок до списку розбіжностей
                        (setq diffList (cons (cons ename (cons otmetkaStr zCoord)) diffList))
                        (setq diffCount (1+ diffCount))
                      )
                    )
                    (princ (strcat "\n Попередження: Не вдалося конвертувати значення '" otmetkaStr "' атрибута 'ОТМЕТКА' в число для блоку: " (vl-princ-to-string ename)))
                  )
                )
                (princ (strcat "\n Попередження: Не знайдено атрибут 'ОТМЕТКА' для блоку: " (vl-princ-to-string ename)))
              )
            ) ; end progn (is insert)
            (princ (strcat "\n Попередження: Об'єкт у збереженій вибірці не є блоком (INSERT): " (vl-princ-to-string ename)))
          ) ; end if (is insert)
          (princ (strcat "\n Попередження: Об'єкт зі збереженої вибірки вже не існує: " (vl-princ-to-string ename)))
        ) ; end if (entget)
        (setq i (1+ i))
      ) ; end repeat

      ;; --- Звіт про результати перевірки та ВИДІЛЕННЯ ПРОБЛЕМНИХ ---
      (if (= diffCount 0)
        (progn
           (princ (strcat "\nПеревірку завершено. Всі " (itoa totalCount) " блоків мають відповідну координату Z та значення атрибуту 'ОТМЕТКА'."))
           ;; Зняти виділення, якщо розбіжностей немає
           (sssetfirst nil nil)
        )
        (progn ;; Знайдено розбіжності
          (princ (strcat "\nПеревірку завершено. Всього перевірено: " (itoa totalCount) " блоків."))
          (princ (strcat "\nЗнайдено розбіжностей у Z-координаті та атрибуті 'ОТМЕТКА': " (itoa diffCount) " блоків."))
          (princ "\nСписок блоків з розбіжностями (Атрибут 'ОТМЕТКА' | Координата Z):")
          ;; Вивести список блоків з розбіжностями
          (foreach item diffList
            (princ (strcat "\n  - Блок <" (vl-princ-to-string (car item)) ">: '" (car (cdr item)) "' | " (rtos (cdr (cdr item)))))
          )

          ;; ***** ПОЧАТОК ЗМІН: Створення та виділення нового набору *****
          (setq ssDiff (ssadd)) ; Створити порожній набір
          (if diffList ; Перевірка, чи список не порожній (хоча diffCount > 0 вже це гарантує)
              (foreach item diffList
                 (if (entget (car item)) ; Додаткова перевірка існування об'єкта перед додаванням
                    (ssadd (car item) ssDiff) ; Додати ename кожного блоку з розбіжностями
                 )
              )
          )
          (if (> (sslength ssDiff) 0)
             (progn
                (princ (strcat "\nВиділено " (itoa (sslength ssDiff)) " блоків з розбіжностями."))
                (sssetfirst nil ssDiff) ; Виділити тільки ці блоки
             )
             (progn
                (princ "\nНе вдалося створити набір вибірки для виділення блоків з розбіжностями.")
                (sssetfirst nil nil)    ; Якщо з якоїсь причини ssDiff порожній, зняти виділення
             )
          )
          ;; ***** КІНЕЦЬ ЗМІН *****

          ;; --- Запит на виправлення ---
          (initget "Yes No") ; Дозволити тільки ці відповіді
          (setq answer (getkword "\n\nЗмінити координату Z для цих блоків відповідно до атрибуту 'ОТМЕТКА'? [Yes/No]: "))

          (if (eq answer "Yes")
            (progn
              ;; --- Виконання змін ---
              (princ "\nВиконую зміни...")
              ;; *** Код роботи з UNDOCTL видалено, покладаємось на налаштування AutoCAD ***
              (command "_.UNDO" "_Begin") ; Почати групування UNDO

              (foreach item diffList
                 (setq ename (car item))
                 (setq otmetkaStr (car (cdr item)))
                 (if (setq edata (entget ename))
                    (progn
                      (setq ipoint (cdr (assoc 10 edata)))
                      (setq otmetkaNum (distof otmetkaStr))
                      (if otmetkaNum
                        (progn
                          (setq new_ipoint (list (car ipoint) (cadr ipoint) otmetkaNum))
                          (setq edata (subst (cons 10 new_ipoint) (assoc 10 edata) edata))
                          (if (entmod edata)
                            (setq modCount (1+ modCount))
                            (princ (strcat "\n Помилка зміни Z координати для блоку: " (vl-princ-to-string ename)))
                          )
                        )
                      )
                    )
                    (princ (strcat "\n Помилка: Не вдалося отримати дані для блоку " (vl-princ-to-string ename) " під час спроби зміни."))
                 )
              ) ; end foreach item

              (command "_.UNDO" "_End") ; Завершити групування UNDO
              (princ (strcat "\nУспішно змінено Z координату для " (itoa modCount) " з " (itoa diffCount) " блоків."))
              ;; *** Код відновлення UNDOCTL видалено ***

            ) ; end progn (answer = Yes)
            (princ "\nЗміни не виконувались.")
          ) ; end if (answer = Yes)
        ) ; end progn (diffCount > 0)
      ) ; end if (= diffCount 0)
    ) ; end progn (results are valid)
    ;; --- Якщо Результатів Пошуку Немає або вони недійсні ---
    (princ "\nНе знайдено дійсних результатів попереднього пошуку (з команди SEARCH). Спочатку виконайте команду SEARCH.")
  ) ; end if (check *g_last_search_result*)

  ;; --- Відновлення середовища та вихід ---
  (setvar "CMDECHO" oldCmdecho) ; Відновити CMDECHO
  (setq *error* nil) ; Скинути обробник помилок
  (princ) ;; Чистий вихід
) ;; кінець defun c:CHECKPOINTS


;; ====================================================================
;; СКРИПТ 4: ЗАМІНА ПІДСТРОКИ В АТРИБУТІ "НОМЕРА"
;; ====================================================================
;; Команда: REPLACENAME
;; Бере набір вибірки, збережений командою SEARCH у змінній *g_last_search_result*.
;; Запитує підстроку для пошуку та підстроку для заміни.
;; Замінює всі входження першої підстроки на другу у значенні
;; атрибута "НОМЕРА" для кожного блоку у вибірці.

(defun c:REPLACENAME ( / *error* ss i ename edata
                       attEname attEdata attTag currentVal newVal
                       findStr replaceStr modCount oldCmdecho )

  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho)) ; Відновити CMDECHO
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command "_.UNDO" "_End")) ; Завершити UNDO якщо активне
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\nПомилка: " msg)))
    )
    (setq *error* nil)
    (princ)
  )

  ;; --- Ініціалізація ---
  (setq modCount 0) ; Лічильник змінених блоків
  (setq oldCmdecho (getvar "CMDECHO"))
  ;(setvar "CMDECHO" 0) ; Можна вимкнути ехо, якщо бажано

  ;; --- Перевірка наявності та дійсності збережених результатів пошуку ---
  (if (and (boundp '*g_last_search_result*)
           *g_last_search_result*
           (= 'PICKSET (type *g_last_search_result*))
           (> (sslength *g_last_search_result*) 0)
      )
    (progn ;; Є дійсні результати пошуку
      (setq ss *g_last_search_result*)
      (princ (strcat "\nБуде оброблено " (itoa (sslength ss)) " блоків зі збереженого результату пошуку."))

      ;; --- Отримати рядки для пошуку та заміни ---
      (setq findStr (getstring T "\nВведіть підстроку, ЯКУ замінити в атрибуті 'НОМЕРА': "))
      (if (= "" findStr)
          (progn
            (princ "\nПомилка: Підстрока для пошуку не може бути порожньою. Заміну скасовано.")
            (exit)
          )
      )
      (setq replaceStr (getstring T "\nВведіть підстроку, НА ЯКУ замінити: "))

      (princ (strcat "\nВиконую заміну '" findStr "' на '" replaceStr "' в атрибутах 'НОМЕРА'..."))

      ;; --- Початок групи UNDO ---
      (command "_.UNDO" "_Begin")

      ;; --- Цикл по Збережених Об'єктах ---
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i))
        (if (setq edata (entget ename)) ; Перевірити, чи об'єкт ще існує
          (if (and (eq "INSERT" (cdr (assoc 0 edata))) ; Перевірити, чи це блок
                   (assoc 66 edata) (= 1 (cdr (assoc 66 edata)))) ; і чи має атрибути
            (progn
              ;; --- Пошук атрибута "НОМЕРА" та його зміна ---
              (setq attEname (entnext ename))
              (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                (setq attTag (strcase (cdr (assoc 2 attEdata)))) ; Тег (для перевірки)

                (if (eq "НОМЕРА" attTag)
                  (progn
                    (setq currentVal (cdr (assoc 1 attEdata))) ; Поточне значення атрибута
                    ;; Виконати заміну за допомогою допоміжної функції
                    (setq newVal (str-replace findStr replaceStr currentVal))

                    ;; Якщо значення змінилося, оновити атрибут
                    (if (not (eq currentVal newVal))
                      (progn
                        (setq attEdata (subst (cons 1 newVal) (assoc 1 attEdata) attEdata))
                        (if (entmod attEdata)
                            (setq modCount (1+ modCount)) ; Збільшити лічильник успішних змін
                            (princ (strcat "\n Помилка оновлення атрибута для блоку: " (vl-princ-to-string ename)))
                        )
                      )
                      ;(princ (strcat "\n У блоці " (vl-princ-to-string ename) " підстроку '" findStr "' не знайдено в атрибуті 'НОМЕРА'."))
                    )
                    (setq attEname nil) ; Зупинити пошук атрибутів для цього блоку
                  )
                  ;; Якщо це не атрибут "НОМЕРА", перейти до наступного
                  (setq attEname (entnext attEname))
                ) ; кінець if (eq "НОМЕРА" attTag)
              ) ; кінець while (перебір атрибутів)
            ) ; кінець progn (це блок з атрибутами)
          ) ; кінець if (це блок з атрибутами)
        ) ; кінець if (entget)
        (setq i (1+ i))
      ) ; кінець repeat

      ;; --- Завершення групи UNDO ---
      (command "_.UNDO" "_End")

      ;; --- Фінальне повідомлення ---
      (princ (strcat "\nЗавершено. Виконано заміну в атрибуті 'НОМЕРА' для " (itoa modCount) " блоків."))

    ) ; end progn (results are valid)
    ;; --- Якщо Результатів Пошуку Немає або вони недійсні ---
    (princ "\nНе знайдено дійсних результатів попереднього пошуку (з команди SEARCH). Спочатку виконайте команду SEARCH.")
  ) ; end if (check *g_last_search_result*)

  ;; --- Відновлення середовища та вихід ---
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho)) ; Відновити CMDECHO, якщо змінювали
  (setq *error* nil) ; Скинути обробник помилок
  (princ) ;; Чистий вихід
) ;; кінець defun c:REPLACENAME


;; --- Повідомлення про завантаження ---
(princ "\nLISP-скрипти (v3) завантажено.")
(princ "\nКоманди:")
(princ "\n  SEARCH      - Пошук блоків 'PIKET' за атрибутом 'НОМЕРА', виділення та збереження результату.")
(princ "\n  PASTEHERE   - Вставка об'єкта з буфера (_COPYBASE) в точки збереженого результату SEARCH.")
(princ "\n  CHECKPOINTS - Перевірка відповідності Z координати та атрибуту 'ОТМЕТКА' у блоках зі збереженого результату SEARCH, з можливістю виправлення.")
(princ "\n  REPLACENAME - Зміна значення атрибуту НОМЕРА (якщо теба перейменувати блоки точок)")
(princ) ;; Чистий вихід