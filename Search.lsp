;; --- LISP Скрипт для Пошуку Блоків та Наступної Вставки/Перевірки/Перейменування (v5.3 - Модифіковано RENAME_ZMARKER для редагування вибору) ---
;; --- Додано функцію CHECKPOINTS (v3) ---
;; --- Додано функцію REPLACENAME (v3) ---
;; --- Модифіковано PASTEHERE, CHECKPOINTS, REPLACENAME для роботи з ручним вибором (v4) ---
;; --- Додано функцію RENAME_OKM (v5) ---
;; --- Додано функцію RENAME_ZMARKER (v1.3 - Дозволено редагування вибору перед підтвердженням) ---

;; Глобальна змінна для зберігання результату пошуку
(setq *g_last_search_result* nil)

;; Глобальна змінна для зберігання кандидатів на оновлення текстом відмітки Z
(setq *g_rename_zmarker_candidates* nil)

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
;; СКРИПТ 1: ПОШУК ТА ВИДІЛЕННЯ БЛОКІВ (v5 - Виправлено скидання результату при скасуванні)
;; ====================================================================
;; Команда: SEARCH
;; Шукає блоки "PIKET" за атрибутом "НОМЕРА", ВИДІЛЯЄ їх, якщо значення
;; атрибута ПОЧИНАЄТЬСЯ з ключового слова,
;; та ЗБЕРІГАЄ набір вибірки у глобальну змінну *g_last_search_result*.
;; Цей результат МОЖЕ бути використаний командами PASTEHERE, CHECKPOINTS, REPLACENAME, RENAME_OKM, RENAME_ZMARKER.

(defun c:SEARCH ( / keyword ssAll i ename edata bname hasAttribs
                  attEname attEdata attTag attValue ssFound foundCount *error* old_g_last_search) ; Додано *error* та old_g_last_search

  ;; --- Функція обробки помилок для коректного скидання ---
  (defun *error* (msg)
    (if (and old_g_last_search (not *g_last_search_result*)) ; Якщо пошук був скасований/невдалий, а старий результат існував
        (setq *g_last_search_result* nil) ; Явно скинути, якщо скасовано через ESC
    )
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\nПомилка в SEARCH: " msg)))
    )
    (setq *error* nil) ; Скинути обробник помилок
    (princ)
  )

  ;; Зберегти поточний стан на випадок скасування через ESC
  (setq old_g_last_search *g_last_search_result*)
  ;; Скинути попередній результат перед новим пошуком
  (setq *g_last_search_result* nil)

  ;; --- Отримати Введення Користувача ---
  (setq keyword (getstring T "\nВведіть ПОЧАТОК значення атрибуту 'НОМЕРА' для пошуку: ")) ; Змінено підказку

  ;; --- Перевірити, чи Надано Ключове Слово ---
  (if (and keyword (/= "" keyword))
    (progn ;; Продовжити, тільки якщо ключове слово не порожнє
      (princ (strcat "\nШукаю блоки 'PIKET', де атрибут 'НОМЕРА' починається з '" keyword "'...")) ; Змінено повідомлення

      ;; --- Ініціалізація ---
      (setq ssFound (ssadd)) ;; Створити порожній набір вибірки для результатів
      (setq foundCount 0)      ;; Ініціалізувати лічильник знайдених

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
                              (setq attValue (cdr (assoc 1 attEdata)))      ;; Значення
                              (if (and (eq "НОМЕРА" attTag)
                                       attValue
                                       ;; Перевіряємо, чи значення атрибута (в верхньому регістрі)
                                       ;; відповідає шаблону "КЛЮЧОВЕСЛОВО*" (також в верхньому регістрі)
                                       (wcmatch (strcase attValue) (strcat (strcase keyword) "*"))
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
          (princ (strcat "\nЗнайдено та ВИДІЛЕНО " (itoa foundCount) " блоків. Результат збережено для можливого використання командами CHECKPOINTS, PASTEHERE, REPLACENAME, RENAME_OKM або RENAME_ZMARKER.")) ; Оновлено повідомлення
        )
        (progn ;; Якщо нічого не знайдено
           ;; Переконатися, що результат порожній (вже скинуто на початку, але для ясності)
           (setq *g_last_search_result* nil)
           (princ (strcat "\nБлоки 'PIKET', де атрибут 'НОМЕРА' починається з '" keyword "', не знайдено.")) ; Змінено повідомлення
        )
      )
    ) ;; кінець progn (ключове слово надано)
    (progn ;; <-- ЗМІНА: Якщо ключове слово не надано або порожнє (або ESC)
      (princ "\nКлючове слово не введено або пошук скасовано (ESC).")
      (setq *g_last_search_result* nil) ; Явно скидаємо результат, якщо пошук не відбувся
    )
  ) ;; кінець if (перевірка ключового слова)
  (setq *error* nil) ; Скинути обробник помилок для цієї функції
  (princ) ;; Чистий вихід
) ;; кінець defun c:SEARCH


;; ====================================================================
;; СКРИПТ 2: ВСТАВКА В ТОЧКИ ЗБЕРЕЖЕНОГО РЕЗУЛЬТАТУ АБО ВИБРАНИХ ОБ'ЄКТІВ (v5)
;; ====================================================================
;; Команда: PASTEHERE (v5 - Скидає g_last_search_result після виконання)
;; Бере набір вибірки, збережений командою SEARCH у змінній *g_last_search_result*,
;; АБО об'єкти, вибрані користувачем (попередньо або під час виконання команди).
;; Вставляє в точку вставки кожного об'єкта (блока/вставки) з цього набору
;; об'єкт(и) з буфера обміну (які були скопійовані за допомогою COPYBASE).
(defun c:PASTEHERE ( / *error* ss ss_source i ename edata ipoint oldCmdEcho oldOsmode pasteCount )

  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho)) ; Відновити CMDECHO
    (if oldOsmode (setvar "OSMODE" oldOsmode))   ; Відновити OSMODE
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command "_.UNDO" "_End")) ; Завершити UNDO якщо активне
    (cond ((not msg))                           ; Вихід без повідомлення (наприклад, ESC)
          ((vl-string-search "Function cancelled" msg)) ; Користувач скасував
          ((vl-string-search "quit / exit abort" msg))  ; Користувач скасував
          (T (princ (strcat "\nПомилка: " msg)))      ; Інша помилка
    )
    ;; <-- ЗМІНА: Скидання результату пошуку в обробнику помилок -->
    (setq *g_last_search_result* nil)
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

  ;; --- Визначення робочого набору вибірки (ss) ---
  (setq ss nil ss_source "") ; Ініціалізація
  (cond
    ;; 1. Перевірити збережений результат SEARCH
    ((and (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "збереженого результату пошуку (" (itoa (sslength ss)) " об.)"))
    )
    ;; 2. Перевірити попередню вибірку (PickFirst)
    ((setq ss (car (ssgetfirst))) ; Отримати попередньо вибрані об'єкти
     (setq ss_source (strcat "поточної вибірки (" (itoa (sslength ss)) " об.)"))
    )
    ;; 3. Запросити користувача вибрати об'єкти
    (T
     (princ "\nНе знайдено збереженого результату пошуку або попередньої вибірки.")
     ;; Запит на вибір блоків (INSERT)
     (princ "\nВиберіть об'єкти (блоки), в точки вставки яких потрібно вставити з буфера: ")
     (setq ss (ssget '((0 . "INSERT")))) ; Фільтр для блоків
     (if ss
       (setq ss_source (strcat "щойно вибраних об'єктів (" (itoa (sslength ss)) " об.)"))
       (progn (princ "\nОб'єкти не вибрано. Команду скасовано.") (exit)) ; Вихід, якщо нічого не вибрано
     )
    )
  )

  ;; --- Основна логіка вставки ---
  (if ss
    (progn ;; Є дійсний набір вибірки
      (princ (strcat "\nВставлення об'єкта(ів) з буфера обміну в точки вставки " ss_source "..."))

      ;; --- Налаштування середовища та UNDO ---
      (setq oldCmdEcho (getvar "CMDECHO"))
      (setq oldOsmode (getvar "OSMODE"))
      (setvar "CMDECHO" 0) ;; Вимкнути ехо команд
      (setvar "OSMODE" 0)  ;; Вимкнути об'єктну прив'язку
      (command "_.UNDO" "_Begin") ;; Почати групування UNDO
      (setq pasteCount 0) ;; Лічильник успішних вставок

      ;; --- Цикл по Об'єктах у вибірці та Вставка ---
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i)) ;; Отримати ім'я об'єкта з набору вибірки
        (if (setq edata (entget ename)) ; Перевірити, чи об'єкт ще існує
          (progn
            ;; Перевірка, чи це блок (INSERT) - вже відфільтровано в ssget, але залишаємо для надійності
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
              ;; Попередження, якщо об'єкт у вибірці - не блок (малоймовірно через фільтр ssget)
              (princ (strcat "\n Попередження: Об'єкт у вибірці не є блоком (INSERT): " (vl-princ-to-string ename)))
            )
          )
          ;; Попередження, якщо об'єкт з вибірки вже не існує
          (princ (strcat "\n Попередження: Об'єкт зі збереженої/вибраної вибірки вже не існує: " (vl-princ-to-string ename)))
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
    ;; --- Якщо ss залишився nil (мало б обробитись раніше, але про всяк випадок) ---
    (princ "\nНе вдалося визначити об'єкти для обробки.")
  ) ;; кінець if (ss)

  ;; --- Очистка та вихід ---
  ;; <-- ЗМІНА: Завжди скидаємо результат пошуку при виході з PASTEHERE -->
  (setq *g_last_search_result* nil)
  (setq *error* nil) ; Скинути обробник помилок
  (princ) ;; Чистий вихід
) ;; кінець defun c:PASTEHERE


;; ====================================================================
;; СКРИПТ 3: ПЕРЕВІРКА ТА ВИПРАВЛЕННЯ КООРДИНАТИ Z ЗА АТРИБУТОМ "ОТМЕТКА" (v5)
;; ====================================================================
;; Команда: CHECKPOINTS (v5 - Скидає g_last_search_result після виконання)
;; Бере набір вибірки "PIKET" з результату SEARCH, АБО вибрані користувачем.
;; Для кожного блоку "PIKET" у наборі порівнює значення атрибута "ОТМЕТКА"
;; з координатою Z точки вставки блоку.
;; Виводить звіт про розбіжності, ВИДІЛЯЄ тільки блоки з розбіжностями
;; та запитує дозвіл на виправлення Z координати.
(defun c:CHECKPOINTS ( / *error* ss ss_source totalCount i ename edata ipoint zCoord
                         attEname attEdata attTag otmetkaStr otmetkaNum
                         diffList diffCount fuzz modCount answer oldCmdecho
                         ssDiff )

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
    ;; <-- ЗМІНА: Скидання результату пошуку в обробнику помилок -->
    (setq *g_last_search_result* nil)
    (setq *error* nil)
    (princ)
  )

  ;; --- Ініціалізація ---
  (setq diffList nil      ; Список блоків з розбіжностями [(ename . (otmetkaStr . zCoord))]
        diffCount 0       ; Лічильник блоків з розбіжностями
        fuzz 1e-6         ; Допуск для порівняння дійсних чисел
        modCount 0        ; Лічильник змінених блоків
        oldCmdecho nil    ; Ініціалізуємо перед getvar
        ss nil            ; Робочий набір вибірки
        ss_source ""      ; Джерело вибірки для повідомлень
  )
  (setq oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0) ; Тимчасово вимкнути ехо команд

  ;; --- Визначення робочого набору вибірки (ss) ---
  (cond
    ;; 1. Перевірити збережений результат SEARCH
    ((and (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "збереженого результату пошуку (" (itoa (sslength ss)) " об.)"))
    )
    ;; 2. Перевірити попередню вибірку (PickFirst)
    ((setq ss (car (ssgetfirst)))
     ;; Додатково фільтруємо попередню вибірку, залишаючи тільки блоки PIKET з атрибутами
     (if ss
       (progn
         (setq ss (ssget "_P" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Фільтруємо pickfirst set
         (if (or (null ss) (= 0 (sslength ss)))
             (setq ss nil) ; Якщо після фільтрації нічого не залишилось
             (setq ss_source (strcat "поточної вибірки (відфільтровано до " (itoa (sslength ss)) " блоків 'PIKET')"))
         )
       )
     )
    )
    ;; 3. Запросити користувача вибрати об'єкти
    (T
     (princ "\nНе знайдено збереженого результату пошуку або релевантної попередньої вибірки.")
     (princ "\nВиберіть блоки 'PIKET' для перевірки Z-координати та атрибуту 'ОТМЕТКА': ")
     (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Фільтр для PIKET з атрибутами
     (if ss
       (setq ss_source (strcat "щойно вибраних блоків 'PIKET' (" (itoa (sslength ss)) " об.)"))
       (progn (princ "\nБлоки 'PIKET' не вибрано. Команду скасовано.") (exit))
     )
    )
  )

  ;; --- Основна логіка перевірки ---
  (if ss
    (progn
      (setq totalCount (sslength ss))
      (princ (strcat "\nПеревірка " (itoa totalCount) " блоків з " ss_source "..."))

      ;; --- Цикл по Об'єктах у вибірці ---
      (setq i 0)
      (repeat totalCount
        (setq ename (ssname ss i))
        (if (setq edata (entget ename)) ; Перевірити, чи об'єкт ще існує
          ;; Перевірка типу об'єкта (має бути INSERT, але ssget вже фільтрує)
          (if (eq "INSERT" (cdr (assoc 0 edata)))
            (progn
              (setq ipoint (cdr (assoc 10 edata))) ; Отримати точку вставки
              (setq zCoord (caddr ipoint))       ; Отримати Z координату
              (setq otmetkaStr nil)               ; Скинути значення атрибута

              ;; --- Пошук атрибута "ОТМЕТКА" ---
              ;; Перевірка наявності атрибутів (має бути, бо фільтрували)
              (if (and (assoc 66 edata) (= 1 (cdr (assoc 66 edata))))
                (progn
                  (setq attEname (entnext ename))
                  (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                    (setq attTag (strcase (cdr (assoc 2 attEdata))))
                    (if (eq "ОТМЕТКА" attTag)
                      (progn
                        (setq otmetkaStr (cdr (assoc 1 attEdata))) ; Зберегти значення атрибута
                        (setq attEname nil) ; Зупинити пошук атрибутів
                      )
                      (setq attEname (entnext attEname)) ; Наступний атрибут
                    )
                  ) ; end while
                )
                (princ (strcat "\n Попередження: Блок " (vl-princ-to-string ename) " не має атрибутів (неочікувано)."))
              ) ; end if has attributes

              ;; --- Порівняння значення атрибута та Z координати ---
              (if otmetkaStr
                (progn
                  (setq otmetkaNum (distof (str-replace "," "." otmetkaStr))) ; Спробувати конвертувати рядок в число (заміняємо кому на крапку)
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
            (princ (strcat "\n Попередження: Об'єкт у вибірці не є блоком (INSERT): " (vl-princ-to-string ename)))
          ) ; end if (is insert)
          (princ (strcat "\n Попередження: Об'єкт зі збереженої/вибраної вибірки вже не існує: " (vl-princ-to-string ename)))
        ) ; end if (entget)
        (setq i (1+ i))
      ) ; end repeat

      ;; --- Звіт про результати перевірки та ВИДІЛЕННЯ ПРОБЛЕМНИХ ---
      (if (= diffCount 0)
        (progn
           (princ (strcat "\nПеревірку завершено. Всі " (itoa totalCount) " перевірених блоків мають відповідну координату Z та значення атрибуту 'ОТМЕТКА'."))
           ;; Зняти виділення, якщо розбіжностей немає
           (sssetfirst nil nil)
        )
        (progn ;; Знайдено розбіжності
          (princ (strcat "\nПеревірку завершено. Всього перевірено: " (itoa totalCount) " блоків."))
          (princ (strcat "\nЗнайдено розбіжностей у Z-координаті та атрибуті 'ОТМЕТКА': " (itoa diffCount) " блоків."))
          (princ "\nСписок блоків з розбіжностями (Атрибут 'ОТМЕТКА' | Координата Z):")
          ;; Вивести список блоків з розбіжностями
          (foreach item diffList
            (princ (strcat "\n   - Блок <" (vl-princ-to-string (car item)) ">: '" (car (cdr item)) "' | " (rtos (cdr (cdr item)))))
          )

          ;; Створення та виділення нового набору тільки з проблемних блоків
          (setq ssDiff (ssadd)) ; Створити порожній набір
          (if diffList
              (foreach item diffList
                  (if (entget (car item)) ; Додаткова перевірка існування
                    (ssadd (car item) ssDiff) ; Додати ename
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
                (sssetfirst nil nil) ; Зняти виділення
              )
          )

          ;; --- Запит на виправлення ---
          (initget "Так Ні") ; Дозволити тільки ці відповіді (Українською)
          (setq answer (getkword "\n\nЗмінити координату Z для цих блоків відповідно до атрибуту 'ОТМЕТКА'? [Так/Ні]: "))

          (if (eq answer "Так")
            (progn
              ;; --- Виконання змін ---
              (princ "\nВиконую зміни...")
              (command "_.UNDO" "_Begin") ; Почати групування UNDO

              (foreach item diffList
                (setq ename (car item))
                (setq otmetkaStr (car (cdr item)))
                (if (setq edata (entget ename))
                  (progn
                    (setq ipoint (cdr (assoc 10 edata)))
                    (setq otmetkaNum (distof (str-replace "," "." otmetkaStr))) ; Переконуємось, що конвертуємо правильно
                    (if otmetkaNum
                      (progn
                        (setq new_ipoint (list (car ipoint) (cadr ipoint) otmetkaNum))
                        (setq edata (subst (cons 10 new_ipoint) (assoc 10 edata) edata))
                        (if (entmod edata)
                          (setq modCount (1+ modCount))
                          (princ (strcat "\n Помилка зміни Z координати для блоку: " (vl-princ-to-string ename)))
                        )
                      )
                       (princ (strcat "\n Помилка конвертації '" otmetkaStr "' для блоку " (vl-princ-to-string ename) " під час спроби зміни."))
                    )
                  )
                  (princ (strcat "\n Помилка: Не вдалося отримати дані для блоку " (vl-princ-to-string ename) " під час спроби зміни."))
                )
              ) ; end foreach item

              (command "_.UNDO" "_End") ; Завершити групування UNDO
              (princ (strcat "\nУспішно змінено Z координату для " (itoa modCount) " з " (itoa diffCount) " блоків."))
            ) ; end progn (answer = Так)
            (princ "\nЗміни не виконувались.")
          ) ; end if (answer = Так)
        ) ; end progn (diffCount > 0)
      ) ; end if (= diffCount 0)
    ) ; end progn (ss is valid)
    (princ "\nНе вдалося визначити об'єкти для обробки.")
  ) ; end if (ss)

  ;; --- Відновлення середовища та вихід ---
  (setvar "CMDECHO" oldCmdecho) ; Відновити CMDECHO
  ;; <-- ЗМІНА: Завжди скидаємо результат пошуку при виході з CHECKPOINTS -->
  (setq *g_last_search_result* nil)
  (setq *error* nil) ; Скинути обробник помилок
  (princ) ;; Чистий вихід
) ;; кінець defun c:CHECKPOINTS


;; ====================================================================
;; СКРИПТ 4: ЗАМІНА ПІДСТРОКИ В АТРИБУТІ "НОМЕРА" (v5)
;; ====================================================================
;; Команда: REPLACENAME (v5 - Скидає g_last_search_result після виконання)
;; Бере набір вибірки "PIKET" з результату SEARCH, АБО вибрані користувачем.
;; Запитує підстроку для пошуку та підстроку для заміни.
;; Замінює всі входження першої підстроки на другу у значенні
;; атрибута "НОМЕРА" для кожного блоку у вибірці. (Чутливо до регістру!)
(defun c:REPLACENAME ( / *error* ss ss_source i ename edata
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
    ;; <-- ЗМІНА: Скидання результату пошуку в обробнику помилок -->
    (setq *g_last_search_result* nil)
    (setq *error* nil)
    (princ)
  )

  ;; --- Ініціалізація ---
  (setq modCount 0        ; Лічильник змінених блоків
        oldCmdecho nil
        ss nil            ; Робочий набір вибірки
        ss_source ""      ; Джерело вибірки для повідомлень
  )
  (setq oldCmdecho (getvar "CMDECHO"))
  ;(setvar "CMDECHO" 0) ; Можна вимкнути ехо, якщо бажано

 ;; --- Визначення робочого набору вибірки (ss) ---
  (cond
    ;; 1. Перевірити збережений результат SEARCH
    ((and (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "збереженого результату пошуку (" (itoa (sslength ss)) " об.)"))
    )
    ;; 2. Перевірити попередню вибірку (PickFirst)
    ((setq ss (car (ssgetfirst)))
     ;; Додатково фільтруємо попередню вибірку, залишаючи тільки блоки PIKET з атрибутами
     (if ss
       (progn
         (setq ss (ssget "_P" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Фільтруємо pickfirst set
         (if (or (null ss) (= 0 (sslength ss)))
             (setq ss nil) ; Якщо після фільтрації нічого не залишилось
             (setq ss_source (strcat "поточної вибірки (відфільтровано до " (itoa (sslength ss)) " блоків 'PIKET')"))
         )
       )
     )
    )
    ;; 3. Запросити користувача вибрати об'єкти
    (T
     (princ "\nНе знайдено збереженого результату пошуку або релевантної попередньої вибірки.")
     (princ "\nВиберіть блоки 'PIKET', в яких потрібно замінити значення атрибуту 'НОМЕРА': ")
     (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Фільтр для PIKET з атрибутами
     (if ss
       (setq ss_source (strcat "щойно вибраних блоків 'PIKET' (" (itoa (sslength ss)) " об.)"))
       (progn (princ "\nБлоки 'PIKET' не вибрано. Команду скасовано.") (exit))
     )
    )
  )

  ;; --- Основна логіка заміни ---
  (if ss
    (progn ;; Є дійсний набір вибірки
      (princ (strcat "\nБуде оброблено " (itoa (sslength ss)) " блоків з " ss_source "."))

      ;; --- Отримати рядки для пошуку та заміни ---
      (setq findStr (getstring T "\nВведіть підстроку, ЯКУ замінити в атрибуті 'НОМЕРА' (чутливо до регістру): "))
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

      ;; --- Цикл по Об'єктах у вибірці ---
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i))
        (if (setq edata (entget ename)) ; Перевірити, чи об'єкт ще існує
          ;; Перевірка типу об'єкта та наявності атрибутів (має бути за фільтром)
          (if (and (eq "INSERT" (cdr (assoc 0 edata)))
                   (assoc 66 edata) (= 1 (cdr (assoc 66 edata))))
            (progn
              ;; --- Пошук атрибута "НОМЕРА" та його зміна ---
              (setq attEname (entnext ename))
              (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                (setq attTag (strcase (cdr (assoc 2 attEdata)))) ; Тег (для перевірки)

                (if (eq "НОМЕРА" attTag)
                  (progn
                    (setq currentVal (cdr (assoc 1 attEdata))) ; Поточне значення атрибута
                    ;; Виконати заміну за допомогою допоміжної функції (чутлива до регістру)
                    (setq newVal (str-replace findStr replaceStr currentVal))

                    ;; Якщо значення змінилося, оновити атрибут
                    (if (not (equal currentVal newVal)) ; Використовуємо equal для рядків
                      (progn
                        (setq attEdata (subst (cons 1 newVal) (assoc 1 attEdata) attEdata))
                        (if (entmod attEdata)
                          (setq modCount (1+ modCount)) ; Збільшити лічильник успішних змін
                          (princ (strcat "\n Помилка оновлення атрибута для блоку: " (vl-princ-to-string ename)))
                        )
                      )
                    )
                    (setq attEname nil) ; Зупинити пошук атрибутів для цього блоку
                  )
                  ;; Якщо це не атрибут "НОМЕРА", перейти до наступного
                  (setq attEname (entnext attEname))
                ) ; кінець if (eq "НОМЕРА" attTag)
              ) ; кінець while (перебір атрибутів)
            ) ; кінець progn (це блок з атрибутами)
            (princ (strcat "\n Попередження: Об'єкт " (vl-princ-to-string ename) " не є блоком 'PIKET' з атрибутами (неочікувано)."))
          ) ; кінець if (це блок з атрибутами)
          (princ (strcat "\n Попередження: Об'єкт зі збереженої/вибраної вибірки вже не існує: " (vl-princ-to-string ename)))
        ) ; кінець if (entget)
        (setq i (1+ i))
      ) ; кінець repeat

      ;; --- Завершення групи UNDO ---
      (command "_.UNDO" "_End")

      ;; --- Фінальне повідомлення ---
      (princ (strcat "\nЗавершено. Виконано заміну в атрибуті 'НОМЕРА' для " (itoa modCount) " блоків."))

    ) ; end progn (ss is valid)
    (princ "\nНе вдалося визначити об'єкти для обробки.")
  ) ; end if (ss)

  ;; --- Відновлення середовища та вихід ---
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho)) ; Відновити CMDECHO, якщо змінювали
  ;; <-- ЗМІНА: Завжди скидаємо результат пошуку при виході з REPLACENAME -->
  (setq *g_last_search_result* nil)
  (setq *error* nil) ; Скинути обробник помилок
  (princ) ;; Чистий вихід
) ;; кінець defun c:REPLACENAME


;; ====================================================================
;; СКРИПТ 5: ОНОВЛЕННЯ ТЕКСТУ БІЛЯ ПІКЕТІВ ЗА АТРИБУТОМ "НОМЕРА" (v5.3)
;; ====================================================================
;; Команда: RENAME_OKM (v5.3 - Відстань розраховується в 2D, ігноруючи Z)
;; Бере набір вибірки "PIKET" з результату SEARCH, АБО вибрані користувачем.
;; Для кожного блоку "PIKET":
;; 1. Вилучає номер з дужок в атрибуті "НОМЕРА" (напр., з "ОКМ(22)12" -> "22").
;; 2. Шукає найближчий текстовий об'єкт (TEXT або MTEXT) в межах заданого 2D-радіусу.
;; 3. Якщо текст знайдено, він починається з "№" І ЩЕ НЕ БУВ ОБРОБЛЕНИЙ У ЦЬОМУ ЗАПУСКУ:
;;    - Збирає інформацію про потенційне оновлення.
;; Після перевірки всіх блоків:
;; 4. ВИДІЛЯЄ всі текстові об'єкти, які потребують оновлення.
;; 5. Запитує користувача підтвердження на зміну.
;; 6. Якщо підтверджено, оновлює текст.

(defun c:RENAME_OKM ( / *error* ss ss_source i enamePiket edataPiket attEname attEdata attTag
                       attrValNomera blockPt openParen closeParen
                       extractedNum searchDist ssTextAll j textEnt textData textPt textVal
                       newTextVal textFoundForBlock updatedCount processedCount totalCount
                       oldCmdecho fuzz updatedTextEnts
                       ;; --- Нові змінні для виділення та підтвердження ---
                       texts_to_update_info ssHighlight potentialUpdateCount answer actualUpdateCount
                      )

  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command-s "_.UNDO" "_End"))
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\nПомилка в RENAME_OKM: " msg)))
    )
    (setq *g_last_search_result* nil)
    (setq *error* nil)
    (princ)
  )

  ;; --- Ініціалізація ---
  (setq updatedCount 0
        processedCount 0
        oldCmdecho nil
        ss nil
        ss_source ""
        fuzz 1e-9
        updatedTextEnts nil
        texts_to_update_info nil
        ssHighlight nil
        potentialUpdateCount 0
        actualUpdateCount 0
        answer nil
  )
  (setq oldCmdecho (getvar "CMDECHO"))
  ;(setvar "CMDECHO" 0)


  ;; --- Отримати радіус пошуку тексту ---
  ; Пояснення можна додати, що це 2D відстань, якщо потрібно
  (setq searchDist (getdist "\nВведіть максимальну відстань для пошуку тексту біля точки PIKET (в площині XY): "))
  (if (or (null searchDist) (<= searchDist 0))
    (progn (princ "\nНевірна відстань пошуку. Команду скасовано.") (exit))
  )

  ;; --- Визначення робочого набору вибірки (ss) для блоків PIKET ---
   (cond
    ;; 1. Перевірити збережений результат SEARCH
    ((and (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (if (and ss (> (sslength ss) 0))
          (setq ss_source (strcat "збереженого результату пошуку (" (itoa (sslength ss)) " блоків 'PIKET')"))
          (setq ss nil ss_source "збереженого результату пошуку (але він порожній або некоректний)")
     )
    )
    ;; 2. Перевірити попередню вибірку (PickFirst) - УВАГА: ssgetfirst повертає (ss name_ss), тому cadr
    ((setq ss (cadr (ssgetfirst)))
      (if ss
        (progn
          (setq ss (ssget "_I" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Перевіряємо поточний PickFirst на відповідність критеріям
          (if (or (null ss) (= 0 (sslength ss)))
              (setq ss nil ss_source "поточної вибірки (але вона не містить блоків 'PIKET' з атрибутами)")
              (setq ss_source (strcat "поточної вибірки (відфільтровано до " (itoa (sslength ss)) " блоків 'PIKET')"))
          )
        )
        (setq ss nil) ; Якщо cadr(ssgetfirst) - nil
      )
    )
    ;; 3. Запросити користувача вибрати об'єкти
    (T
     (princ "\nНе знайдено збереженого результату пошуку або релевантної попередньої вибірки.")
     (princ "\nВиберіть блоки 'PIKET', біля яких потрібно оновити текст: ")
     (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1))))
     (if ss
       (setq ss_source (strcat "щойно вибраних блоків 'PIKET' (" (itoa (sslength ss)) " об.)"))
       (progn (princ "\nБлоки 'PIKET' не вибрано. Команду скасовано.") (exit))
     )
    )
  )

  ;; --- Основна логіка пошуку та збору кандидатів на оновлення ---
  (if ss
    (progn
      (setq totalCount (sslength ss))
      (princ (strcat "\nПошук відповідних текстових полів біля " (itoa totalCount) " блоків 'PIKET' з " ss_source "..."))

      (setq ssTextAll (ssget "_X" '((0 . "TEXT,MTEXT"))))
      (if (null ssTextAll) (princ "\nПопередження: У кресленні не знайдено жодних текстових об'єктів (TEXT або MTEXT)."))

      ;; --- Цикл по вибраних/знайдених блоках PIKET ---
      (setq i 0)
      (repeat totalCount
        (setq enamePiket (ssname ss i))
        (setq extractedNum nil attrValNomera nil)
        (setq textFoundForBlock nil)

        (if (setq edataPiket (entget enamePiket))
          (progn
            (setq processedCount (1+ processedCount))
            (setq blockPt (cdr (assoc 10 edataPiket))) ; 3D точка блоку

            ;; --- Пошук атрибуту "НОМЕРА" ---
            (if (and (assoc 66 edataPiket) (= 1 (cdr (assoc 66 edataPiket))))
              (progn
                (setq attEname (entnext enamePiket))
                (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                  (setq attTag (strcase (cdr (assoc 2 attEdata))))
                  (if (eq "НОМЕРА" attTag)
                    (progn (setq attrValNomera (cdr (assoc 1 attEdata))) (setq attEname nil))
                    (setq attEname (entnext attEname))
                  )
                )
              )
            )

            ;; --- Вилучення номера з атрибуту ---
            (if attrValNomera
              (progn
                (setq openParen (vl-string-search "(" attrValNomera))
                (setq closeParen (vl-string-search ")" attrValNomera (if openParen (+ openParen 1) 0)))
                (if (and openParen closeParen (> closeParen openParen))
                  (setq extractedNum (substr attrValNomera (+ openParen 2) (- closeParen openParen 1)))
                )
              )
            )

            ;; --- Пошук тексту, ЯКЩО номер вилучено і є текстові об'єкти ---
            (if (and extractedNum ssTextAll blockPt)
              (progn
                 (setq j 0)
                 ;; Цикл по ВСІХ текстових об'єктах
                 (while (and (< j (sslength ssTextAll)) (not textFoundForBlock))
                   (setq textEnt (ssname ssTextAll j))
                   (if (setq textData (entget textEnt))
                     (progn
                       (setq textPt (cdr (assoc 10 textData))) ; 3D точка тексту
                       (setq textVal (cdr (assoc 1 textData)))

                       ;; Перевірка відстані в 2D, префіксу "№" І ЧИ НЕ БУВ ЦЕЙ ТЕКСТ ВЖЕ ОБРОБЛЕНИЙ
                       (if (and textPt textVal
                                ;; --- ЗМІНА: Розрахунок 2D відстані ---
                                (<= (distance (list (car blockPt) (cadr blockPt) 0.0) ; 2D точка блоку
                                              (list (car textPt) (cadr textPt) 0.0)   ; 2D точка тексту
                                    )
                                    searchDist
                                )
                                ;; --- Кінець зміни ---
                                (= (vl-string-search "№" textVal) 0) ; Перевірка префіксу
                                (not (member textEnt updatedTextEnts)) ; Перевірка чи не оброблено
                           )
                         (progn
                           ;; Цей текст підходить
                           (setq updatedCount (1+ updatedCount))
                           (setq newTextVal (strcat "№" extractedNum))

                           (if (not (equal textVal newTextVal))
                               (progn
                                 (setq texts_to_update_info (cons (list textEnt newTextVal enamePiket) texts_to_update_info))
                                 (setq potentialUpdateCount (1+ potentialUpdateCount))
                                 (princ (strcat "\n   * Кандидат на оновлення: <" (vl-princ-to-string textEnt) "> ('" textVal "' -> '" newTextVal "') біля блоку <" (vl-princ-to-string enamePiket) ">"))
                               )
                           )

                           (setq updatedTextEnts (cons textEnt updatedTextEnts))
                           (setq textFoundForBlock T)
                         )
                       ) ; кінець if (перевірка відстані, префіксу та списку)
                     )
                   )
                   (setq j (1+ j))
                 ) ; end while (пошук тексту)
              )
            )
          )
        ) ; end if (entget enamePiket)
        (setq i (1+ i))
      ) ; end repeat (по блоках PIKET)

      ;; --- Виділення знайдених кандидатів та запит на підтвердження ---
      (if (> potentialUpdateCount 0)
        (progn
          (princ (strcat "\n\nЗнайдено " (itoa potentialUpdateCount) " текстових полів, які потребують оновлення."))
          ;; --- Створення набору вибірки для виділення ---
          (setq ssHighlight (ssadd))
          (foreach item texts_to_update_info
            (if (entget (car item))
                (ssadd (car item) ssHighlight)
            )
          )

          (if (> (sslength ssHighlight) 0)
            (progn
              (princ (strcat "\nВиділено " (itoa (sslength ssHighlight)) " текстових об'єктів для перевірки."))
              (sssetfirst nil ssHighlight) ; Виділити знайдені тексти

              ;; --- Запит на підтвердження ---
              (initget "Так Ні")
              (setq answer (getkword "\n\nОновити виділені текстові поля? [Так/Ні]: "))

              (if (eq answer "Так")
                (progn
                  ;; --- Виконання змін ---
                  (princ "\nВиконую оновлення...")
                  (command "_.UNDO" "_Begin")
                  (foreach item texts_to_update_info
                      (setq textEnt (car item))
                      (setq newTextVal (cadr item))
                      (setq enamePiket (caddr item))

                      (if (setq textData (entget textEnt))
                        (progn
                          (setq currentTextVal (cdr (assoc 1 textData)))
                          (setq textData (subst (cons 1 newTextVal) (assoc 1 textData) textData))
                          (if (entmod textData)
                            (progn
                              (princ (strcat "\n  Оновлено: <" (vl-princ-to-string textEnt) "> ('" currentTextVal "' -> '" newTextVal "')"))
                              (setq actualUpdateCount (1+ actualUpdateCount))
                            )
                            (princ (strcat "\n  Помилка оновлення тексту <" (vl-princ-to-string textEnt) ">"))
                          )
                        )
                        (princ (strcat "\n  Помилка: Не вдалося отримати дані для тексту <" (vl-princ-to-string textEnt) "> під час спроби оновлення."))
                      )
                  )
                  (command "_.UNDO" "_End")
                  (princ (strcat "\nУспішно оновлено " (itoa actualUpdateCount) " текстових полів."))
                )
                ;; --- Якщо користувач відповів "Ні" або скасував ---
                (progn
                  (princ "\nЗміни скасовано користувачем. Текстові поля не оновлено.")
                  (sssetfirst nil nil) ; Зняти виділення
                )
              )
            )
            ;; --- Якщо не вдалося створити набір для виділення ---
            (princ "\nНе вдалося створити набір вибірки для виділення текстів.")
          )
        )
        ;; --- Якщо не знайдено текстів, що потребують оновлення ---
        (progn
           (princ "\n\nНе знайдено текстових полів, що потребують оновлення.")
           (if updatedTextEnts (sssetfirst nil nil)) ; Зняти виділення, якщо щось було знайдено, але не потребувало змін
        )
      )

      ;; --- Фінальний звіт ---
      (princ (strcat "\n\nОперацію завершено."))
      (princ (strcat "\nВсього блоків 'PIKET' для обробки (з " ss_source "): " (itoa totalCount)))
      (princ (strcat "\nРеально оброблено блоків: " (itoa processedCount)))
      (princ (strcat "\nЗнайдено відповідних текстових полів біля блоків: " (itoa updatedCount)))
      (if (> potentialUpdateCount 0) (princ (strcat "\nЗ них потребували оновлення: " (itoa potentialUpdateCount))))
      (if (eq answer "Так") (princ (strcat "\nФактично оновлено після підтвердження: " (itoa actualUpdateCount))))

    ) ; end progn (ss is valid)
    (princ "\nНе вдалося визначити об'єкти для обробки (немає блоків 'PIKET' у вибірці).")
  ) ; end if (ss)

  ;; --- Відновлення середовища та вихід ---
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (setq *g_last_search_result* nil)
  (setq *error* nil)
  (princ) ;; Чистий вихід
) ;; кінець defun c:RENAME_OKM


;; ====================================================================
;; СКРИПТ 6: ПОШУК КАНДИДАТІВ ДЛЯ ОНОВЛЕННЯ ТЕКСТУ ЗА АТРИБУТОМ "ОТМЕТКА" (v1.5.1)
;; ====================================================================
;; Команда: RENAME_ZMARKER (v1.5.1 - Виправлено зайву дужку в циклі пошуку тексту)
;; Ця команда виконує першу частину процесу:
;; 1. Бере набір вибірки "PIKET" (з SEARCH або вибрані).
;; 2. Знаходить значення атрибуту "ОТМЕТКА" для кожного блоку.
;; 3. Шукає поруч текстові об'єкти на шарі "21 ВІДМІТКИ".
;; 4. Збирає список кандидатів на оновлення.
;; 5. Зберігає цей список у глобальну змінну *g_rename_zmarker_candidates*.
;; 6. Підсвічує знайдених кандидатів.
;; 7. Повідомляє користувача про необхідність відредагувати вибір та запустити RENAME_ZMARKER_RUN.

(defun c:RENAME_ZMARKER ( / *error* ss ss_source i enamePiket edataPiket attEname attEdata attTag
                            attrValOtmetka blockPt
                            otmetkaValue searchDist ssTextAll j textEnt textData textPt textVal textLayer
                            newTextVal textFoundForBlock updatedCount processedCount totalCount
                            oldCmdecho fuzz updatedTextEnts
                            ;; --- Змінні для кандидатів ---
                            texts_to_update_info ssHighlight potentialUpdateCount
                           )

  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    ;; Очищення глобальної змінної при помилці/скасуванні
    (setq *g_rename_zmarker_candidates* nil)
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\nПомилка в RENAME_ZMARKER: " msg)))
    )
    (setq *g_last_search_result* nil) ; Також очищаємо результат пошуку, якщо використовувався
    (setq *error* nil)
    (princ)
  )

  ;; --- Ініціалізація ---
  ;; Очищення глобальної змінної перед новим пошуком кандидатів
  (setq *g_rename_zmarker_candidates* nil)
  (setq updatedCount 0 processedCount 0 oldCmdecho nil ss nil ss_source "" fuzz 1e-9
        updatedTextEnts nil texts_to_update_info nil ssHighlight nil potentialUpdateCount 0
  )
  (setq oldCmdecho (getvar "CMDECHO"))
  ;(setvar "CMDECHO" 0)

  ;; --- Отримати радіус пошуку тексту ---
  (setq searchDist (getdist "\nВведіть максимальну відстань для пошуку тексту біля точки PIKET (в площині XY): "))
  (if (or (null searchDist) (<= searchDist 0))
    (progn (princ "\nНевірна відстань пошуку. Команду скасовано.") (exit))
  )

  ;; --- Визначення робочого набору вибірки (ss) для блоків PIKET ---
  (cond
    ((and (boundp '*g_last_search_result*) *g_last_search_result* (= 'PICKSET (type *g_last_search_result*)) (> (sslength *g_last_search_result*) 0))
     (setq ss *g_last_search_result*) (setq ss_source (strcat "збереженого пошуку (" (itoa (sslength ss)) " бл.)")))
    ((setq ss (cadr (ssgetfirst)))
     (if ss (progn (setq ss (ssget "_I" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) (if (or (null ss) (= 0 (sslength ss))) (setq ss nil ss_source "поточної вибірки (не підходить)") (setq ss_source (strcat "поточної вибірки (" (itoa (sslength ss)) " бл.)")))) (setq ss nil)))
    (T (princ "\nНе знайдено збереженого пошуку...") (princ "\nВиберіть блоки 'PIKET'...") (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1))))
       (if ss (setq ss_source (strcat "щойно вибраних блоків (" (itoa (sslength ss)) " бл.)")) (progn (princ "\nБлоки 'PIKET' не вибрано.") (exit)))))

  ;; --- Основна логіка пошуку та збору кандидатів ---
  (if ss
    (progn
      (setq totalCount (sslength ss))
      (princ (strcat "\nПошук текстових полів на шарі '21 ВІДМІТКИ' біля " (itoa totalCount) " блоків 'PIKET' з " ss_source "..."))

      (setq ssTextAll (ssget "_X" '((0 . "TEXT,MTEXT"))))
      (if (null ssTextAll) (princ "\nПопередження: У кресленні не знайдено жодних текстових об'єктів."))

      ;; --- Цикл по вибраних/знайдених блоках PIKET ---
      (setq i 0)
      (repeat totalCount
        (setq enamePiket (ssname ss i))
        (setq attrValOtmetka nil otmetkaValue nil textFoundForBlock nil)
        (if (setq edataPiket (entget enamePiket))
          (progn
            (setq processedCount (1+ processedCount)) (setq blockPt (cdr (assoc 10 edataPiket)))
            ;; Пошук атрибуту "ОТМЕТКА"
            (if (and (assoc 66 edataPiket) (= 1 (cdr (assoc 66 edataPiket))))
              (progn (setq attEname (entnext enamePiket))
                     (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                       (setq attTag (strcase (cdr (assoc 2 attEdata))))
                       (if (eq "ОТМЕТКА" attTag) (progn (setq attrValOtmetka (cdr (assoc 1 attEdata))) (setq attEname nil)) (setq attEname (entnext attEname)))
                     )
              )
            )
            (if attrValOtmetka (setq otmetkaValue attrValOtmetka) (princ (strcat "\n Попередження: Атрибут 'ОТМЕТКА' не знайдено/порожній: " (vl-princ-to-string enamePiket))))
            ;; Пошук тексту
            (if (and otmetkaValue ssTextAll blockPt)
              (progn
                 (setq j 0)
                 (while (and (< j (sslength ssTextAll)) (not textFoundForBlock))
                   (setq textEnt (ssname ssTextAll j))
                   (if (setq textData (entget textEnt))
                     (progn
                       (setq textPt (cdr (assoc 10 textData))) (setq textVal (cdr (assoc 1 textData))) (setq textLayer (cdr (assoc 8 textData)))
                       (if (and textPt textVal textLayer (equal textLayer "21 ВІДМІТКИ") (<= (distance (list (car blockPt) (cadr blockPt) 0.0) (list (car textPt) (cadr textPt) 0.0)) searchDist) (not (member textEnt updatedTextEnts))) ; Початок IF conditions met
                         (progn
                           (setq updatedCount (1+ updatedCount)) (setq newTextVal otmetkaValue)
                           (if (not (equal textVal newTextVal))
                             (progn
                               (setq texts_to_update_info (cons (list textEnt newTextVal enamePiket) texts_to_update_info))
                               (setq potentialUpdateCount (1+ potentialUpdateCount))
                               (princ (strcat "\n   * Кандидат: <" (vl-princ-to-string textEnt) "> ('" textVal "' -> '" newTextVal "') біля <" (vl-princ-to-string enamePiket) ">"))
                             )
                           )
                           (setq updatedTextEnts (cons textEnt updatedTextEnts))
                           (setq textFoundForBlock T)
                         )
                       )
                     )
                   )
                   (setq j (1+ j))
                 )
              )
            )
          )
        )
        (setq i (1+ i))
      )

      ;; --- Збереження результатів та підсвічування ---
      (if (> potentialUpdateCount 0)
        (progn
          (princ (strcat "\n\nЗнайдено " (itoa potentialUpdateCount) " потенційних кандидатів на оновлення."))
          ;; --- Створення набору вибірки для підсвічування ---
          (setq ssHighlight (ssadd))
          (foreach item texts_to_update_info
            (if (entget (car item)) (ssadd (car item) ssHighlight)))

          (if (and ssHighlight (> (sslength ssHighlight) 0))
            (progn
              ;; *** Збереження списку кандидатів у глобальну змінну ***
              (setq *g_rename_zmarker_candidates* (reverse texts_to_update_info)) ; Reverse, щоб зберегти порядок знаходження (опціонально)

              (princ (strcat "\nВиділено " (itoa (sslength ssHighlight)) " текстових об'єктів-кандидатів."))
              ;; *** Підсвічування кандидатів ***
              (sssetfirst nil ssHighlight)

              ;; *** Інструкція для користувача ***
              (princ "\n\n---> Кандидатів ВИДІЛЕНО. Будь ласка, відредагуйте вибір стандартними засобами AutoCAD (Shift+клік тощо).")
              (princ "\n---> Потім запустіть команду RENAME_ZMARKER_RUN для підтвердження та виконання оновлення.")
            )
            (princ "\nНе вдалося створити набір вибірки для виділення текстів-кандидатів.")
          )
        )
        ;; --- Якщо не знайдено кандидатів на оновлення ---
        (progn
           (princ "\n\nНе знайдено текстових полів на шарі '21 ВІДМІТКИ', що потребують оновлення.")
           (setq *g_rename_zmarker_candidates* nil) ; Переконуємося, що глобальна змінна порожня
           (sssetfirst nil nil) ; Знімаємо будь-яке попереднє виділення
        )
      )
    ) ; end progn (ss is valid)
    (princ "\nНе вдалося визначити об'єкти для обробки (немає блоків 'PIKET' у вибірці).")
  ) ; end if (ss)

  ;; --- Відновлення середовища та вихід ---
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  ; НЕ очищуємо *g_last_search_result* тут, він може бути потрібен RENAME_ZMARKER_RUN, якщо він базувався на пошуку
  (setq *error* nil) ; Скинути обробник помилок
  (princ) ;; Чистий вихід
) ;; кінець defun c:RENAME_ZMARKER


;; ====================================================================
;; СКРИПТ 7: ВИКОНАННЯ ОНОВЛЕННЯ ТЕКСТУ ЗА АТРИБУТОМ "ОТМЕТКА" (v1.1)
;; ====================================================================
;; Команда: RENAME_ZMARKER_RUN (v1.1 - Додано очищення *g_last_search_result*)
;; Ця команда виконує другу частину процесу:
;; 1. Перевіряє, чи є збережений список кандидатів від RENAME_ZMARKER.
;; 2. Отримує поточний набір вибірки (PickFirst set), який користувач мав відредагувати.
;; 3. Фільтрує список кандидатів, залишаючи тільки ті, що є у поточній вибірці.
;; 4. Запитує підтвердження на оновлення.
;; 5. Якщо підтверджено, оновлює текст для відфільтрованих об'єктів.
;; 6. Очищує збережений список кандидатів (*g_rename_zmarker_candidates*) ТА результат пошуку (*g_last_search_result*).

(defun c:RENAME_ZMARKER_RUN ( / *error* retrieved_candidates final_ss final_update_info
                                answer actualUpdateCount item textEnt newTextVal textData currentTextVal
                                oldCmdecho user_input
                              )
  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command-s "_.UNDO" "_End"))
    ;; Очищення глобальних змінних при помилці/скасуванні
    (setq *g_rename_zmarker_candidates* nil)
    (setq *g_last_search_result* nil) ; <--- ДОДАНО ОЧИЩЕННЯ ТУТ
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg) (princ "\nСкасовано."))
          ((vl-string-search "quit / exit abort" msg) (princ "\nСкасовано."))
          (T (princ (strcat "\nПомилка в RENAME_ZMARKER_RUN: " msg)))
    )
    (setq *error* nil)
    (princ)
  )

  ;; --- Ініціалізація ---
  (setq retrieved_candidates nil final_ss nil final_update_info nil answer nil actualUpdateCount 0 oldCmdecho nil user_input nil)
  (setq oldCmdecho (getvar "CMDECHO"))
  ;(setvar "CMDECHO" 0)

  ;; --- Перевірка та отримання списку кандидатів ---
  (if (and (boundp '*g_rename_zmarker_candidates*) *g_rename_zmarker_candidates* (listp *g_rename_zmarker_candidates*))
      (setq retrieved_candidates *g_rename_zmarker_candidates*)
      (progn
        (princ "\nПомилка: Список кандидатів не знайдено. Спочатку запустіть команду RENAME_ZMARKER.")
        (exit)
      )
  )

  ;; --- Отримання фінального вибору користувача (БЕЗ ЗАПИТУ) ---
  (setq final_ss (ssget "_I")) ; Отримуємо поточний PickFirst набір (результат редагування)

  (if (null final_ss)
      (progn
         (princ "\nНічого не вибрано для оновлення (PickFirst порожній). Запустіть RENAME_ZMARKER знову, якщо потрібно.")
         (setq *g_rename_zmarker_candidates* nil) ; Очищаємо глобальну змінну кандидатів
         (setq *g_last_search_result* nil) ; <--- ДОДАНО ОЧИЩЕННЯ ТУТ
         (exit)
      )
  )

  ;; --- Фільтрація списку кандидатів на основі фінального вибору ---
  (setq final_update_info nil)
  (foreach item retrieved_candidates
      (setq textEnt (car item))
      (if (ssmemb textEnt final_ss) ; Перевірити, чи кандидат є у фінальній вибірці
         (setq final_update_info (cons item final_update_info))
      )
  )

  ;; --- Перевірка відфільтрованого списку та запит на підтвердження ---
  (if final_update_info
      (progn ; Є що оновлювати з вибраного
        (setq final_update_info (reverse final_update_info)) ; Відновити порядок для звіту (опціонально)
        (princ (strcat "\nЗ вибраних об'єктів " (itoa (length final_update_info)) " є дійсними кандидатами на оновлення."))

        ;; *** ЗАПИТ НА ПІДТВЕРДЖЕННЯ (з виділенням фінального списку) ***
        ; Спочатку підсвітимо те, що будемо оновлювати
        (if final_ss (sssetfirst nil final_ss))

        ; Використовуємо getstring для уникнення подвійної підказки
        (setq answer nil) ; Скидаємо відповідь
        (while (not answer)
           (setq user_input (strcase (getstring T (strcat "\n\nОновити ці " (itoa (length final_update_info)) " об'єкт(и)? [Так/Ні]: "))))
           (cond
              ((member user_input '("ТАК" "Т" "ДА" "Д" "YES" "Y")) (setq answer "Так"))
              ((member user_input '("НІ" "Н" "НЕТ" "Н" "NO" "N")) (setq answer "Ні"))
              (T (prompt "\nБудь ласка, введіть 'Так' або 'Ні'.")))
        )

      )
      (progn ; Вибрані об'єкти не містять дійсних кандидатів
        (princ "\nСеред вибраних об'єктів немає дійсних кандидатів, знайдених RENAME_ZMARKER.")
        (setq answer "Ні") ; Вважаємо скасуванням
      )
  )

  ;; --- Виконання змін ---
  (if (eq answer "Так")
      (progn
         (princ "\nВиконую оновлення...")
         (command "_.UNDO" "_Begin")
         (foreach item final_update_info
             (setq textEnt (car item))
             (setq newTextVal (cadr item))
             (if (setq textData (entget textEnt))
               (progn
                 (setq currentTextVal (cdr (assoc 1 textData)))
                 (setq textData (subst (cons 1 newTextVal) (assoc 1 textData) textData))
                 (if (entmod textData)
                   (progn
                     (princ (strcat "\n  Оновлено: <" (vl-princ-to-string textEnt) "> ('" currentTextVal "' -> '" newTextVal "')"))
                     (setq actualUpdateCount (1+ actualUpdateCount))
                   )
                   (princ (strcat "\n  Помилка оновлення тексту <" (vl-princ-to-string textEnt) ">"))
                 )
               )
               (princ (strcat "\n  Помилка: Не вдалося отримати дані для тексту <" (vl-princ-to-string textEnt) "> під час спроби оновлення."))
             )
         )
         (command "_.UNDO" "_End")
         (if (> actualUpdateCount 0)
             (princ (strcat "\nУспішно оновлено " (itoa actualUpdateCount) " текстових полів."))
             (princ "\nЖодного об'єкта не було оновлено (можливо, через помилки).")
         )
      )
      ;; --- Якщо користувач відповів "Ні" ---
      (progn
        (princ "\nОновлення скасовано користувачем.")
        (princ "\nЗміни не виконувались.")
      )
  )

  ;; --- Очищення та вихід ---
  (setq *g_rename_zmarker_candidates* nil) ; Очищення глобальної змінної кандидатів
  (setq *g_last_search_result* nil) ; <--- ДОДАНО ОЧИЩЕННЯ ТУТ
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (setq *error* nil) ; Скинути обробник помилок
  (princ) ;; Чистий вихід
) ;; кінець defun c:RENAME_ZMARKER_RUN


;; ====================================================================
;; СКРИПТ 8: СТВОРЕННЯ ВІДСУТНІХ ТЕКСТОВИХ ВІДМІТОК Z (v1.0)
;; ====================================================================
;; Команда: CREATE_ZMARKER (v1.0 - Створює текст Z в точці PIKET, якщо його там ще немає)
;; 1. Вибирає блоки PIKET (з пошуку, виділення або вручну).
;; 2. Для кожного блоку перевіряє наявність атрибуту "ОТМЕТКА".
;; 3. Перевіряє, чи існує текст на шарі "21 ВІДМІТКИ" ТОЧНО в точці вставки блоку.
;; 4. Якщо текст в точці вставки відсутній, створює новий текст зі значенням "ОТМЕТКА"
;;    в цій точці на шарі "21 ВІДМІТКИ".

(defun c:CREATE_ZMARKER ( / *error* ss ss_source totalCount i piketEnt piketData piketPt
                            attrValOtmetka otmetkaValue ssTextLayer j textEnt textData textPt
                            foundExactText createdCount oldCmdecho fuzz attEname attEdata attTag newTextDxf
                           )
  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command-s "_.UNDO" "_End")) ; Завершити UNDO, якщо активне
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg) (princ "\nСкасовано."))
          ((vl-string-search "quit / exit abort" msg) (princ "\nСкасовано."))
          (T (princ (strcat "\nПомилка в CREATE_ZMARKER: " msg)))
    ) ;_ end cond
    (setq *error* nil)
    (princ)
  ) ;_ end defun *error*

  ;; --- Ініціалізація ---
  (setq ss nil ss_source nil totalCount 0 i 0 piketEnt nil piketData nil piketPt nil
        attrValOtmetka nil otmetkaValue nil ssTextLayer nil j 0 textEnt nil textData nil textPt nil
        foundExactText nil createdCount 0 oldCmdecho nil fuzz 1e-9 attEname nil attEdata nil attTag nil newTextDxf nil
  )
  (setq oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0) ; Вимкнути ехо команд

  ;; --- Визначення робочого набору вибірки (ss) для блоків PIKET ---
  (cond
    ((and (boundp '*g_last_search_result*) *g_last_search_result* (= 'PICKSET (type *g_last_search_result*)) (> (sslength *g_last_search_result*) 0))
     (setq ss *g_last_search_result*) (setq ss_source (strcat "збереженого пошуку (" (itoa (sslength ss)) " бл.)")))
    ((setq ss (cadr (ssgetfirst)))
     (if ss (progn (setq ss (ssget "_I" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) (if (or (null ss) (= 0 (sslength ss))) (setq ss nil ss_source "поточної вибірки (не підходить)") (setq ss_source (strcat "поточної вибірки (" (itoa (sslength ss)) " бл.)")))) (setq ss nil)))
    (T (princ "\nНе знайдено збереженого пошуку або поточної вибірки.") (princ "\nВиберіть блоки 'PIKET' для створення відсутніх відміток: ") (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1))))
       (if ss (setq ss_source (strcat "щойно вибраних блоків (" (itoa (sslength ss)) " бл.)")) (progn (princ "\nБлоки 'PIKET' не вибрано.") (exit))))
  ) ;_ end cond

  ;; --- Основна логіка ---
  (if ss
    (progn ; Початок блоку IF ss
      (setq totalCount (sslength ss))
      (princ (strcat "\nПеревірка " (itoa totalCount) " блоків 'PIKET' з " ss_source " на наявність тексту відмітки в точці вставки..."))

      ;; Отримати всі тексти на цільовому шарі один раз
      (setq ssTextLayer (ssget "_X" '((0 . "TEXT,MTEXT") (8 . "21 ВІДМІТКИ"))))
      (if (null ssTextLayer) (princ "\nПопередження: У кресленні не знайдено текстів на шарі '21 ВІДМІТКИ'."))

      (command "_.UNDO" "_Begin") ; Почати групування UNDO

      ;; --- Цикл по вибраних блоках PIKET ---
      (setq i 0)
      (repeat totalCount
        (setq piketEnt (ssname ss i))
        (setq attrValOtmetka nil otmetkaValue nil piketPt nil foundExactText nil) ; Скидання для поточного блоку

        (if (setq piketData (entget piketEnt)) ; Початок IF piketData
          (progn ; Початок PROGN piketData
            (setq piketPt (cdr (assoc 10 piketData))) ; Точка вставки блоку

            ;; Пошук атрибуту "ОТМЕТКА"
            (if (and piketPt (assoc 66 piketData) (= 1 (cdr (assoc 66 piketData)))) ; Перевірка наявності точки та атрибутів
              (progn (setq attEname (entnext piketEnt))
                     (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                       (setq attTag (strcase (cdr (assoc 2 attEdata))))
                       (if (eq "ОТМЕТКА" attTag)
                         (progn (setq attrValOtmetka (cdr (assoc 1 attEdata))) (setq attEname nil)) ; Знайшли, виходимо
                         (setq attEname (entnext attEname)) ; Наступний атрибут
                       ) ;_ end if "ОТМЕТКА"
                     ) ;_ end while attributes
                     (if attrValOtmetka (setq otmetkaValue attrValOtmetka)) ; Присвоюємо значення, якщо знайшли
              ) ;_ end progn attribute search
            ) ;_ end if block has point and attributes

            ;; Перевірка наявності існуючого тексту ТОЧНО в точці вставки
            (if (and piketPt otmetkaValue (/= "" otmetkaValue)) ; Продовжуємо, тільки якщо є точка і не порожня відмітка
              (progn
                (setq foundExactText nil) ; Скидаємо прапорець перед пошуком
                (if ssTextLayer ; Шукаємо тільки якщо є тексти на потрібному шарі
                  (progn
                    (setq j 0)
                    (while (and (< j (sslength ssTextLayer)) (not foundExactText)) ; Початок WHILE text check
                      (setq textEnt (ssname ssTextLayer j))
                      (if (setq textData (entget textEnt)) ; Початок IF textData
                        (progn
                          (setq textPt (cdr (assoc 10 textData))) ; Точка вставки тексту
                          (if (equal piketPt textPt fuzz) ; Порівняння точок з допуском
                              (setq foundExactText T) ; Знайшли текст точно в точці
                          ) ;_ end if equal points
                        ) ;_ end progn process text
                      ) ;_ end IF textData
                      (setq j (1+ j))
                    ) ;_ end WHILE text check
                  ) ;_ end progn ssTextLayer exists
                ) ;_ end if ssTextLayer exists

                ;; Створення нового тексту, якщо не знайдено існуючий в точці
                (if (not foundExactText)
                  (progn
                    ;; Створення DXF списку для нового тексту
                    (setq newTextDxf (list
                                       '(0 . "TEXT")
                                       '(100 . "AcDbEntity")
                                       '(8 . "21 ВІДМІТКИ") ; Цільовий шар
                                       '(100 . "AcDbText")
                                       (cons 10 piketPt) ; Точка вставки = точка вставки блоку
                                       (cons 40 (getvar "TEXTSIZE")) ; Поточна висота тексту
                                       (cons 1 otmetkaValue) ; Значення тексту
                                       (cons 7 (getvar "TEXTSTYLE")) ; Поточний стиль тексту
                                       (cons 50 0.0) ; Кут повороту (0)
                                       ; '(72 . 0) '(73 . 0) ; Вирівнювання (опціонально, 0=Left/Baseline)
                                     ) ;_ end list
                    ) ;_ end setq newTextDxf
                    ;; Створення сутності
                    (if (entmake newTextDxf)
                        (progn
                          (setq createdCount (1+ createdCount))
                          ;(princ (strcat "\n  + Створено текст '" otmetkaValue "' для блоку <" (vl-princ-to-string piketEnt) ">")) ; Опціональне детальне повідомлення
                        ) ;_ end progn entmake success
                        (princ (strcat "\n  ! Помилка створення тексту для блоку <" (vl-princ-to-string piketEnt) ">"))
                    ) ;_ end if entmake
                  ) ;_ end progn create text
                ) ;_ end if not foundExactText
              ) ;_ end progn piketPt and otmetkaValue valid
            ) ;_ end if piketPt and otmetkaValue valid
          ) ;_ end PROGN piketData
        ) ;_ end IF piketData
        (setq i (1+ i))
      ) ;_ end repeat

      (command "_.UNDO" "_End") ; Завершити групування UNDO

      ;; Фінальне повідомлення
      (if (> createdCount 0)
        (princ (strcat "\nОбробку завершено. Створено " (itoa createdCount) " відсутніх текстових відміток у точках вставки блоків."))
        (princ "\nОбробку завершено. Відсутніх текстових відміток у точках вставки не знайдено або не вдалося створити.")
      ) ;_ end if

    ) ;_ end progn IF ss
    (princ "\nНе вдалося визначити об'єкти для обробки (немає блоків 'PIKET' у вибірці).")
  ) ;_ end if ss

  ;; --- Відновлення середовища та вихід ---
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (setq *error* nil)
  (princ) ;; Чистий вихід
) ;; кінець defun c:CREATE_ZMARKER


;; --- Повідомлення про завантаження ---
(princ "\nLISP-скрипти (v5.5 + CREATE_ZMARKER v1.0) завантажено.") ; Оновлено версію

(princ "\nКоманди:")
(princ "\n  SEARCH              - Пошук блоків 'PIKET' за атрибутом 'НОМЕРА'.")
(princ "\n  PASTEHERE           - Вставка об'єкта з буфера в точки блоків.")
(princ "\n  CHECKPOINTS         - Перевірка Z та атрибуту 'ОТМЕТКА' у блоках 'PIKET'.")
(princ "\n  REPLACENAME         - Заміна підстроки в атрибуті 'НОМЕРА' блоків 'PIKET'.")
(princ "\n  RENAME_OKM          - Оновлення тексту ('№...') біля блоків 'PIKET'.")
(princ "\n  RENAME_ZMARKER      - Крок 1: Знаходить/підсвічує тексти для оновлення значенням 'ОТМЕТКА'.")
(princ "\n  RENAME_ZMARKER_RUN  - Крок 2: Виконує оновлення для вибраних текстів Z.")
(princ "\n  CREATE_ZMARKER      - Створює відсутні тексти 'ОТМЕТКА' в точках вставки блоків 'PIKET'.") ; <-- Нова команда

(princ) ;; Чистий вихід