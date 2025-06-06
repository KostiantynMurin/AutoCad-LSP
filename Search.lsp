;; --- LISP Скрипт для Пошуку Блоків та Наступної Вставки/Перевірки/Перейменування (v6.0 - Фінальна версія) ---
;; --- Включає SEARCH, PASTEHERE, CHECKPOINTS, REPLACENAME, RENAME_OKM, CREATE_ZMARKER ---
;; --- Пріоритет вибору змінено: Спочатку PickFirst, потім результат SEARCH, потім запит ---
;; --- CREATE_ZMARKER створює текст і вставляє символ з буфера ---

;; Глобальна змінна для зберігання результату пошуку
(setq *g_last_search_result* nil)

;; Глобальна змінна для запам'ятовування останньої висоти тексту для CREATE_ZMARKER
(setq *g_create_zmarker_last_height* nil)
;; Глобальна змінна для запам'ятовування останнього кута повороту тексту для CREATE_ZMARKER
(setq *g_create_zmarker_last_angle* nil) ; Ініціалізуємо як nil

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
    ;; (if (and old_g_last_search (not *g_last_search_result*)) ; Якщо пошук був скасований/невдалий, а старий результат існував
        (setq *g_last_search_result* nil) ; Явно скинути, якщо скасовано через ESC
    ;; )
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
    ;; 1. Перевірити ПОТОЧНУ ВИБІРКУ (PickFirst) СПОЧАТКУ
    ((setq ss (ssget "_I" '((0 . "INSERT")))) ; Отримати PickFirst, що є блоком/вставкою
     (setq ss_source (strcat "поточної вибірки (" (itoa (sslength ss)) " об.)"))
    )
    ;; 2. Перевірити збережений результат SEARCH (ЯКЩО ПОПЕРЕДНЄ НЕ СПРАЦЮВАЛО)
    ((and (null ss) ; Перевіряємо, тільки якщо 'ss' ще не визначено
          (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "збереженого результату пошуку (" (itoa (sslength ss)) " об.)"))
    )
    ;; 3. Запросити користувача вибрати об'єкти (ЯКЩО НІЧОГО НЕ ЗНАЙДЕНО)
    (T
     (princ "\nНе знайдено попередньої вибірки або збереженого пошуку.")
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
  (setq ss nil ss_source "") ; Ініціалізація
  (cond
    ;; 1. Перевірити ПОТОЧНУ ВИБІРКУ (PickFirst) СПОЧАТКУ (з фільтром)
    ((setq ss (ssget "_I" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Отримати PickFirst, що є PIKET з атрибутами
     (setq ss_source (strcat "поточної вибірки (відфільтровано до " (itoa (sslength ss)) " блоків 'PIKET')"))
    )
    ;; 2. Перевірити збережений результат SEARCH (ЯКЩО ПОПЕРЕДНЄ НЕ СПРАЦЮВАЛО)
    ((and (null ss) ; Перевіряємо, тільки якщо 'ss' ще не визначено
          (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "збереженого результату пошуку (" (itoa (sslength ss)) " об.)"))
    )
    ;; 3. Запросити користувача вибрати об'єкти (ЯКЩО НІЧОГО НЕ ЗНАЙДЕНО)
    (T
     (princ "\nНе знайдено попередньої вибірки або збереженого пошуку.")
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
  (setq ss nil ss_source "") ; Ініціалізація
  (cond
    ;; 1. Перевірити ПОТОЧНУ ВИБІРКУ (PickFirst) СПОЧАТКУ (з фільтром)
    ((setq ss (ssget "_I" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Отримати PickFirst, що є PIKET з атрибутами
     (setq ss_source (strcat "поточної вибірки (відфільтровано до " (itoa (sslength ss)) " блоків 'PIKET')"))
    )
    ;; 2. Перевірити збережений результат SEARCH (ЯКЩО ПОПЕРЕДНЄ НЕ СПРАЦЮВАЛО)
    ((and (null ss) ; Перевіряємо, тільки якщо 'ss' ще не визначено
          (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "збереженого результату пошуку (" (itoa (sslength ss)) " об.)"))
    )
    ;; 3. Запросити користувача вибрати об'єкти (ЯКЩО НІЧОГО НЕ ЗНАЙДЕНО)
    (T
     (princ "\nНе знайдено попередньої вибірки або збереженого пошуку.")
     (princ "\nВиберіть блоки 'PIKET', в яких потрібно замінити значення атрибуту 'НОМЕРА': ")
     (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Фільтр для PIKET з атрибутами
     (if ss
       (setq ss_source (strcat "щойно вибраних блоків 'PIKET' (" (itoa (sslength ss)) " об.)"))
       (progn (princ "\nБлоки 'PIKET' не вибрано. Команду скасовано.") (exit))
     )
    )
  ) ; кінець cond

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
;; СКРИПТ 5.1: ОНОВЛЕННЯ АТРИБУТУ "НОМЕР" В БЛОЦІ ОПОРИ (v5.6.0 - Фінальна версія)
;; ====================================================================
;; Команда: RENAME_OKM_SUPPORT 
;; Опис:
;; Бере набір вибірки "PIKET". Для кожного блоку:
;; 1. Отримує значення атрибута "НОМЕРА", коректно обробляючи як рядкові, так і числові значення.
;; 2. Вилучає алфавітно-цифровий номер за двома патернами:
;;    - З перших дужок: ОКМ(22a)XYZ -> "22a"
;;    - Після префіксу "ОКМ" і до наступних дужок: ОКМ22a(...)XYZ -> "22a"
;; 3. Шукає блок "Опора КМ (з.б.)" за точним іменем дуже близько до точки вставки блоку "PIKET".
;; 4. Якщо блок опори знайдено, збирає інформацію для оновлення його атрибута "НОМЕР".
;; 5. Після перевірки всіх блоків, виділяє кандидатів і запитує підтвердження на оновлення.
;; ====================================================================


;; --- Допоміжна функція: Перевіряє, чи рядок складається тільки з ЛАТИНСЬКИХ літер та/або цифр ---
(defun app:string-is-alphanumeric-p (str / char-code-list)
  (if (and str (> (strlen str) 0)) 
      (progn
        (setq char-code-list (mapcar 'ascii (vl-string->list str)))
        (vl-every '(lambda (char-code)
                     (or
                       (<= (ascii "0") char-code (ascii "9"))  
                       (<= (ascii "A") char-code (ascii "Z"))  
                       (<= (ascii "a") char-code (ascii "z"))  
                     )
                   )
                  char-code-list
        )
      )
      nil 
  )
)

;; --- Основна функція ---
(defun c:RENAME_OKM_SUPPORT ( / *error* ss ss_source i enamePiket edataPiket attEname attEdata attTag
                           attrValNomera attrValNomera_raw openParen_v1 closeParen_v1 candidate_v1 
                           extractedNum processed_support_ents_list support_block_name ssSupportBlock 
                           supportBlockEnt supportBlockData targetAttTag targetAttEname targetAttData 
                           currentAttVal processedCount totalCount oldCmdecho fuzz_dist p1_fuzz p2_fuzz 
                           texts_to_update_info ssHighlight potentialUpdateCount answer actualUpdateCount
                           pikets_missing_support_block found_attrib_for_update
                         )

  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command-s "_.UNDO" "_End"))
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\nПомилка в RENAME_OKM_SUPPORT (v5.6.0): " msg)))
    )
    (setq *g_last_search_result* nil) 
    (setq *error* nil) 
    (princ)
  )

  (setq processedCount 0 oldCmdecho nil ss nil ss_source "" texts_to_update_info nil
        processed_support_ents_list nil ssHighlight nil potentialUpdateCount 0 
        actualUpdateCount 0 answer nil pikets_missing_support_block nil
        support_block_name "Опора КМ (з.б.)" 
        fuzz_dist 1e-6 
  )
  (setq oldCmdecho (getvar "CMDECHO"))
  (princ (strcat "\nОновлення атрибутів 'НОМЕР' для блоків '" support_block_name "' (v5.6.0)..."))

  (setq ss nil ss_source "") 
  (cond
    ((setq ss (ssget "_I" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) 
     (setq ss_source (strcat "поточної вибірки (" (itoa (sslength ss)) " PIKET)"))
    )
    ((and (null ss) (boundp '*g_last_search_result*) *g_last_search_result* (= 'PICKSET (type *g_last_search_result*)) (> (sslength *g_last_search_result*) 0) )
     (setq ss *g_last_search_result*) (setq ss_source (strcat "збереженого пошуку (" (itoa (sslength ss)) " об.)"))
    )
    (T
     (princ "\nВиберіть блоки 'PIKET': ") (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) 
     (if ss (setq ss_source (strcat "щойно вибраних (" (itoa (sslength ss)) " PIKET)"))
       (progn (princ "\nPIKET не вибрано. Команду скасовано.") (exit))
     )
    )
  ) 

  (if ss
    (progn
      (setq totalCount (sslength ss))
      (princ (strcat "\nПошук опор біля " (itoa totalCount) " PIKET з " ss_source "..."))
      (setq i 0)
      (repeat totalCount
        (setq enamePiket (ssname ss i))
        (setq extractedNum nil attrValNomera nil attrValNomera_raw nil)

        (if (setq edataPiket (entget enamePiket))
          (progn
            (setq processedCount (1+ processedCount))
            (setq blockPt (cdr (assoc 10 edataPiket))) 

            ;; 1. ОТРИМАННЯ ТА КОНВЕРТАЦІЯ ЗНАЧЕННЯ АТРИБУТА "НОМЕРА"
            (if (and blockPt (assoc 66 edataPiket) (= 1 (cdr (assoc 66 edataPiket))))
              (progn
                (setq attEname (entnext enamePiket))
                (while (and attEname (not attrValNomera) (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                  (if (eq "НОМЕРА" (strcase (cdr (assoc 2 attEdata))))
                    (progn
                      (setq attrValNomera_raw (cdr (assoc 1 attEdata)))
                     ;; БЛОК КОНВЕРТАЦІЇ ТИПУ (виправлена версія)
                      (cond
                        ((stringp attrValNomera_raw) ; Якщо це вже рядок
                          (setq attrValNomera attrValNomera_raw)
                        )
                        ((numberp attrValNomera_raw) ; Якщо це число
                          (setq attrValNomera (rtos attrValNomera_raw 2 0)) ; Перетворюємо його на рядок
                        )
                        (t ; Для всіх інших випадків (nil, помилкові дані тощо)
                          (if (and attrValNomera_raw (/= 'STR (type attrValNomera_raw))) ; Попередження, якщо тип несподіваний, але не nil
                              (princ (strcat "\n   ПОПЕРЕДЖЕННЯ: 'НОМЕРА' для <" (vl-princ-to-string enamePiket) "> має несподіваний тип: " (vl-prin1-to-string (type attrValNomera_raw))))
                          )
                          (setq attrValNomera "") ; Призначаємо порожній рядок
                        )
                      )
                    )
                  )
                  (if attrValNomera (setq attEname nil) (setq attEname (entnext attEname)))
                )
              )
            )

            ;; 2. ВИЛУЧЕННЯ extractedNum З attrValNomera
            (setq extractedNum nil) 
            (if (and attrValNomera (> (strlen attrValNomera) 0))
              (progn
                ;; СПРОБА 1: (АЛФАВІТНО-ЦИФРОВИЙ номер у дужках)
                (setq openParen_v1 (vl-string-search "(" attrValNomera))
                (if openParen_v1
                  (progn
                    (setq closeParen_v1 (vl-string-search ")" attrValNomera (+ openParen_v1 1)))
                    (if (and closeParen_v1 (> closeParen_v1 openParen_v1))
                      (progn
                        (setq candidate_v1 (substr attrValNomera (+ openParen_v1 2) (- closeParen_v1 openParen_v1 1)))
                        (if (app:string-is-alphanumeric-p candidate_v1) (setq extractedNum candidate_v1))
                      )
                    )
                  )
                )
                ;; СПРОБА 2: ОКМ<алфавітно-цифровий номер>(...)
                (if (not extractedNum) 
                  (let ((prefix_str "ОКМ"))
                    (if (and (>= (strlen attrValNomera) (strlen prefix_str)) (= 0 (vl-string-search prefix_str attrValNomera))) 
                      (let ((openParen_v2 (vl-string-search "(" attrValNomera (strlen prefix_str))))
                        (if openParen_v2 
                          (let* ((num_start_idx (strlen prefix_str))
                                 (num_len (- openParen_v2 num_start_idx)))
                            (if (> num_len 0)
                              (let ((candidate_v2 (substr attrValNomera (+ num_start_idx 1) num_len)))
                                (if (app:string-is-alphanumeric-p candidate_v2) (setq extractedNum candidate_v2))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )

            ;; 3. ПОШУК БЛОКУ ОПОРИ ТА ЗБІР КАНДИДАТІВ
            (if (and extractedNum blockPt)
              (progn
                (setq p1_fuzz (list (- (car blockPt) fuzz_dist) (- (cadr blockPt) fuzz_dist) (- (caddr blockPt) fuzz_dist)))
                (setq p2_fuzz (list (+ (car blockPt) fuzz_dist) (+ (cadr blockPt) fuzz_dist) (+ (caddr blockPt) fuzz_dist)))
                (setq ssSupportBlock (ssget "_C" p1_fuzz p2_fuzz (list '(0 . "INSERT") (cons 2 support_block_name) (cons 410 (getvar "CTAB")))))
                (cond
                  ((and ssSupportBlock (= 1 (sslength ssSupportBlock))) 
                   (setq supportBlockEnt (ssname ssSupportBlock 0))
                   (if (not (member supportBlockEnt processed_support_ents_list))
                     (progn
                       (setq texts_to_update_info (cons (list supportBlockEnt extractedNum enamePiket) texts_to_update_info))
                       (setq processed_support_ents_list (cons supportBlockEnt processed_support_ents_list))
                       (setq potentialUpdateCount (1+ potentialUpdateCount))
                     )
                   )
                  )
                  ((and ssSupportBlock (> (sslength ssSupportBlock) 1)) 
                   (princ (strcat "\n   ! ПОПЕРЕДЖЕННЯ: Знайдено кілька (" (itoa (sslength ssSupportBlock)) ") блоків '" support_block_name "' біля PIKET <" (vl-princ-to-string enamePiket) ">. Пропускається."))
                   (setq pikets_missing_support_block (cons enamePiket pikets_missing_support_block))
                  )
                  (T 
                   (princ (strcat "\n   ! ПОПЕРЕДЖЕННЯ: Блок '" support_block_name "' не знайдено біля PIKET <" (vl-princ-to-string enamePiket) ">. Пропускається."))
                   (setq pikets_missing_support_block (cons enamePiket pikets_missing_support_block))
                  )
                )
              )
              (if (and (not extractedNum) attrValNomera)
                  (princ (strcat "\n   ! ПОПЕРЕДЖЕННЯ: Не вдалося вилучити номер з \"" attrValNomera "\" для PIKET <" (vl-princ-to-string enamePiket) ">."))
              )
            )
          )
        ) 
        (setq i (1+ i))
      ) 
      ;; --- Подальша логіка оновлення (залишається без змін) ---
      (if (> potentialUpdateCount 0)
        (progn
          (princ (strcat "\nЗнайдено кандидатів на оновлення: " (itoa potentialUpdateCount)))
          (setq ssHighlight (ssadd))
          (foreach item texts_to_update_info (if (entget (car item)) (ssadd (car item) ssHighlight)))
          (if (> (sslength ssHighlight) 0)
            (progn
              (sssetfirst nil ssHighlight) 
              (initget "Так Ні")
              (setq answer (getkword (strcat "\nОновити атрибути 'НОМЕР' у " (itoa (sslength ssHighlight)) " виділених блоках? [Так/Ні]: ")))
              (if (eq answer "Так")
                (progn
                  (princ "\nВиконую оновлення...") (command "_.UNDO" "_Begin")
                  (foreach item texts_to_update_info
                    (setq supportBlockEnt (car item) extractedNum (cadr item))
                    (if (setq supportBlockData (entget supportBlockEnt))
                      (if (and (assoc 66 supportBlockData) (= 1 (cdr (assoc 66 supportBlockData)))) 
                        (progn
                          (setq targetAttEname (entnext supportBlockEnt) found_attrib_for_update nil)
                          (while (and targetAttEname (eq "ATTRIB" (cdr (assoc 0 (setq targetAttData (entget targetAttEname))))) (not found_attrib_for_update))
                            (if (eq "НОМЕР" (strcase (cdr (assoc 2 targetAttData)))) 
                              (progn
                                (setq currentAttVal (cdr (assoc 1 targetAttData)))
                                (entmod (subst (cons 1 extractedNum) (assoc 1 targetAttData) targetAttData))
                                (setq actualUpdateCount (1+ actualUpdateCount) found_attrib_for_update T)
                              )
                            )
                            (if (not found_attrib_for_update) (setq targetAttEname (entnext targetAttEname)))
                          )
                        )
                      )
                    )
                  ) (command "_.UNDO" "_End") (princ (strcat "\nУспішно оновлено " (itoa actualUpdateCount) " атрибутів.")))
                (progn (princ "\nЗміни скасовано.") (sssetfirst nil nil))
              )
            )
          )
        )
        (if (= processedCount totalCount) (princ "\nНе знайдено кандидатів на оновлення."))
      )
      (princ (strcat "\n\nОперацію завершено. Оброблено PIKET: " (itoa processedCount) "."))
      (if pikets_missing_support_block
          (progn (princ (strcat "\nPIKET, для яких не знайдено опору '" support_block_name "':"))
                 (foreach p (reverse pikets_missing_support_block) (princ (strcat "\n - <" (vl-princ-to-string p) ">")))
          )
      )
    ) 
    (princ "\nНе вдалося визначити PIKET для обробки.")
  ) 
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (setq *g_last_search_result* nil) 
  (setq *error* nil) 
  (princ) 
) 

(princ "\nКоманду RENAME_OKM_SUPPORT (v5.6.0) завантажено. Введіть RENAME_OKM_SUPPORT для запуску.")
(princ)


;; ====================================================================
;; СКРИПТ 8: СТВОРЕННЯ ВІДСУТНІХ ТЕКСТОВИХ ВІДМІТОК Z ТА СИМВОЛІВ (v1.3)
;; ====================================================================
;; Команда: Команда: CREATE_ZMARKER (v1.5 - Додано запит кута повороту)
;; 1. Перевіряє буфер обміну. Якщо не ОК - ВИХІД.
;; 2. Вибирає блоки PIKET.
;; 3. Запитує висоту тексту.
;; 4. Для кожного блоку перевіряє атрибут "ОТМЕТКА".
;; 5. Перевіряє, чи існує текст ТОЧНО в точці вставки блоку.
;; 6. Якщо текст відсутній:
;;    а. Створює новий текст зі значенням "ОТМЕТКА" в точці вставки блоку (стиль Д-431, шар 21, вирівнювання Вліво, задана висота).
;;    б. Вставляє символ з буфера обміну в ту ж точку вставки (якщо буфер містить дані AutoCAD).

(defun c:CREATE_ZMARKER ( / *error* ss ss_source totalCount i piketEnt piketData piketPt
                           attrValOtmetka otmetkaValue ssTextLayer j textEnt textData textPt
                           foundExactText createdCount oldCmdecho fuzz attEname attEdata attTag newTextDxf
                           userHeight prompt_str inputHeight userAngle prompt_angle_str inputAngle
                           pastedEnt entBeforePaste ; Додано entBeforePaste
                          )
  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command-s "_.UNDO" "_End"))
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg) (princ "\nСкасовано."))
          ((vl-string-search "quit / exit abort" msg) (princ "\nСкасовано."))
          (T (princ (strcat "\nПомилка в CREATE_ZMARKER: " msg)))
    )
    (setq *error* nil)
    (princ)
  )

  ;; --- Ініціалізація ---
  (setq ss nil ss_source nil totalCount 0 i 0 piketEnt nil piketData nil piketPt nil
        attrValOtmetka nil otmetkaValue nil ssTextLayer nil j 0 textEnt nil textData nil textPt nil
        foundExactText nil createdCount 0 oldCmdecho nil fuzz 1e-9 attEname nil attEdata nil attTag nil newTextDxf nil
        userHeight nil prompt_str nil inputHeight nil
        userAngle nil prompt_angle_str nil inputAngle nil
        pastedEnt nil entBeforePaste nil ; Ініціалізація нових змінних
  )
  (setq oldCmdecho (getvar "CMDECHO"))

  ;; --- Перевірка буфера обміну ---
  (if (= 0 (getvar "CLIPROPS"))
    (progn
      (alert "Буфер обміну порожній або не містить даних AutoCAD.\nСпочатку скопіюйте умовне позначення відмітки.")
      (exit)
    )
  )

  ;; --- Визначення робочого набору вибірки (ss) для блоків PIKET ---
  (setq ss nil ss_source "") ; Ініціалізація
  (cond
    ;; 1. Перевірити ПОТОЧНУ ВИБІРКУ (PickFirst) СПОЧАТКУ (з фільтром)
    ((setq ss (ssget "_I" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Отримати PickFirst, що є PIKET з атрибутами
     (setq ss_source (strcat "поточної вибірки (відфільтровано до " (itoa (sslength ss)) " блоків 'PIKET')"))
    )
    ;; 2. Перевірити збережений результат SEARCH (ЯКЩО ПОПЕРЕДНЄ НЕ СПРАЦЮВАЛО)
    ((and (null ss) ; Перевіряємо, тільки якщо 'ss' ще не визначено
          (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "збереженого результату пошуку (" (itoa (sslength ss)) " об.)"))
    )
    ;; 3. Запросити користувача вибрати об'єкти (ЯКЩО НІЧОГО НЕ ЗНАЙДЕНО)
    (T
     (princ "\nНе знайдено попередньої вибірки або збереженого пошуку.")
     (princ "\nВиберіть блоки 'PIKET' для створення відсутніх відміток: ")
     (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Фільтр для PIKET з атрибутами
     (if ss
       (setq ss_source (strcat "щойно вибраних блоків (" (itoa (sslength ss)) " об.)"))
       (progn (princ "\nБлоки 'PIKET' не вибрано.") (exit))
     )
    )
  ) ; кінець cond

  ;; --- Основна логіка ---
  (if ss
    (progn
      ;; --- Запит ВИСОТИ тексту ---
      (if (or (null (boundp '*g_create_zmarker_last_height*)) (null *g_create_zmarker_last_height*) (not (numberp *g_create_zmarker_last_height*)))
          (setq *g_create_zmarker_last_height* 0.9))
      (setq prompt_str (strcat "\nВведіть висоту тексту [1:1000=1.8, 1:500=0.9] <" (rtos *g_create_zmarker_last_height* 2 4) ">: "))
      (initget (+ 2 4)) ; Дозволити Enter, заборонити 0 та негативні
      (setq inputHeight (getreal prompt_str))
      (if inputHeight (setq userHeight inputHeight) (setq userHeight *g_create_zmarker_last_height*))
      (setq *g_create_zmarker_last_height* userHeight)

      ;; --- Запит КУТА ПОВОРОТУ тексту (і тепер об'єкта з буфера) ---
      (if (or (null (boundp '*g_create_zmarker_last_angle*)) (null *g_create_zmarker_last_angle*) (not (numberp *g_create_zmarker_last_angle*)))
          (setq *g_create_zmarker_last_angle* 0.0))
      (setq prompt_angle_str (strcat "\nВведіть кут повороту тексту та об'єкта (градуси) <" (angtos *g_create_zmarker_last_angle* 0 4) ">: "))
      (setq inputAngle (getangle prompt_angle_str)) ; getangle повертає кут в радіанах
      (if inputAngle (setq userAngle inputAngle) (setq userAngle *g_create_zmarker_last_angle*))
      (setq *g_create_zmarker_last_angle* userAngle)

      (setvar "CMDECHO" 0) ; Вимкнути ехо команд тепер

      (setq totalCount (sslength ss))
      (princ (strcat "\nПеревірка " (itoa totalCount) " блоків 'PIKET' з " ss_source "..."))

      (setq ssTextLayer (ssget "_X" '((0 . "TEXT,MTEXT") (8 . "21 ВІДМІТКИ"))))
      (if (null ssTextLayer) (princ "\nПопередження: У кресленні не знайдено текстів на шарі '21 ВІДМІТКИ'."))

      (command "_.UNDO" "_Begin")

      ;; --- Цикл по вибраних блоках PIKET ---
      (setq i 0)
      (repeat totalCount
        (setq piketEnt (ssname ss i))
        (setq attrValOtmetka nil otmetkaValue nil piketPt nil foundExactText nil)

        (if (setq piketData (entget piketEnt))
          (progn
            (setq piketPt (cdr (assoc 10 piketData)))
            ;; Пошук атрибуту "ОТМЕТКА"
            (if (and piketPt (assoc 66 piketData) (= 1 (cdr (assoc 66 piketData))))
              (progn
                (setq attEname (entnext piketEnt))
                (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                  (setq attTag (strcase (cdr (assoc 2 attEdata))))
                  (if (eq "ОТМЕТКА" attTag)
                    (progn (setq attrValOtmetka (cdr (assoc 1 attEdata))) (setq attEname nil)) 
                    (setq attEname (entnext attEname))
                  )
                )
                (if attrValOtmetka (setq otmetkaValue attrValOtmetka))
              )
            )

            (if (and piketPt otmetkaValue (/= "" otmetkaValue))
              (progn
                (setq foundExactText nil)
                (if ssTextLayer
                  (progn
                    (setq j 0)
                    (while (and (< j (sslength ssTextLayer)) (not foundExactText))
                      (setq textEnt (ssname ssTextLayer j))
                      (if (setq textData (entget textEnt))
                        (progn
                          (setq textPt (cdr (assoc 10 textData)))
                          (if (equal piketPt textPt fuzz) (setq foundExactText T))
                        )
                      )
                      (setq j (1+ j))
                    )
                  )
                )

                (if (not foundExactText)
                  (progn
                    (setq newTextDxf (list
                                       '(0 . "TEXT") '(100 . "AcDbEntity") '(8 . "21 ВІДМІТКИ") '(100 . "AcDbText")
                                       (cons 10 piketPt) (cons 40 userHeight) (cons 1 (strcat " " otmetkaValue))
                                       '(7 . "Д-431") (cons 50 userAngle) 
                                       '(72 . 0) '(73 . 0)
                                     )
                    )
                    (if (entmake newTextDxf)
                      (progn
                        (setq createdCount (1+ createdCount))
                        (setq entBeforePaste (entlast)) ; Запам'ятовуємо останній об'єкт (це має бути створений текст)
                        
                        (command "_.PASTECLIP" "_non" piketPt) ; Вставка об'єкта з буфера обміну
                        (setq pastedEnt (entlast)) ; Отримуємо останній об'єкт після вставки

                        ;; --- МОДИФІКАЦІЯ: Поворот вставленого з буфера об'єкта ---
                        ;; Перевіряємо, чи pastedEnt існує і чи він відрізняється від об'єкта перед вставкою (тобто, чи PASTECLIP створив новий об'єкт)
                        (if (and pastedEnt entBeforePaste (not (eq pastedEnt entBeforePaste)))
                            (progn
                                ;; (princ (strcat "\nDebug: Rotating pasted entity: " (vl-princ-to-string pastedEnt))) ; Для відладки
                                (command "_.ROTATE" pastedEnt "" "_non" piketPt (rtos (/ (* userAngle 180.0) pi) 2 8))
                            )
                            (if (and pastedEnt (eq pastedEnt entBeforePaste))
                                (princ (strcat "\nПопередження: PASTECLIP не створив новий об'єкт, відмінний від тексту (можливо, буфер обміну порожній або містить несумісні дані). Поворот об'єкта з буфера пропущено для PIKET: " (vl-princ-to-string piketEnt)))
                                ;; else: pastedEnt is nil, or entBeforePaste was nil (less likely here)
                            )
                        )
                        ;; --- КІНЕЦЬ МОДИФІКАЦІЇ ---
                      )
                      (princ (strcat "\n  ! Помилка створення тексту для блоку <" (vl-princ-to-string piketEnt) ">"))
                    )
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      ) ; end repeat

      (command "_.UNDO" "_End") ; Виправлено синтаксис

      ;; Фінальне повідомлення
      (if (> createdCount 0)
          (princ (strcat "\nОбробку завершено. Створено " (itoa createdCount) " відсутніх пар (текст відмітки + символ)..."))
          (princ "\nОбробку завершено. Відсутніх текстових відміток у точках вставки не знайдено...")
      )

    ) ; end progn IF ss
    (princ "\nНе вдалося визначити об'єкти для обробки (немає блоків 'PIKET' у вибірці).")
  ) ; end if ss

  ;; --- Відновлення середовища та вихід ---
  (setvar "CMDECHO" oldCmdecho)
  (setq *error* nil)
  (princ) ;; Чистий вихід
) ;; кінець defun c:CREATE_ZMARKER

;;; CREATE_ZMARKER_BLOCK
;;; Вставляє екземпляри обраного блоку-маркера в точки вставки блоків "PIKET",
;;; зчитує значення атрибута "ОТМЕТКА" з "PIKET" та записує його
;;; в атрибут "ВІДМІТКА" нового блоку-маркера.
;;; Використовує ActiveX для надійної вставки та оновлення атрибутів.

(defun c:CREATE_ZMARKER_BLOCK ( / *error* ss ss_source totalCount i piketEnt piketData piketPt
                           attrValOtmetka otmetkaValue k blockEnt blockEntData blockPt
                           foundExactBlockMarker createdCount oldCmdecho fuzz attEname attEdata attTag
                           userAngle prompt_angle_str inputAngle
                           templateBlockEnt templateBlockData templateBlockName
                           targetLayer oldAttdia oldAttreq tolerance pMin pMax ssBlockMarkersInVicinity
                           ;; ActiveX related variables
                           acadApp acadDoc modelSpace insertionPoint newBlockVLAObject
                           att_tag_to_set insert_attempt_result
                          )
  ;; --- Локальна допоміжна функція для перевірки наявності атрибуту у визначенні блоку ---
  (defun LM:CheckBlockAttribDef (blockname att_tag_check / blk_obj_def attdef_found blocks_collection)
    (setq attdef_found nil)
    (if (not acadDoc) (setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object))))
    (setq blocks_collection (vla-get-Blocks acadDoc))
    (if (not (vl-catch-all-error-p (setq blk_obj_def (vla-item blocks_collection blockname))))
      (vlax-for entity blk_obj_def
        (if (= "AcDbAttributeDefinition" (vla-get-ObjectName entity))
          (if (= (strcase (vla-get-TagString entity)) (strcase att_tag_check))
            (setq attdef_found T)
          )
        )
      )
      (princ (strcat "\n  Помилка: Блок з ім'ям '" blockname "' не знайдено в таблиці блоків."))
    )
    attdef_found
  )

  ;; --- Локальна допоміжна функція для встановлення значення атрибуту VLA (оновлена) ---
  (defun LM:SetAttributeValueVLA (block_vla att_tag_str new_val_str / attributes_variant attributes_safearray attribute_list att_obj current_tag_str found_att)
    (setq found_att nil)
    (if (and block_vla (= (type block_vla) 'VLA-OBJECT) (not (vlax-object-released-p block_vla)))
      (if (= (vla-get-HasAttributes block_vla) :vlax-true)
        (progn
          (setq attributes_variant (vl-catch-all-apply 'vla-GetAttributes (list block_vla))) ; Отримуємо Variant або помилку
          
          (if (vl-catch-all-error-p attributes_variant) ; Спочатку перевіряємо на помилку
            (princ (strcat "\n    Помилка отримання атрибутів для блоку: " (vl-catch-all-error-message attributes_variant)))
            (progn ; Помилки не було, attributes_variant - це Variant
              (setq attributes_safearray (vlax-variant-value attributes_variant)) ; Витягуємо значення з Variant (має бути SafeArray)
              
              (if (and attributes_safearray (= (type attributes_safearray) 'SAFEARRAY)) ; Перевіряємо, чи це дійсно SafeArray
                (progn
                  (setq attribute_list (vlax-safearray->list attributes_safearray)) ; Конвертуємо SafeArray в список LISP
                  (foreach att_obj attribute_list ; Ітеруємо по списку
                    (if (and att_obj (= (type att_obj) 'VLA-OBJECT)) ; Переконуємося, що кожен елемент - VLA-об'єкт
                       (progn
                          (setq current_tag_str (vla-get-TagString att_obj))
                          (if (= (strcase current_tag_str) (strcase att_tag_str))
                            (progn
                              (vl-catch-all-apply 'vla-put-TextString (list att_obj new_val_str))
                              (setq found_att T)
                              ;; Можна було б додати (setq attribute_list nil) для виходу з foreach, але це не завжди надійно
                            )
                          )
                       )
                       (princ (strcat "\n    Попередження: Елемент у списку атрибутів не є VLA-OBJECT: " (vl-princ-to-string att_obj)))
                    )
                  )
                  (if found_att (vla-Update block_vla)) ; Оновлюємо блок, якщо атрибут було змінено
                )
                (princ (strcat "\n    Попередження: vla-GetAttributes не повернула очікуваний SafeArray. Тип значення варіанту: " (vl-princ-to-string (type attributes_safearray))))
              )
            )
          )
        )
        (princ (strcat "\n    Попередження: Блок " (vla-get-Name block_vla) " не має атрибутів (HasAttributes is False)."))
      )
      (princ "\n    Помилка: Передано невалідний VLA-об'єкт блоку для LM:SetAttributeValueVLA.")
    )
    found_att
  )

  ;; --- Функція обробки помилок ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if oldAttdia (setvar "ATTDIA" oldAttdia))
    (if oldAttreq (setvar "ATTREQ" oldAttreq))
    (if acadDoc (if (= 8 (logand 8 (getvar "UNDOCTL"))) (vla-EndUndoMark acadDoc)))
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg) (princ "\nСкасовано."))
          ((vl-string-search "quit / exit abort" msg) (princ "\nСкасовано."))
          (T (princ (strcat "\nПомилка в CREATE_ZMARKER_BLOCK: " msg)))
    )
    (setq *error* nil acadApp nil acadDoc nil modelSpace nil) 
    (princ)
  )

  ;; --- Ініціалізація ---
  (if (null vlax-get-acad-object) (vl-load-com))

  (setq ss nil ss_source nil totalCount 0 i 0 piketEnt nil piketData nil piketPt nil
        attrValOtmetka nil otmetkaValue nil k 0 blockEnt nil blockEntData nil blockPt nil
        foundExactBlockMarker nil createdCount 0 oldCmdecho (getvar "CMDECHO") fuzz 1e-9
        attEname nil attEdata nil attTag nil
        userAngle nil prompt_angle_str nil inputAngle nil
        templateBlockEnt nil templateBlockData nil templateBlockName nil
        targetLayer "21 ВІДМІТКИ" att_tag_to_set "ВІДМІТКА"
        oldAttdia (getvar "ATTDIA") oldAttreq (getvar "ATTREQ")
        tolerance 0.01 pMin nil pMax nil ssBlockMarkersInVicinity nil
        insert_attempt_result nil
  )
  
  (setvar "CMDECHO" 0)
  (setvar "ATTDIA" 0)
  (setvar "ATTREQ" 1) 

  (setq acadApp (vlax-get-acad-object))
  (setq acadDoc (vla-get-ActiveDocument acadApp))
  (setq modelSpace (vla-get-ModelSpace acadDoc))

  ;; --- Запит користувача на вибір блоку-шаблону ---
  (princ "\nОберіть існуючий екземпляр блоку-маркера на кресленні.")
  (setq templateBlockEnt (car (entsel (strcat "\n>> Оберіть блок-маркер (має містити атрибут '" att_tag_to_set "'): "))))
  (if (null templateBlockEnt)
    (progn (princ "\nБлок-маркер не обрано. Роботу скрипта скасовано.") (*error* "Вибір блоку скасовано") (exit))
  )
  (setq templateBlockData (entget templateBlockEnt))
  (if (or (null templateBlockData) (not (assoc 2 templateBlockData)) (/= "INSERT" (cdr (assoc 0 templateBlockData))))
    (progn (princ "\nОбраний об'єкт не є блоком або не вдалося отримати його дані. Роботу скрипта скасовано.") (*error* "Некоректний вибір блоку") (exit))
  )
  (setq templateBlockName (vla-get-Name (vlax-ename->vla-object templateBlockEnt))) 

  (if (or (null templateBlockName) (= "" templateBlockName))
      (progn (princ "\nНе вдалося отримати ім'я обраного блоку. Роботу скрипта скасовано.") (*error* "Не вдалося отримати ім'я блоку") (exit))
  )
  (if (not (LM:CheckBlockAttribDef templateBlockName att_tag_to_set))
      (progn
        (princ (strcat "\n*** Помилка: Обраний блок-шаблон '" templateBlockName "' не містить визначення атрибуту з тегом '" att_tag_to_set "'."))
        (princ "\nБудь ласка, оберіть інший блок або додайте необхідний атрибут до цього блоку.")
        (*error* (strcat "Атрибут " att_tag_to_set " не знайдено у визначенні блоку " templateBlockName))
        (exit)
      )
  )
  (princ (strcat "\nБуде використовуватись блок-маркер: '" templateBlockName "'."))

  ;; --- Визначення робочого набору вибірки (ss) для блоків PIKET ---
  (setq ss nil ss_source "")
  (cond
    ((setq ss (ssget "_I" '((0 . "INSERT")(2 . "PIKET")(66 . 1))))
     (setq ss_source (strcat "поточної вибірки (відфільтровано до " (itoa (sslength ss)) " блоків 'PIKET')"))
    )
    ((and (null ss) (boundp '*g_last_search_result*) *g_last_search_result* (= 'PICKSET (type *g_last_search_result*)) (> (sslength *g_last_search_result*) 0))
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "збереженого результату пошуку (" (itoa (sslength ss)) " об.)"))
    )
    (T
     (princ "\nНе знайдено попередньої вибірки або збереженого пошуку.")
     (princ "\nВиберіть блоки 'PIKET' для створення відсутніх маркерів: ")
     (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1))))
     (if ss
       (setq ss_source (strcat "щойно вибраних блоків (" (itoa (sslength ss)) " об.)"))
       (progn (princ "\nБлоки 'PIKET' не вибрано.") (*error* "Блоки PIKET не вибрано") (exit))
     )
    )
  )

  ;; --- Основна логіка ---
  (if ss
    (progn
      (vla-StartUndoMark acadDoc)
      (if (or (null (boundp '*g_create_zmarker_last_angle*)) (null *g_create_zmarker_last_angle*) (not (numberp *g_create_zmarker_last_angle*)))
          (setq *g_create_zmarker_last_angle* 0.0))
      (setq prompt_angle_str (strcat "\nВведіть кут повороту для маркерів (градуси) <" (angtos *g_create_zmarker_last_angle* 0 4) ">: "))
      (setq inputAngle (getangle prompt_angle_str))
      (if inputAngle (setq userAngle inputAngle) (setq userAngle *g_create_zmarker_last_angle*))
      (setq *g_create_zmarker_last_angle* userAngle)

      (setq totalCount (sslength ss))
      (princ (strcat "\nПеревірка " (itoa totalCount) " блоків 'PIKET' з " ss_source "..."))

      (setq i 0)
      (repeat totalCount
        (setq piketEnt (ssname ss i))
        (setq attrValOtmetka nil otmetkaValue nil piketPt nil foundExactBlockMarker nil newBlockVLAObject nil insert_attempt_result nil) 

        (if (setq piketData (entget piketEnt))
          (progn
            (setq piketPt (cdr (assoc 10 piketData)))
            (if (and piketPt (assoc 66 piketData) (= 1 (cdr (assoc 66 piketData))))
              (progn
                (setq attEname (entnext piketEnt))
                (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                  (setq attTag (strcase (cdr (assoc 2 attEdata))))
                  (if (eq "ОТМЕТКА" attTag) 
                    (progn (setq attrValOtmetka (cdr (assoc 1 attEdata))) (setq attEname nil))
                    (setq attEname (entnext attEname))
                  )
                )
                (if attrValOtmetka (setq otmetkaValue attrValOtmetka))
              )
            )

            (if (and piketPt otmetkaValue (/= "" otmetkaValue))
              (progn
                (setq foundExactBlockMarker nil)
                (setq pMin (list (- (car piketPt) tolerance) (- (cadr piketPt) tolerance) (- (caddr piketPt) tolerance)))
                (setq pMax (list (+ (car piketPt) tolerance) (+ (cadr piketPt) tolerance) (+ (caddr piketPt) tolerance)))
                (setq ssBlockMarkersInVicinity (ssget "_C" pMin pMax (list '(0 . "INSERT") (cons 2 templateBlockName))))

                (if ssBlockMarkersInVicinity
                  (progn
                    (setq k 0)
                    (while (and (< k (sslength ssBlockMarkersInVicinity)) (not foundExactBlockMarker))
                      (setq blockEnt (ssname ssBlockMarkersInVicinity k))
                      (setq blockEntData (entget blockEnt))
                      (setq blockPt (cdr (assoc 10 blockEntData)))
                      (if (equal piketPt blockPt fuzz) (setq foundExactBlockMarker T))
                      (setq k (1+ k))
                    )
                  )
                )

                (if (not foundExactBlockMarker)
                  (progn
                    (setq insertionPoint (vlax-3D-point piketPt))
                    (setq insert_attempt_result 
                      (vl-catch-all-apply
                        'vla-InsertBlock
                        (list modelSpace insertionPoint templateBlockName 1.0 1.0 1.0 userAngle)
                      )
                    )

                    (if (vl-catch-all-error-p insert_attempt_result) 
                      (progn
                        (princ (strcat "\n  !! Помилка ActiveX: Не вдалося вставити блок '" templateBlockName "' для PIKET: " 
                                       (vl-princ-to-string piketEnt) ". Причина: " 
                                       (vl-catch-all-error-message insert_attempt_result)))
                      )
                      (progn
                        (setq newBlockVLAObject insert_attempt_result) 
                        (if (and newBlockVLAObject (= (type newBlockVLAObject) 'VLA-OBJECT))
                          (progn
                            (vl-catch-all-apply 'vla-put-Layer (list newBlockVLAObject targetLayer))
                            (if (not (LM:SetAttributeValueVLA newBlockVLAObject att_tag_to_set otmetkaValue))
                                (princ (strcat "\n    ! Попередження: Не вдалося встановити атрибут '" att_tag_to_set 
                                               "' для щойно вставленого блоку в точці " (vl-princ-to-string piketPt)))
                            )
                            (setq createdCount (1+ createdCount))
                          )
                          (princ (strcat "\n  !! Внутрішня помилка: vla-InsertBlock не повернув очікуваний об'єкт (не VLA-OBJECT) для PIKET: " 
                                         (vl-princ-to-string piketEnt) 
                                         ", хоча помилка ActiveX не зафіксована."))
                        )
                      )
                    )
                  )
                  (princ (strcat "\n  Маркер для PIKET <" (vl-princ-to-string piketEnt) "> в точці " (vl-princ-to-string piketPt) " вже існує. Пропущено."))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      ) ; end repeat
      (vla-EndUndoMark acadDoc)
      (if (> createdCount 0)
          (princ (strcat "\nОбробку завершено. Створено " (itoa createdCount) " нових маркерів ('" templateBlockName "')..."))
          (princ (strcat "\nОбробку завершено. Нових маркерів ('" templateBlockName "') не створено.")))
    )
    (princ "\nНе вдалося визначити об'єкти 'PIKET' для обробки.")
  )

  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (if oldAttdia (setvar "ATTDIA" oldAttdia))
  (if oldAttreq (setvar "ATTREQ" oldAttreq))
  (setq acadApp nil acadDoc nil modelSpace nil insertionPoint nil newBlockVLAObject nil insert_attempt_result nil)
  (princ)
)

(princ "\nФункція CREATE_ZMARKER_BLOCK (ActiveX v4 - виправлення типу аргумента) завантажена. Введіть CREATE_ZMARKER_BLOCK для запуску.")
(princ)







;; --- Повідомлення про завантаження ---
(princ "\nLISP-скрипти (v6.0 - Фінальна версія) завантажено.")

(princ "\nКоманди:")
(princ "\n  SEARCH              - Пошук блоків 'PIKET' за атрибутом 'НОМЕРА'.")
(princ "\n  PASTEHERE           - Вставка об'єкта з буфера в точки блоків.")
(princ "\n  CHECKPOINTS         - Перевірка Z та атрибуту 'ОТМЕТКА' у блоках 'PIKET'.")
(princ "\n  REPLACENAME         - Заміна підстроки в атрибуті 'НОМЕРА' блоків 'PIKET'.")
(princ "\n  RENAME_OKM_SUPPORT          - Оновлення тексту ('№...') біля блоків 'PIKET' (Опор Контактної Мережі).")
(princ "\n  CREATE_ZMARKER      - Створює текст висотної відмітки та вставляє умовне позначення з буфера в точках PIKET.")
(princ "\n  CREATE_ZMARKER_BLOCK- вставляє обраний блок зі значенням відмітки в точках PIKET.")

(princ) ;; Чистий вихід