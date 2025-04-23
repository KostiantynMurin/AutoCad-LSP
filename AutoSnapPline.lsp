;; AutoSnapPline.lsp (Версія 8.8 - Додано команду FASTLINE)
;; Порядок: Радіус -> Об'єкт. Функціонал: Entmake полілінія, 2D Відстань.

;; Глобальна змінна для запам'ятовування останнього радіусу для AutoSnapPline
(setq *g_autosnap_last_radius* nil) ; Ініціалізуємо як nil

; --- Допоміжна функція для малювання маркерів радіусу ---
; (Без змін)
(defun draw_radius_markers (center radius color size_ss / p_left p_right p_bottom p_top)
  (if (and center radius (> radius 1e-6) color size_ss (> size_ss 1e-6))
    (progn
      (setq p_left   (list (- (car center) radius) (cadr center)))
      (setq p_right  (list (+ (car center) radius) (cadr center)))
      (setq p_bottom (list (car center) (- (cadr center) radius)))
      (setq p_top    (list (car center) (+ (cadr center) radius)))
      (grdraw (polar p_left   (* 1.5 pi) size_ss) (polar p_left   (* 0.5 pi) size_ss) color 0)
      (grdraw (polar p_right  (* 1.5 pi) size_ss) (polar p_right  (* 0.5 pi) size_ss) color 0)
      (grdraw (polar p_bottom pi         size_ss) (polar p_bottom 0.0      size_ss) color 0)
      (grdraw (polar p_top    pi         size_ss) (polar p_top    0.0      size_ss) color 0)
    )
  )
)

; --- Допоміжна функція для зняття підсвічування ---
; (Без змін)
(defun clear_highlights (ents_list / i ename)
  (if ents_list
    (progn
      (setq i 0)
      (repeat (length ents_list)
        (setq ename (nth i ents_list))
        (if (and ename (= (type ename) 'ENAME) (entget ename))
            (redraw ename 4)
        )
        (setq i (1+ i))
      )
    )
  )
  nil
)

; --- Головна функція команди (зберігаємо оригінальну назву) ---
(defun c:AutoSnapPline ( / *error* old_osmode old_cmdecho old_blipmode ; Системні змінні
                         snap_radius start_ent start_pt ent_data initial_ent_type ; Вхідні дані
                         gr_status gr_data cursor_pt draw_color marker_size_ss loop_counter ; Цикл
                         ll ur filter nearby_ss found_now_ents i ename edata obj_pt ; Пошук об'єктів
                         highlighted_ents ; Список підсвічених
                         detection_counter detection_threshold ; Оптимізація
                         cursor_pt_2d obj_pt_2d dist_2d ; 2D відстань
                         vertices_list temp_pline_segment_color pline_dxf vertex ; Entmake
                         prompt_radius_str 
                         )
  (vl-load-com)

  ; --- Обробник помилок ---
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if old_blipmode (setvar "BLIPMODE" old_blipmode))
    (setq highlighted_ents (clear_highlights highlighted_ents))
    (redraw)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
        (princ (strcat "\nСкасовано/Помилка: " msg)))
    (princ)
  )

  ; --- Збереження та налаштування системних змінних ---
  (setq old_cmdecho (getvar "CMDECHO"))
  (setq old_osmode (getvar "OSMODE"))
  (setq old_blipmode (getvar "BLIPMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "BLIPMODE" 0)

  ; --- 1. Отримати Радіус Відображення/Пошуку (з пам'яттю) ---
  ; Ініціалізація / використання збереженого значення (напр. 1.0 за замовчуванням)
  (if (or (null (boundp '*g_autosnap_last_radius*)) (null *g_autosnap_last_radius*) (not (numberp *g_autosnap_last_radius*)))
      (setq *g_autosnap_last_radius* 1) ; Початкове значення за замовчуванням, якщо не задано
  )
  ; Формування підказки зі значенням за замовчуванням
  (setq prompt_radius_str (strcat "\nВведіть радіус відображення/пошуку <" (rtos *g_autosnap_last_radius* 2 4) ">: "))
  (initget 6) ; Заборонити нуль та негативні значення (2+4)
  ; Отримати відстань, передаючи значення за замовчуванням
  ; getdist поверне *g_autosnap_last_radius*, якщо користувач натисне Enter
  (setq snap_radius (getdist prompt_radius_str *g_autosnap_last_radius*))

  ; Перевірка чи отримано валідне значення (getdist поверне nil при ESC)
  (if (or (null snap_radius) (<= snap_radius 0))
      (*error* "Введено недійсний радіус або скасовано.")
      ; Якщо радіус валідний, зберігаємо його для наступного разу
      (setq *g_autosnap_last_radius* snap_radius)
  )

  ; --- 2. Отримати Стартовий Об'єкт та його тип ---
  (setq start_pt nil initial_ent_type nil)
  (while (not start_pt)
    (setq start_ent (entsel (strcat "\nВиберіть стартовий об'єкт POINT, CIRCLE або INSERT (Радіус пошуку: " (rtos snap_radius 2 2) "): ")))
    (cond
      ((null start_ent) (*error* "Вибір скасовано."))
      ((listp start_ent)
       (setq ent_data (entget (car start_ent)))
       (setq initial_ent_type (cdr (assoc 0 ent_data)))
       (if (member initial_ent_type '("POINT" "CIRCLE" "INSERT"))
         (setq start_pt (cdr (assoc 10 ent_data)))
         (progn
            (princ (strcat "\nНевірний тип об'єкта: " initial_ent_type ". Виберіть POINT, CIRCLE або INSERT."))
            (setq initial_ent_type nil)
         )
       )
      )
      (t (princ "\nНевірний вибір.")(exit))
    )
  )
  (if (or (not start_pt) (not initial_ent_type))
      (*error* "Не вдалося визначити стартову точку або тип об'єкта.")
  )
  (setq vertices_list (list start_pt))

  ; --- Ініціалізація для циклу ---
  (setq loop_counter 0)
  (setq draw_color 30)
  (setq temp_pline_segment_color 1)
  (setq marker_size_ss (* (getvar "VIEWSIZE") 0.015))
  (setq highlighted_ents nil)
  (setq detection_counter 0)
  (setq detection_threshold 5)

  (princ "\n--- РЕЖИМ ВІДОБРАЖЕННЯ, ПОШУКУ ТА СТВОРЕННЯ ПОЛІЛІНІЇ (v8.8) ---") ; Оновлено версію
  (princ (strcat "\nШукаємо '" initial_ent_type "' в 2D радіусі " (rtos snap_radius) ". ЛКМ на підсвіченому для додавання вершини."))
  (princ "\nНатисніть Enter/ПКМ для завершення полілінії, Esc для скасування.")

  ; --- Інтерактивний цикл ---
  (while T
     (setq loop_counter (1+ loop_counter))
     (setq gr_data (grread T 2 0))
     (setq gr_status (car gr_data))
     (setq cursor_pt (cadr gr_data))

     (redraw) ; Очищає маркери та тимчасові сегменти

     ; Малювання тимчасових сегментів
     (if (> (length vertices_list) 1)
        (progn
          (setq i 0)
          (while (< i (- (length vertices_list) 1))
             (grdraw (nth i vertices_list) (nth (1+ i) vertices_list) temp_pline_segment_color -1)
             (setq i (1+ i))
          )
        )
     )

     ; Обробка подій
     (cond
       ; ЛКМ: Додати вершину
       ((= gr_status 3)
         (if highlighted_ents
           (progn
             (setq target_ename (car highlighted_ents))
             (if (and target_ename (= (type target_ename) 'ENAME) (setq edata (entget target_ename)))
               (progn
                 (setq target_pt (cdr (assoc 10 edata)))
                 (if target_pt
                   (progn
                     (princ (strcat "\nДодано вершину: " (vl-princ-to-string target_pt)))
                     (setq vertices_list (append vertices_list (list target_pt)))
                     (setq highlighted_ents (clear_highlights highlighted_ents))
                   )
                   (princ "\nПомилка: Координати.")
                 )
               )
               (princ "\nПомилка: Дані об'єкта.")
             )
           )
         )
       ) ; Кінець status 3

       ; Рух Миші: Оновити маркери та підсвічування
       ((and (= gr_status 5) cursor_pt)
         (progn
           (draw_radius_markers cursor_pt snap_radius draw_color marker_size_ss)
           (setq detection_counter (1+ detection_counter))
           (if (>= detection_counter detection_threshold)
             (progn
               (setq detection_counter 0)
               ; Блок Пошуку/Підсвічування (2D Відстань)
               (setq ll (list (- (car cursor_pt) snap_radius) (- (cadr cursor_pt) snap_radius)))
               (setq ur (list (+ (car cursor_pt) snap_radius) (+ (cadr cursor_pt) snap_radius)))
               (setq filter (list (cons 0 initial_ent_type)))
               (setq nearby_ss (ssget "_C" ll ur filter))
               (setq found_now_ents nil)
               (if nearby_ss
                 (progn
                   (setq i 0)
                   (repeat (sslength nearby_ss)
                     (setq ename (ssname nearby_ss i))
                     (if (setq edata (entget ename))
                       (progn
                         (setq obj_pt (cdr (assoc 10 edata)))
                         (if obj_pt
                           (progn
                              (setq cursor_pt_2d (list (car cursor_pt) (cadr cursor_pt)))
                              (setq obj_pt_2d    (list (car obj_pt)    (cadr obj_pt)))
                              (setq dist_2d (distance cursor_pt_2d obj_pt_2d))
                              (if (<= dist_2d snap_radius)
                                (setq found_now_ents (cons ename found_now_ents))
                              )
                            )
                         )
                       )
                     )
                     (setq i (1+ i))
                   ) ; кінець repeat
                 ) ; кінець progn if nearby_ss
               ) ; кінець if nearby_ss
               ; Керування підсвічуванням
               (setq highlighted_ents (clear_highlights highlighted_ents))
               (if found_now_ents
                  (progn
                     (foreach ent found_now_ents
                        (if (= (type ent) 'ENAME) (redraw ent 3))
                     )
                     (setq highlighted_ents found_now_ents)
                  )
               )
               ; --- Кінець блоку Пошуку/Підсвічування ---
             ) ; кінець progn для блоку, що виконується рідше
           ) ; кінець if для перевірки порогу лічильника
         ) ; кінець progn для обробки події руху миші
       ) ; Кінець status 5

       ; Клавіші Виходу: Створити полілінію (Enter/ПКМ) або скасувати (Esc)
       ((or (= gr_status 11) (= gr_status 25) (and (= gr_status 2) (= (cadr gr_data) 13))) ; Enter або ПКМ
          (if (> (length vertices_list) 1)
             (progn
                (princ (strcat "\nСтворюємо полілінію з " (itoa (length vertices_list)) " вершин..."))
                (setq pline_dxf (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                      (cons 90 (length vertices_list))
                                      '(70 . 0)
                                )
                )
                (foreach vertex vertices_list
                   (setq pline_dxf (append pline_dxf (list (cons 10 vertex))))
                )
                (entmake pline_dxf)
                (princ " Готово.")
             )
             (princ "\nНедостатньо вершин (потрібно мінімум 2) для створення полілінії.")
          )
          (princ "\nЗавершення.")
          (exit)
       )
       ((and (= gr_status 2) (= (cadr gr_data) 27)) ; Esc
          (*error* "Скасовано користувачем (Esc).")
          (exit)
       )

       ; Інші статуси ігноруються
       (t nil)

     ) ; кінець головного cond

  ) ; кінець while

  ; --- Очищення при нормальному виході ---
  (*error* nil)
  (princ)
) ; кінець c:AutoSnapPline

; --- === Додаткові Команди (Аліаси) === ---

; Короткий аліас ASPL
(defun c:ASPL ()
  (c:AutoSnapPline)
  (princ)
)

; Логічний аліас PCON (Polyline Connect / Point Connect)
(defun c:PCON ()
  (c:AutoSnapPline)
  (princ)
)

; Ще один аліас FASTLINE <<-- ДОДАНО
(defun c:FASTLINE ()
  (c:AutoSnapPline) ; Викликати основну функцію
  (princ)
)

; --- Повідомлення про завантаження (з переліком команд) ---
(princ "\nСкрипт AutoSnapPline (v8.8) завантажено.") ; <<-- ЗМІНЕНО версію
(princ "\nДоступні команди: AutoSnapPline, ASPL, PCON, FASTLINE") ; <<-- Додано FASTLINE
(princ)