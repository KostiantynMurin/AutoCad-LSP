;; ============================================================
;; == Скрипт для побудови осі стрілочного переводу (марка 1/9 або 1/11) ==
;; == Призначений для 2D-креслень (всі побудовані лінії та заливки на Z=0.0) ==
;; == Блоки P1-P5 переміщуються в 2D площині, зберігаючи оригінальну Z-координату. ==
;; == ========================================================== ==
;; == Основні можливості: ==
;; == - Автоматичне визначення марки стрілочного переводу (1/9 або 1/11) за довжиною відрізка P2-P4. ==
;; == - Побудова прямої осі (P1-P2_proj-P4) та осі відгалуження (ЦСП-P5_proj). ==
;; == - Переміщення вибраних блоків (P1-P5) на спроектовані 2D-позиції (збереження Z). ==
;; == - Додавання позначки Центру Стрілочного Переводу (ЦСП) як перпендикулярної лінії. ==
;; == - Створення замкнутого контуру з чорною заливкою від ЦСП. ==
;; == - Вставка динамічного блоку "Система_Керування_СП" у точку P2 з корекцією кута. ==
;; == - Переміщення всіх вихідних блоків на шар "22 ГЕОДЕЗИЧНА ОСНОВА". ==
;; == ========================================================== ==
;; == Додатково: ==
;; == - Розширене дебаг-логування у файл 'SwitchAxisDebug.txt' (у тимчасовій директорії). ==
;; == - Встановлення ваги ліній для осей та маркера ЦСП (0.30 мм). ==
;; ============================================================

(vl-load-com) ; Переконатися, що VLISP функції доступні

;; ============================================================
;; == ГЛОБАЛЬНІ НАЛАШТУВАННЯ СКРИПТА (Редагуйте ці значення) ==
;; ============================================================
;; -- Додаткова корекція кута повороту для динамічного блоку "Система_Керування_СП" --
;; (У градусах) - для точного вирівнювання, якщо базової перпендикулярності (90 градусів) недостатньо.
;; Якщо блок повертається неправильно, змініть це значення, щоб вирівняти його відносно лінії P1-P4.
;; Зазвичай це 0.0, 90.0, 180.0, 270.0 (градусів) або їх від'ємні значення,
;; залежно від того, як базовий блок "Система_Керування_СП" намальований.
;;
;; ФОРМУЛА КУТА ВСТАВКИ БЛОКУ = КУТ_ЛІНІЇ_P1-P4 + ЗНАЧЕННЯ_ЦІЄЇ_ЗМІННОЇ
;; Наприклад:
;; - Якщо ваш блок намальований горизонтально (по осі X) і ви хочете, щоб він лежав ВЗДОВЖ лінії P1-P4, залиште 0.0.
;; - Якщо ваш блок намальований вертикально (по осі Y) і ви хочете, щоб він був ПЕРПЕНДИКУЛЯРНИЙ лінії P1-P4, спробуйте 90.0 або -90.0.
(if (not *switch_block_rotation_offset_deg*)
    (setq *switch_block_rotation_offset_deg* 0.0) ; За замовчуванням: немає додаткової корекції кута
)
;; ============================================================

;; Глобальні змінні для збереження оригінальних налаштувань AutoCAD
(setq *oldEcho* nil)
(setq *oldOsmode* nil)
(setq *oldCmdDia* nil)
(setq *oldOsmodeZ* nil)

;; Локальна функція обробки помилок для відновлення налаштувань AutoCAD у разі збою або скасування.
(defun *error* (msg)
  (if *oldEcho* (setvar "CMDECHO" *oldEcho*))
  (if *oldOsmode* (setvar "OSMODE" *oldOsmode*))
  (if *oldCmdDia* (setvar "CMDDIA" *oldCmdDia*))
  (if *oldOsmodeZ* (setvar "OSNAPZ" *oldOsmodeZ*))
  (sssetfirst nil nil) ; Зняти будь-яке поточне виділення
  (princ "\n*** Помилка LISP або операцію скасовано: ")
  (if msg (princ msg)) ; Вивести повідомлення про помилку, якщо воно є
  (princ " ***")
  (princ)
)

;; --- Допоміжна функція для переміщення об'єкта на вказаний шар ---
(defun Helper:MoveEntityToLayer (ent layer_name / doc layers layer_obj vla_ent_obj)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        layers (vla-get-Layers doc)
  )
  ;; Перевіряємо, чи існує шар. Якщо ні, створюємо його автоматично.
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-Item (list layers layer_name)))
    (progn (setq layer_obj (vla-Add layers layer_name)) (princ (strcat "\nСтворено новий шар: '" layer_name "'.")))
  )
  ;; Переміщуємо об'єкт (переданий за іменем сутності 'ent') на вказаний шар.
  (if (and ent (setq vla_ent_obj (vlax-ename->vla-object ent)))
    (vla-put-Layer vla_ent_obj layer_name)
  )
  (princ)
)
;; --- КІНЕЦЬ ДОПОМІЖНОЇ ФУНКЦІЇ ---

;; Допоміжна функція для отримання одиничного вектора з заданого вектора.
;; Використовується для нормалізації напрямків.
(defun unit_vector (vec)
  (setq len (distance (apply 'list (mapcar (function (lambda (x) 0.0)) vec)) vec)) ; Довжина вектора
  (if (and (numberp len) (> len 0.000001)) ; Перевірка, щоб уникнути ділення на нуль
    (mapcar '/ vec (list len len (if (= (length vec) 3) len 1.0))) ; Ділимо кожну компоненту на довжину
    (apply 'list (mapcar (function (lambda (x) 0.0)) vec)) ; Повертаємо нульовий вектор, якщо довжина нульова
  )
)

;; Допоміжна функція для обчислення скалярного добутку двох векторів.
(defun dot_product (v1 v2)
  (apply '+ (mapcar '* v1 v2))
)

;; Допоміжна функція для обчислення векторного добутку (cross product) двох векторів.
;; Використовується для визначення напрямку (наліво/направо) у 2D за допомогою Z-компоненти.
(defun cross_product (v1 v2)
  (setq v1 (if (= (length v1) 2) (append v1 '(0.0)) v1)) ; Доповнюємо 2D вектори до 3D, якщо потрібно
  (setq v2 (if (= (length v2) 2) (append v2 '(0.0)) v2))
  (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
        (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
        (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
)

;; Допоміжні функції для перетворення кутів
(defun dtr (a) (* pi (/ a 180.0))) ; Перетворення градусів у радіани
(defun rtd (a) (* 180.0 (/ a pi))) ; Перетворення радіан у градуси

;; Допоміжна функція для вибору блоку користувачем та отримання його точки вставки (3D) та VLA-об'єкта.
;; Повертає список (координати_точки_вставки . VLA_об'єкт_блоку) або nil у разі скасування/помилки.
(defun GetBlockInsertionPointAndVLA (prompt_msg / ent_data ent_name ent_list ent_type insertion_pt vla_obj)
  (setq ent_data (entsel prompt_msg)) ; Запит вибору об'єкта у користувача
  (if ent_data
    (progn
      (setq ent_name (car ent_data)) ; Отримуємо ім'я сутності
      (setq vla_obj (vlax-ename->vla-object ent_name)) ; Отримуємо VLA-об'єкт
      (setq ent_list (entget ent_name)) ; Отримуємо список даних сутності (DXF-коди)
      (setq ent_type (cdr (assoc 0 ent_list))) ; Отримуємо тип об'єкта (DXF-код 0)

      (if (equal ent_type "INSERT") ; Перевіряємо, чи є обраний об'єкт блоком (INSERT)
        (progn
          (setq insertion_pt (cdr (assoc 10 ent_list))) ; Отримуємо 3D точку вставки (DXF-код 10)
          (princ (strcat "\nОбрано блок. Точка: " (vl-princ-to-string insertion_pt)))
          (cons insertion_pt vla_obj) ; Повертаємо пару: 3D координати . VLA-об'єкт
        )
        (progn
          (princ "\nПомилка: Вибраний об'єкт не є блоком. Спробуйте ще раз.")
          nil
        )
      )
    )
    (progn
      (princ "\nВідміна вибору. Операцію скасовано.")
      nil
    )
  )
)

;; ============================================================
;; == ОСНОВНА ФУНКЦІЯ СКРИПТА: DrawSwitchAxisPro ==
;; ============================================================
(defun c:DrawSwitchAxisPro ( / p1_data p2_data p4_data p3_data p5_data
                                 p1_orig_coords p2_orig_coords p4_orig_coords p3_orig_coords p5_orig_coords
                                 p1_2d_coords p2_2d_coords p4_2d_coords p3_2d_coords p5_2d_coords
                                 straight_axis_obj
                                 proj_pt_p3 proj_pt_p2 csp_pt branch_angle branch_end_pt
                                 branch_axis_obj p2_projected_on_straight p5_projected_on_branch
                                 p1_block_vla p2_block_vla p3_block_vla p4_block_vla p5_block_vla ; VLA-об'єкти для всіх блоків
                                 final_p2_target_pt final_p5_target_pt
                                 temp_straight_axis_ent temp_branch_axis_ent
                                 mark_1_9_angle_deg mark_1_11_angle_deg
                                 mark_1_9_dist_to_csp mark_1_11_dist_to_csp
                                 determined_mark
                                 dist_to_csp branch_angle_deg
                                 debug_file debug_file_path
                                 etalon_p2_p4_1_9 etalon_p2_p4_1_11 actual_p2_p4_length
                                 csp_marker_length csp_marker_lineweight
                                 straight_axis_main_angle perp_angle csp_marker_pt1 csp_marker_pt2 csp_marker_obj
                                 contour_length_along_straight contour_pt2 contour_pt3 contour_polyline_ent contour_polyline_obj hatch_ent hatch_obj
                                 block_name acad_doc model_space block_def_found block_ref_obj block_ref_ent 
                                 base_p1_p4_angle_rad final_insertion_angle_deg 
                                 temp_pt1 temp_pt2 ) ; Тимчасові змінні для обчислення точок маркера ЦСП


  ;; Зберегти поточні налаштування AutoCAD перед змінами
  (setq *oldEcho* (getvar "CMDECHO"))
  (setq *oldOsmode* (getvar "OSMODE"))
  (setq *oldCmdDia* (getvar "CMDDIA"))
  (setq *oldOsmodeZ* (getvar "OSNAPZ"))
  
  ;; Встановити оптимальні налаштування для роботи скрипта
  (setvar "CMDECHO" 0) ; Вимикаємо ехо команд для чистішого виконання
  (setvar "CMDDIA" 0) ; Вимикаємо діалоги, щоб скрипт не зупинявся на запитах
  (setvar "OSNAPZ" 0) ; Тимчасово встановлюємо OSNAPZ в 0 для коректної роботи MOVE з 3D точками
                       ; і щоб нові 2D об'єкти створювалися на Z=0.0

  (princ "\n--- Побудова осі стрілочного переводу (Автоматичне визначення марки) ---")

  ;; --- Ініціалізація та налаштування файлу дебаг-логу ---
  (setq debug_file_path (strcat (getenv "TEMP") "\\SwitchAxisDebug.txt"))
  (setq debug_file (open debug_file_path "w")) ; Відкриття файлу для запису (перезаписує, якщо існує)
  (if debug_file
      (princ (strcat "\nДебаг-лог буде збережено у файлі: " debug_file_path))
      (princ "\nПОМИЛКА: Не вдалося відкрити файл дебаг-логу для запису. Перевірте шлях/дозволи. Спроба запису у тимчасову директорію.")
  )
  (if debug_file
      (progn
          (write-line (strcat "--- DEBUG LOG --- " (rtos (getvar "CDATE") 2 8)) debug_file)
          (write-line "" debug_file)
      )
  )
  ;; --- Кінець ініціалізації дебаг-логу ---


  ;; 1. Запит блоків у користувача та отримання їхніх оригінальних 3D координат і VLA-об'єктів
  ;; Ці блоки будуть переміщені пізніше, але їхні оригінальні Z-координати будуть збережені.
  (setq p1_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки стику рамної рейки (P1): "))
  (if (not p1_data) (progn (*error* "Вибір P1 скасовано.") (exit)))
  (setq p1_orig_coords (car p1_data))
  (setq p1_block_vla (cdr p1_data)) ; Зберігаємо VLA-об'єкт блоку P1

  (setq p2_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки початку вістря (P2): "))
  (if (not p2_data) (progn (*error* "Вибір P2 скасовано.") (exit)))
  (setq p2_orig_coords (car p2_data))
  (setq p2_block_vla (cdr p2_data))

  (setq p4_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по прямому напрямку (P4): "))
  (if (not p4_data) (progn (*error* "Вибір P4 скасовано.") (exit)))
  (setq p4_orig_coords (car p4_data))
  (setq p4_block_vla (cdr p4_data))

  (setq p3_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки центру хрестовини (P3): "))
  (if (not p3_data) (progn (*error* "Вибір P3 скасовано.") (exit)))
  (setq p3_orig_coords (car p3_data))
  (setq p3_block_vla (cdr p3_data))

  (setq p5_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по відгалуженню (P5): "))
  (if (not p5_data) (progn (*error* "Вибір P5 скасовано.") (exit)))
  (setq p5_orig_coords (car p5_data))
  (setq p5_block_vla (cdr p5_data))

  ;; === Перетворюємо всі оригінальні 3D-координати блоків на 2D-координати (Z=0.0) ===
  ;; Ці 2D-координати будуть використовуватися для всіх подальших геометричних розрахунків,
  ;; щоб забезпечити коректну побудову об'єктів у площині XY на Z=0.0.
  (setq p1_2d_coords (list (car p1_orig_coords) (cadr p1_orig_coords) 0.0))
  (setq p2_2d_coords (list (car p2_orig_coords) (cadr p2_orig_coords) 0.0))
  (setq p3_2d_coords (list (car p3_orig_coords) (cadr p3_orig_coords) 0.0))
  (setq p4_2d_coords (list (car p4_orig_coords) (cadr p4_orig_coords) 0.0))
  (setq p5_2d_coords (list (car p5_orig_coords) (cadr p5_orig_coords) 0.0))
  
  (princ (strcat "\nDBG: P3_2D: " (vl-princ-to-string p3_2d_coords)))
  (princ (strcat "\nDBG: P4_2D: " (vl-princ-to-string p4_2d_coords)))
  (princ (strcat "\nDBG: P5_2D: " (vl-princ-to-string p5_2d_coords)))

  ;; --- Детальний лог оригінальних 3D та 2D координат у дебаг-файл ---
  (if debug_file
      (progn
          (write-line "--- Original 3D Coords (from selected blocks) ---" debug_file)
          (write-line (strcat "P1_orig_coords: " (rtos (car p1_orig_coords) 2 15) ", " (rtos (cadr p1_orig_coords) 2 15) ", " (rtos (caddr p1_orig_coords) 2 15)) debug_file)
          (write-line (strcat "P2_orig_coords: " (rtos (car p2_orig_coords) 2 15) ", " (rtos (cadr p2_orig_coords) 2 15) ", " (rtos (caddr p2_orig_coords) 2 15)) debug_file)
          (write-line (strcat "P3_orig_coords: " (rtos (car p3_orig_coords) 2 15) ", " (rtos (cadr p3_orig_coords) 2 15) ", " (rtos (caddr p3_orig_coords) 2 15)) debug_file)
          (write-line (strcat "P4_orig_coords: " (rtos (car p4_orig_coords) 2 15) ", " (rtos (cadr p4_orig_coords) 2 15) ", " (rtos (caddr p4_orig_coords) 2 15)) debug_file)
          (write-line (strcat "P5_orig_coords: " (rtos (car p5_orig_coords) 2 15) ", " (rtos (cadr p5_orig_coords) 2 15) ", " (rtos (caddr p5_orig_coords) 2 15)) debug_file)
          (write-line "" debug_file)
          (write-line "--- 2D Coords (Z=0) for calculations ---" debug_file)
          (write-line (strcat "P1_2d_coords: " (rtos (car p1_2d_coords) 2 15) ", " (rtos (cadr p1_2d_coords) 2 15) ", " (rtos (caddr p1_2d_coords) 2 15)) debug_file)
          (write-line (strcat "P2_2d_coords: " (rtos (car p2_2d_coords) 2 15) ", " (rtos (cadr p2_2d_coords) 2 15) ", " (rtos (caddr p2_2d_coords) 2 15)) debug_file)
          (write-line (strcat "P3_2d_coords: " (rtos (car p3_2d_coords) 2 15) ", " (rtos (cadr p3_2d_coords) 2 15) ", " (rtos (caddr p3_2d_coords) 2 15)) debug_file)
          (write-line (strcat "P4_2d_coords: " (rtos (car p4_2d_coords) 2 15) ", " (rtos (cadr p4_2d_coords) 2 15) ", " (rtos (caddr p4_2d_coords) 2 15)) debug_file)
          (write-line (strcat "P5_2d_coords: " (rtos (car p5_2d_coords) 2 15) ", " (rtos (cadr p5_2d_coords) 2 15) ", " (rtos (caddr p5_2d_coords) 2 15)) debug_file)
          (write-line "" debug_file)
      )
  )
  ;; --- Кінець логування координат ---


  ;; ===================================================================
  ;; == АВТОМАТИЧНЕ ВИЗНАЧЕННЯ МАРКИ ХРЕСТОВИНИ (ЗА ДОВЖИНОЮ ВІДРІЗКА P2-P4) ==
  ;; ===================================================================
  (princ "\nВизначення марки хрестовини за еталонною довжиною P2-P4...")

  ;; Еталонні довжини P2-P4 (у метрах) для визначення марки стрілочного переводу.
  (setq etalon_p2_p4_1_9 28.270)
  (setq etalon_p2_p4_1_11 30.598)

  ;; Еталонні кути відгалуження (у градусах) для кожної марки.
  (setq mark_1_9_angle_deg 6.340277778)   ; Кут для марки 1/9 (6°20'25")
  (setq mark_1_11_angle_deg 5.194444444) ; Кут для марки 1/11 (5°11'40")

  ;; Еталонні відстані до Центру Стрілочного Переводу (ЦСП) від P3 (у метрах).
  (setq mark_1_9_dist_to_csp 13.68)
  (setq mark_1_11_dist_to_csp 16.72)

  ;; Обчислюємо фактичну довжину відрізка P2-P4 у 2D-площині.
  (setq actual_p2_p4_length (distance p2_2d_coords p4_2d_coords))
  (princ (strcat "\nФактична довжина P2-P4: " (rtos actual_p2_p4_length 2 8) " м"))

  ;; Визначаємо, яка еталонна довжина ближча до фактичної.
  (setq diff_to_1_9 (abs (- actual_p2_p4_length etalon_p2_p4_1_9)))
  (setq diff_to_1_11 (abs (- actual_p2_p4_length etalon_p2_p4_1_11)))

  (if (< diff_to_1_9 diff_to_1_11)
    (setq determined_mark "1/9")
    (setq determined_mark "1/11") ; За замовчуванням, якщо 11 ближче або однаково
  )
  (princ (strcat "\nАвтоматично визначена марка хрестовини: " determined_mark))

  ;; Встановлюємо параметри (відстань до ЦСП та кут відгалуження) відповідно до визначеної марки.
  (cond
    ((equal determined_mark "1/9")
     (setq dist_to_csp mark_1_9_dist_to_csp
           branch_angle_deg mark_1_9_angle_deg)
    )
    ((equal determined_mark "1/11")
     (setq dist_to_csp mark_1_11_dist_to_csp
           branch_angle_deg mark_1_11_angle_deg)
    )
  )

  ;; --- Детальний лог визначення марки у дебаг-файл ---
  (if debug_file
      (progn
          (write-line "--- Mark Determination Details (Length-based) ---" debug_file)
          (write-line (strcat "Actual P2-P4 Length: " (rtos actual_p2_p4_length 2 15) " m") debug_file)
          (write-line (strcat "Etalon 1/9 P2-P4 Length: " (rtos etalon_p2_p4_1_9 2 15) " m (Diff: " (rtos diff_to_1_9 2 15) ")") debug_file)
          (write-line (strcat "Etalon 1/11 P2-P4 Length: " (rtos etalon_p2_p4_1_11 2 15) " m (Diff: " (rtos diff_to_1_11 2 15) ")") debug_file)
          (write-line (strcat "Determined Mark: " determined_mark) debug_file)
          (write-line "" debug_file)
      )
  )
  ;; --- Кінець логування визначення марки ---


  ;; === Побудова тимчасової прямої полілінії P1-P4 для проекційних розрахунків ===
  ;; Ця тимчасова полілінія буде видалена після використання.
  (command "_.PLINE" p1_2d_coords p4_2d_coords "")
  (setq straight_axis_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт щойно створеної полілінії
  (setq temp_straight_axis_ent (entlast)) ; Зберігаємо ім'я сутності для подальшого видалення


  ;; === Розрахунок проекції P3 на пряму вісь P1-P4 (для визначення точки відліку для ЦСП) ===
  (setq proj_pt_p3 (vlax-curve-getClosestPointTo straight_axis_obj p3_2d_coords))
  (setq proj_pt_p3 (list (car proj_pt_p3) (cadr proj_pt_p3) 0.0)) ; Забезпечуємо Z=0.0 для спроектованої точки


  ;; === Обчислюємо остаточну точку Центру Стрілочного Переводу (ЦСП) ===
  ;; Використовуємо визначену марку та відповідну відстань 'dist_to_csp'.
  ;; ЦСП розташовується на Z=0.0.
  (setq vec_p1_proj_final (mapcar '- p1_2d_coords proj_pt_p3)) ; Вектор від P3_proj до P1
  (setq vec_p1_proj_unit_final (unit_vector vec_p1_proj_final)) ; Одиничний вектор
  (setq csp_pt (mapcar '+ proj_pt_p3 (mapcar '* vec_p1_proj_unit_final (list dist_to_csp dist_to_csp 0.0)))) ; Обчислення ЦСП
  
  ;; --- Детальний лог обчислення ЦСП у дебаг-файл ---
  (if debug_file
      (progn
          (write-line "--- CSP Calculation Details ---" debug_file)
          (write-line (strcat "proj_pt_p3: " (rtos (car proj_pt_p3) 2 15) ", " (rtos (cadr proj_pt_p3) 2 15) ", " (rtos (caddr proj_pt_p3) 2 15)) debug_file)
          (write-line (strcat "vec_p1_proj_unit_final: " (rtos (car vec_p1_proj_unit_final) 2 15) ", " (rtos (cadr vec_p1_proj_unit_final) 2 15) ", " (rtos (caddr vec_p1_proj_unit_final) 2 15)) debug_file)
          (write-line (strcat "Desired dist_to_csp (used in calc): " (rtos dist_to_csp 2 15)) debug_file)
          (write-line (strcat "csp_pt (calculated): " (rtos (car csp_pt) 2 15) ", " (rtos (cadr csp_pt) 2 15) ", " (rtos (caddr csp_pt) 2 15)) debug_file)
          (write-line (strcat "ACTUAL MEASURED DISTANCE from proj_pt_p3 to csp_pt (by script): " (rtos (distance proj_pt_p3 csp_pt) 2 15)) debug_file)
          (write-line "" debug_file)
      )
  )
  ;; --- Кінець логування ЦСП ---

  ;; === Проектування P2 на пряму вісь P1-P4 ===
  (setq proj_pt_p2 (vlax-curve-getClosestPointTo straight_axis_obj p2_2d_coords))
  (setq proj_pt_p2 (list (car proj_pt_p2) (cadr proj_pt_p2) 0.0)) ; Забезпечуємо Z=0.0 для спроектованої точки


  ;; --- Переміщення блоку P2 на спроектовану 2D точку, ЗБЕРІГАЮЧИ його оригінальну Z-координату ---
  (if p2_block_vla
    (progn
      (setq final_p2_target_pt (list (car proj_pt_p2)
                                     (cadr proj_pt_p2)
                                     (caddr p2_orig_coords))) ; Зберігаємо оригінальну Z для блоку
      
      (command "_move" (vlax-vla-object->ename p2_block_vla) "" 
               "_none" p2_orig_coords ; Базова точка переміщення (оригінальна позиція)
               "_none" final_p2_target_pt) ; Цільова точка переміщення
      (princ "\nБлок P2 переміщено на спроектовану точку, ЗБЕРІГАЮЧИ оригінальну Z-координату.")
    )
    (princ "\nПомилка: Не вдалося перемістити блок P2, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини ПРЯМОЇ осі: створюємо нову полілінію P1 - P2_proj - P4 ---
  ;; Спочатку видаляємо тимчасову пряму вісь.
  (if temp_straight_axis_ent
    (progn
      (vla-delete (vlax-ename->vla-object temp_straight_axis_ent)) ; Видаляємо тимчасову P1-P4
      (setq straight_axis_obj nil) ; Очищаємо змінну VLA-об'єкта
      (princ "\nТимчасова пряма вісь видалена (для перетворення в P1-P2_proj-P4).")
      
      ;; Створюємо нову пряму полілінію P1 - P2_proj - P4. Всі точки полілінії будуть на Z=0.0.
      (setq p2_proj_for_pline (list (car proj_pt_p2)
                                    (cadr proj_pt_p2)
                                    0.0)) ; Забезпечуємо Z=0 для точки P2_proj для полілінії
      
      (command "_.PLINE" p1_2d_coords p2_proj_for_pline p4_2d_coords "")
      (setq straight_axis_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт нової полілінії
      (vla-put-Lineweight straight_axis_obj 30) ; Встановлюємо вагу лінії (0.30 мм)
      (princ "\nНова пряма вісь (P1-P2_proj-P4) створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити пряму вісь.")
  )

  ;; 5. Визначення напрямку відгалуження (вліво/вправо)
  ;; Це робиться за допомогою векторного добутку для 2D-координат.
  (setq vec_line (mapcar '- p4_2d_coords p1_2d_coords)) ; Вектор лінії P1-P4
  (setq vec_test (mapcar '- p5_2d_coords p1_2d_coords)) ; Вектор від P1 до P5
  (setq cross_z (caddr (cross_product vec_line vec_test))) ; Z-компонента векторного добутку
  (setq is_left (if (> cross_z 0) T nil)) ; Визначаємо напрямок: True, якщо вліво (>0), False, якщо вправо (<0)

  ;; 6. Побудова осі відгалуження від ЦСП (початкова, тимчасова)
  (setq branch_length 20.0) ; Початкова довжина для тимчасової полілінії відгалуження
  ;; Змінна branch_angle_deg вже визначена вище, відповідно до марки.
  (setq branch_angle_rad (dtr branch_angle_deg)) ; Перетворюємо кут відгалуження в радіани
  (setq straight_line_angle (angle p1_2d_coords p4_2d_coords)) ; Кут прямої осі P1-P4 (в радіанах)

  ;; Обчислюємо кінцевий кут для осі відгалуження залежно від напрямку (вліво/вправо).
  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad))
    (setq final_branch_angle (- straight_line_angle branch_angle_rad))
  )

  ;; Створюємо тимчасову полілінію для осі відгалуження (від ЦСП до тимчасової кінцевої точки).
  (setq temp_branch_end_pt (polar csp_pt final_branch_angle branch_length))
  (command "_.PLINE" csp_pt temp_branch_end_pt "")
  (setq temp_branch_axis_ent (entlast)) ; Зберігаємо ім'я сутності для подальшого видалення

  ;; --- Проектування P5 на вісь відгалуження (в 2D) ---
  (setq branch_axis_obj (vlax-ename->vla-object temp_branch_axis_ent))
  (setq p5_projected_on_branch (vlax-curve-getClosestPointTo branch_axis_obj p5_2d_coords))
  (setq p5_projected_on_branch (list (car p5_projected_on_branch) (cadr p5_projected_on_branch) 0.0)) ; Забезпечуємо Z=0.0 для спроектованої P5


  ;; --- Переміщення блоку P5 на спроектовану 2D точку, ЗБЕРІГАЮЧИ його оригінальну Z-координату ---
  (if p5_block_vla
    (progn
      (setq final_p5_target_pt (list (car p5_projected_on_branch)
                                     (cadr p5_projected_on_branch)
                                     (caddr p5_orig_coords))) ; Зберігаємо оригінальну Z для блоку
      
      (command "_move" (vlax-vla-object->ename p5_block_vla) "" 
               "_none" p5_orig_coords
               "_none" final_p5_target_pt)
      (princ "\nБлок P5 переміщено на спроектовану точку, ЗБЕРІГАЮЧИ оригінальну Z-координату.")
    )
    (princ "\nПомилка: Не вдалося перемістити блок P5, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини осі відгалуження: створюємо нову полілінію від ЦСП до спроектованої P5 ---
  ;; Спочатку видаляємо тимчасову вісь відгалуження.
  (if (and temp_branch_axis_ent branch_axis_obj)
    (progn
      (vla-delete branch_axis_obj) ; Видаляємо тимчасову вісь
      (setq branch_axis_obj nil) ; Очищаємо змінну VLA-об'єкта
      (princ "\nТимчасова вісь відгалуження видалена.")

      ;; Створюємо нову, коректну полілінію від ЦСП до спроектованої P5. Всі точки будуть на Z=0.0.
      (setq p5_proj_for_pline_branch (list (car p5_projected_on_branch)
                                            (cadr p5_projected_on_branch)
                                            0.0)) ; Забезпечуємо Z=0 для спроектованої P5 для полілінії
      
      (command "_.PLINE" csp_pt p5_proj_for_pline_branch "")
      (setq branch_axis_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт нової полілінії
      (vla-put-Lineweight branch_axis_obj 30) ; Встановлюємо вагу лінії (0.30 мм)
      (princ "\nНова вісь відгалуження від ЦСП до спроектованої P5 створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити вісь відгалуження.")
  )

  ;; --- Накреслити маркер Центру Стрілочного Переводу (ЦСП) ---
  ;; Маркер буде представлений короткою лінією, перпендикулярною прямій осі,
  ;; центрованою на точці ЦСП. Всі точки маркера будуть на Z=0.0.
  (setq csp_marker_length 0.76) ; Довжина маркера ЦСП (у метрах)
  (setq csp_marker_lineweight 30) ; Вага лінії маркера (0.30 мм)
  
  ;; Обчислюємо кут прямої осі (P1_2d до P4_2d) для визначення напрямку перпендикуляра.
  (setq straight_axis_main_angle (angle p1_2d_coords p4_2d_coords)) 
  (setq perp_angle (+ straight_axis_main_angle (/ pi 2.0))) ; Кут, перпендикулярний прямій осі (+90 градусів)

  ;; Розраховуємо дві кінцеві точки маркера ЦСП, центруючи його на `csp_pt`.
  (setq temp_pt1 (polar csp_pt perp_angle (/ csp_marker_length 2.0)))
  (setq temp_pt2 (polar csp_pt (+ perp_angle pi) (/ csp_marker_length 2.0))) ; Точка в протилежному напрямку

  ;; Явно встановлюємо Z-координату для кінцевих точок маркера на 0.0,
  ;; щоб забезпечити його створення на площині XY.
  (setq csp_marker_pt1 (list (car temp_pt1) (cadr temp_pt1) 0.0)) 
  (setq csp_marker_pt2 (list (car temp_pt2) (cadr temp_pt2) 0.0))

  (command "_.PLINE" csp_marker_pt1 csp_marker_pt2 "") ; Створюємо полілінію маркера
  (setq csp_marker_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт створеного маркера
  (if csp_marker_obj
      (progn
          (vla-put-Lineweight csp_marker_obj csp_marker_lineweight) ; Встановлюємо вагу лінії
          (vla-put-color csp_marker_obj 256) ; Встановлюємо колір на ByLayer (використовує колір поточного шару)
          (princ (strcat "\nDBG: Маркер ЦСП (EntName: " (vl-princ-to-string (vlax-vla-object->ename csp_marker_obj)) ") створено."))
      )
      (princ "\nПОМИЛКА: Маркер ЦСП не вдалося створити (можливо, точки збігаються або PLINE не спрацював).")
  )
  (princ "\nМаркер ЦСП накреслено.") ; Повідомлення про успішне накреслення

  ;; --- Створення замкнутого контуру з чорною заливкою від ЦСП ---
  ;; Цей контур буде представляти область, залиту чорним кольором.
  (princ "\nСтворення замкнутого контуру та заливка чорним...")

  ;; 1. Розраховуємо точки контуру. Всі точки контуру будуть на Z=0.0.
  (setq contour_length_along_straight 15.0) ; Довжина ділянки контуру вздовж прямої осі від ЦСП
  
  ;; Точка 1: ЦСП (csp_pt) - вже визначена на Z=0.0.
  ;; Точка 2: 15 метрів від ЦСП в напрямку P4, на Z=0.0.
  (setq contour_pt2 (polar csp_pt (angle csp_pt p4_2d_coords) contour_length_along_straight))
  (setq contour_pt2 (list (car contour_pt2) (cadr contour_pt2) 0.0)) ; Забезпечуємо Z=0.0

  ;; Точка 3: На лінії ЦСП-P5 (осі відгалуження), проекція від Точки 2, на Z=0.0.
  ;; Використовуємо 'branch_axis_obj', що є полілінією від ЦСП до p5_projected_on_branch.
  (setq contour_pt3 (vlax-curve-getClosestPointTo branch_axis_obj contour_pt2)) 
  (setq contour_pt3 (list (car contour_pt3) (cadr contour_pt3) 0.0)) ; Забезпечуємо Z=0.0

  ;; 2. Створюємо замкнуту полілінію, використовуючи обчислені точки.
  (command "_.PLINE" csp_pt contour_pt2 contour_pt3 "_C") ; "_C" замикає полілінію
  (setq contour_polyline_ent (entlast)) ; Отримуємо ім'я щойно створеної полілінії
  (setq contour_polyline_obj (vlax-ename->vla-object contour_polyline_ent)) ; Отримуємо VLA-об'єкт

  ;; 3. Заливаємо створений контур чорним кольором.
  (if contour_polyline_obj
      (progn
          (command "_.HATCH" "SOLID" contour_polyline_ent "") ; Створюємо штриховку "SOLID" (суцільна заливка)
          (setq hatch_ent (entlast)) ; Отримуємо ім'я створеної штриховки
          (setq hatch_obj (vlax-ename->vla-object hatch_ent)) ; Отримуємо VLA-об'єкт штриховки

          (if hatch_obj
              (progn
                  (vla-put-Color hatch_obj 7) ; Встановлюємо колір на чорний (ACLISP index 7)
                  (princ "\nЗамкнутий контур залито чорним.")
              )
              (princ "\nПОМИЛКА: Не вдалося створити об'єкт заливки (HATCH).")
          )
      )
      (princ "\nПОМИЛКА: Не вдалося створити контурну полілінію для заливки.")
  )

  ;; 4. Видаляємо допоміжну контурну полілінію (залишаємо тільки заливку).
  (if (and contour_polyline_obj (not (vlax-object-released-p contour_polyline_obj)))
      (progn
          (vla-delete contour_polyline_obj)
          (princ "\nКонтурні лінії видалено, залишено лише заливку.")
      )
      (princ "\nУвага: Контурну полілінію не знайдено для видалення або вона вже була звільнена.")
  )

  ;; === Вставка динамічного блоку "Система_Керування_СП" ===
  ;; Блок буде вставлено у скориговану точку P2 з розрахованим кутом повороту.
  (princ "\n--- Вставка позначення Система_Керування_СП ---")
  (setq block_name "Система_Керування_СП")
  
  ;; Отримання посилання на простір моделі активного документа.
  (setq acad_doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq model_space (vla-get-ModelSpace acad_doc))

  ;; 1. Перевірка наявності визначення блоку у поточному кресленні.
  (setq block_def_found nil)
  (vl-catch-all-apply
    (function (lambda ()
                (setq block_def_found (vla-item (vla-get-Blocks acad_doc) block_name))
              ))
  )
  
  (if block_def_found
      (progn
          (princ (strcat "\nБлок '" block_name "' знайдено в кресленні."))

          ;; 2. Розрахунок кута для вставки блоку (напрямок лінії P1-P4).
          (setq base_p1_p4_angle_rad (angle p1_2d_coords p4_2d_coords)) 
          
          ;; Кут вставки блоку = базовий кут лінії P1-P4 (у градусах) + додаткова корекція.
          (setq final_insertion_angle_deg (+ (rtd base_p1_p4_angle_rad) *switch_block_rotation_offset_deg*))

          ;; --- Лог кута вставки у дебаг-файл ---
          (princ (strcat "\nDBG: Базовий кут P1-P4 (в рад): " (rtos base_p1_p4_angle_rad 2 8)))
          (princ (strcat "\nDBG: Базовий кут P1-P4 (в град): " (rtos (rtd base_p1_p4_angle_rad) 2 8)))
          (princ (strcat "\nDBG: Встановлена корекція (град): " (rtos *switch_block_rotation_offset_deg* 2 8)))
          (princ (strcat "\nDBG: Фінальний кут вставки блоку (в град): " (rtos final_insertion_angle_deg 2 8)))
          (if debug_file
              (progn
                  (write-line (strcat "Insertion Angle Details:" ) debug_file)
                  (write-line (strcat "  P1_2d: " (rtos (car p1_2d_coords) 2 15) ", " (rtos (cadr p1_2d_coords) 2 15)) debug_file)
                  (write-line (strcat "  P4_2d: " (rtos (car p4_2d_coords) 2 15) ", " (rtos (cadr p4_2d_coords) 2 15)) debug_file)
                  (write-line (strcat "  Base P1-P4 Angle (rad): " (rtos base_p1_p4_angle_rad 2 15)) debug_file)
                  (write-line (strcat "  Base P1-P4 Angle (deg): " (rtos (rtd base_p1_p4_angle_rad) 2 15)) debug_file)
                  (write-line (strcat "  Rotation Offset (deg): " (rtos *switch_block_rotation_offset_deg* 2 15)) debug_file)
                  (write-line (strcat "  Final Insertion Angle (deg): " (rtos final_insertion_angle_deg 2 15)) debug_file)
                  (write-line "" debug_file)
              )
          )
          ;; --- Кінець логування кута ---

          ;; 3. Вставка динамічного блоку за допомогою COMMAND.
          ;; Вставлено у 'final_p2_target_pt', яка зберігає оригінальну Z-координату блоку P2.
          (command "_.INSERT"
                   block_name
                   final_p2_target_pt ; Точка вставки блоку P2
                   1.0 ; Масштаб X (1.0 = без зміни)
                   1.0 ; Масштаб Y (1.0 = без зміни)
                   final_insertion_angle_deg) ; Розрахований кут повороту
          
          ;; Спроба отримати VLA-об'єкт щойно вставленого блоку, використовуючи (entlast).
          (setq block_ref_ent (entlast))
          (setq block_ref_obj (vlax-ename->vla-object block_ref_ent))

          (if (and block_ref_obj (equal (vla-get-objectname block_ref_obj) "AcDbBlockReference")) ; Перевірка, чи entlast повернув блок
              (princ (strcat "\nДинамічний блок '" block_name "' вставлено в точку P2."))
              (princ "\nПОМИЛКА: Не вдалося отримати посилання на щойно вставлений блок (entlast не вказав на блок).")
          )
      )
      (princ (strcat "\nПОМИЛКА: Блок '" block_name "' не знайдено в кресленні. Будь ласка, переконайтеся, що визначення блоку існує."))
  )

  ;; === Переміщення оброблених блоків P1-P5 на цільовий шар ===
  (princ "\nПереміщення вихідних блоків на шар '22 ГЕОДЕЗИЧНА ОСНОВА'...")
  (Helper:MoveEntityToLayer (vlax-vla-object->ename p1_block_vla) "22 ГЕОДЕЗИЧНА ОСНОВА")
  (Helper:MoveEntityToLayer (vlax-vla-object->ename p2_block_vla) "22 ГЕОДЕЗИЧНА ОСНОВА")
  (Helper:MoveEntityToLayer (vlax-vla-object->ename p3_block_vla) "22 ГЕОДЕЗИЧНА ОСНОВА")
  (Helper:MoveEntityToLayer (vlax-vla-object->ename p4_block_vla) "22 ГЕОДЕЗИЧНА ОСНОВА")
  (Helper:MoveEntityToLayer (vlax-vla-object->ename p5_block_vla) "22 ГЕОДЕЗИЧНА ОСНОВА")
  (princ " Завершено.")
  ;; ================================================================

  (princ (strcat "\n--- Осі стрілочного переводу марки " determined_mark " побудовано, блоки переміщено та осі скориговано! ---"))
  (princ "\nСкрипт завершено. Відновлення вихідних налаштувань AutoCAD.")
  (princ)

  ;; Закриваємо дебаг-файл, якщо він був відкритий.
  (if debug_file (close debug_file))

  ;; Очищаємо глобальні змінні для уникнення конфліктів у майбутніх запусках.
  (setq *oldEcho* nil)
  (setq *oldOsmode* nil)
  (setq *oldCmdDia* nil)
  (setq *oldOsmodeZ* nil)

)

(princ "\nLISP 'DrawSwitchAxisPro' успішно завантажено.")
(princ "\nДля запуску введіть 'DrawSwitchAxisPro' у командний рядок AutoCAD.")
(princ)