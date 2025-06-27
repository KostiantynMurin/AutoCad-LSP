;; ============================================================
;; == Скрипт для побудови осі стрілочного переводу 1/11 за геодезичними точками ==
;; == Призначення:
;; ==   1. Користувач обирає 5 блоків-точок геодезичної зйомки:
;; ==      - P1: Стик рамної рейки
;; ==      - P2: Початок вістря (НОВЕ)
;; ==      - P4: Хвіст хрестовини по прямому напрямку
;; ==      - P3: Центр хрестовини
;; ==      - P5: Хвіст хрестовини по відгалуженню
;; ==   2. Будує пряму полілінію (P1-P2_proj-P4).
;; ==   3. Визначає проекцію P3 на пряму вісь.
;; ==   4. Розраховує Центр Стрілочного Переводу (ЦСП) на прямій осі.
;; ==   5. Будує полілінію відгалуження від ЦСП з заданим кутом.
;; ==   6. Проектує оригінальний блок P2 на пряму вісь та переміщує його, зберігаючи Z.
;; ==   7. Обрізає/продовжує пряму вісь, додаючи спроектовану точку P2 як вершину.
;; ==   8. Проектує оригінальний блок P5 на створену вісь відгалуження та переміщує його, зберігаючи Z.
;; ==   9. Обрізає/продовжує вісь відгалуження до спроектованої точки P5.
;; ==
;; == Виклик: DrawSwitchAxisPro
;; ============================================================

(vl-load-com) ; Переконатися, що VLISP функції доступні

;; Глобальні змінні для збереження оригінальних налаштувань AutoCAD
(setq *oldEcho* nil)
(setq *oldOsmode* nil)
(setq *oldCmdDia* nil)
(setq *oldOsmodeZ* nil)

;; Локальна функція обробки помилок для відновлення налаштувань
(defun *error* (msg)
  (if *oldEcho* (setvar "CMDECHO" *oldEcho*))
  (if *oldOsmode* (setvar "OSMODE" *oldOsmode*))
  (if *oldCmdDia* (setvar "CMDDIA" *oldCmdDia*))
  (if *oldOsmodeZ* (setvar "OSNAPZ" *oldOsmodeZ*))
  (sssetfirst nil nil) ; Зняти виділення при помилці або скасуванні
  (princ "\n*** Помилка LISP або скасовано: ")
  (if msg (princ msg))
  (princ " ***")
  (princ)
)

;; Допоміжна функція для отримання одиничного вектора
(defun unit_vector (vec)
  (setq len (distance '(0 0 0) vec))
  (if (and (numberp len) (> len 0.00000001))
    (mapcar '/ vec (list len len (if (caddr vec) len 1.0)))
    '(0.0 0.0 0.0)
  )
)

;; Допоміжна функція для векторного добутку (cross product) - для визначення ліво/право
(defun cross_product (v1 v2)
  (setq v1 (if (= (length v1) 2) (append v1 '(0.0)) v1)) ; Перетворюємо 2D в 3D
  (setq v2 (if (= (length v2) 2) (append v2 '(0.0)) v2)) ; Перетворюємо 2D в 3D
  (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
        (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
        (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
)

;; Допоміжні функції для перетворення кутів
(defun dtr (a) (* pi (/ a 180.0))) ; Градуси в радіани
(defun rtd (a) (* 180.0 (/ a pi))) ; Радіани в градуси

;; Допоміжна функція для вибору блоку та отримання його точки вставки та VLA-об'єкта
;; Повертає список (координати_точки_вставки . VLA_об'єкт_блоку) або nil
(defun GetBlockInsertionPointAndVLA (prompt_msg / ent_data ent_name ent_list ent_type insertion_pt vla_obj)
  (setq ent_data (entsel prompt_msg))
  (if ent_data
    (progn
      (setq ent_name (car ent_data))
      (setq vla_obj (vlax-ename->vla-object ent_name))
      (setq ent_list (entget ent_name))
      (setq ent_type (cdr (assoc 0 ent_list)))

      (if (equal ent_type "INSERT")
        (progn
          (setq insertion_pt (cdr (assoc 10 ent_list))) ; Отримуємо точку вставки з DXF-коду 10
          (princ (strcat "\nОбрано блок. Точка: " (vl-princ-to-string insertion_pt)))
          (cons insertion_pt vla_obj) ; Повертаємо пару: координати . VLA-об'єкт
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
;; == ОСНОВНА ФУНКЦІЯ СКРИПТА ==
;; ============================================================
(defun c:DrawSwitchAxisPro ( / p1_data p2_data p4_data p3_data p5_data ; <--- Додав p2_data
                                 p1_coords p2_coords p4_coords p3_coords p5_coords ; <--- Додав p2_coords
                                 straight_axis_obj ; Змінив назву для прямої осі (була line_straight_obj)
                                 proj_pt_p3 proj_pt_p2 csp_pt branch_angle branch_end_pt ; <--- Додав proj_pt_p2
                                 branch_axis_obj p2_projected_on_straight p5_projected_on_branch
                                 p2_block_vla p5_block_vla ; <--- Додав p2_block_vla
                                 final_p2_target_pt final_p5_target_pt
                                 temp_straight_axis_ent temp_branch_axis_ent ) ; <--- Змінні для тимчасових осей

  ;; Зберегти поточні налаштування AutoCAD
  (setq *oldEcho* (getvar "CMDECHO"))
  (setq *oldOsmode* (getvar "OSMODE"))
  (setq *oldCmdDia* (getvar "CMDDIA"))
  (setq *oldOsmodeZ* (getvar "OSNAPZ"))
  
  ;; Встановити налаштування для скрипта
  (setvar "CMDECHO" 0) ; Вимикаємо ехо команд
  (setvar "CMDDIA" 0) ; Вимикаємо діалоги
  (setvar "OSNAPZ" 0) ; Тимчасово встановлюємо OSNAPZ в 0 для коректної роботи MOVE з 3D точками

  (princ "\n--- Побудова осі стрілочного переводу (1/11) за блоками (Pro) ---")

  ;; 1. Запит блоків у користувача та отримання їхніх координат і VLA-об'єктів
  (setq p1_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки стику рамної рейки (P1): "))
  (if (not p1_data) (*error* "Вибір P1 скасовано."))
  (setq p1_coords (car p1_data))

  (setq p2_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки початку вістря (P2): ")) ; <--- НОВИЙ ВИБІР БЛОКУ P2
  (if (not p2_data) (*error* "Вибір P2 скасовано."))
  (setq p2_coords (car p2_data))
  (setq p2_block_vla (cdr p2_data)) ; VLA-об'єкт блоку P2

  (setq p4_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по прямому напрямку (P4): "))
  (if (not p4_data) (*error* "Вибір P4 скасовано."))
  (setq p4_coords (car p4_data))

  (setq p3_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки центру хрестовини (P3): "))
  (if (not p3_data) (*error* "Вибір P3 скасовано."))
  (setq p3_coords (car p3_data))

  (setq p5_data (GetBlockInsertionPointAndVLA "\nВиберіть блок для точки хвоста хрестовини по відгалуженню (P5): "))
  (if (not p5_data) (*error* "Вибір P5 скасовано."))
  (setq p5_coords (car p5_data))        ; Оригінальні координати P5 блоку
  (setq p5_block_vla (cdr p5_data))      ; VLA-об'єкт блоку P5

  ;; --- Починаємо побудову, використовуючи отримані координати ---

  ;; 2. Побудова початкової прямої полілінії між P1 і P4 (тимчасова для проекції P2 і P3)
  (command "_.PLINE" p1_coords p4_coords "")
  (setq straight_axis_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт цієї тимчасової лінії
  (setq temp_straight_axis_ent (entlast)) ; Зберігаємо ім'я ентоєм

  ;; 3. Проектування P2 та P3 на тимчасову пряму вісь
  (setq p2_projected_on_straight (vlax-curve-getClosestPointTo straight_axis_obj p2_coords)) ; <--- Проектуємо P2
  (setq proj_pt_p3 (vlax-curve-getClosestPointTo straight_axis_obj p3_coords)) ; Проектуємо P3
  ;(command "_.POINT" p2_projected_on_straight) ; Для візуалізації
  ;(command "_.POINT" proj_pt_p3) ; Для візуалізації

  ;; 4. Розрахунок Центру Стрілочного Переводу (ЦСП) на основі спроектованої P3
  (setq dist_to_csp 16.72)
  (setq vec_p1_proj (mapcar '- p1_coords proj_pt_p3)) ; Вектор від проекції P3 до P1
  (setq vec_p1_proj_unit (unit_vector vec_p1_proj))
  (setq csp_pt (mapcar '+ proj_pt_p3 (mapcar '* vec_p1_proj_unit (list dist_to_csp dist_to_csp 0.0))))
  ;(command "_.POINT" csp_pt) ; Для візуалізації

  ;; --- Переміщення блоку P2 на спроектовану точку (ЗБЕРІГАЮЧИ Z!) ---
  (if p2_block_vla
    (progn
      (setq final_p2_target_pt (list (car p2_projected_on_straight)
                                     (cadr p2_projected_on_straight)
                                     (caddr p2_coords))) ; <--- ЗБЕРІГАЄМО ОРИГІНАЛЬНУ Z
      
      (command "_move" (vlax-vla-object->ename p2_block_vla) "" 
               "_none" p2_coords ; Оригінальна точка P2
               "_none" final_p2_target_pt)
      (princ "\nБлок P2 переміщено на спроектовану точку, ЗБЕРІГАЮЧИ оригінальну Z-координату.")
    )
    (princ "\nПомилка: Не вдалося перемістити блок P2, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини ПРЯМОЇ осі (P1 - P2_proj - P4) ---
  ;; Видаляємо стару тимчасову пряму полілінію, створюємо нову з 3-ма вершинами
  (if temp_straight_axis_ent
    (progn
      (vla-delete (vlax-ename->vla-object temp_straight_axis_ent))
      (setq straight_axis_obj nil) ; Очищаємо змінну
      (princ "\nТимчасова пряма вісь видалена.")
      
      ;; Створюємо нову пряму полілінію P1 - P2_proj - P4
      ;; Z-координата для вершин полілінії буде взята з їхніх X,Y,Z значень
      ;; або Elevation, якщо це 2D полілінія.
      ;; Давайте для P2_proj використаємо Z з P2_coords, щоб вона була в одній площині з блоком.
      (setq p2_proj_for_pline (list (car p2_projected_on_straight)
                                    (cadr p2_projected_on_straight)
                                    (caddr p2_coords))) ; Використовуємо Z з оригінального P2
      
      (command "_.PLINE" p1_coords p2_proj_for_pline p4_coords "") ; <--- Нова пряма полілінія з P2_proj
      (setq straight_axis_obj (vlax-ename->vla-object (entlast))) ; Оновлюємо VLA-об'єкт прямої осі
      (princ "\nНова пряма вісь (P1-P2_proj-P4) створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити пряму вісь.")
  )

  ;; 5. Визначення напрямку відгалуження (вліво/вправо)
  ;; Важливо: для цього кроку потрібна нова пряма вісь, якщо її було змінено
  (setq vec_line (mapcar '- p4_coords p1_coords)) ; Вектор нової прямої P1-P4
  (setq vec_test (mapcar '- p5_coords p1_coords)) ; Використовуємо оригінальний P5
  (setq cross_z (caddr (cross_product vec_line vec_test)))
  (setq is_left (if (> cross_z 0) T nil))

  ;; 6. Побудова осі відгалуження від ЦСП (початкова, тимчасова)
  (setq branch_length 20.0) ; Довжина для створення тимчасової лінії, потім її замінимо
  (setq branch_angle_deg 5.194444444) ; 5d11'40"
  (setq branch_angle_rad (dtr branch_angle_deg))
  (setq straight_line_angle (angle p1_coords p4_coords)) ; Кут прямої осі (тепер це нова полілінія P1-P2_proj-P4)

  (if is_left
    (setq final_branch_angle (+ straight_line_angle branch_angle_rad))
    (setq final_branch_angle (- straight_line_angle branch_angle_rad))
  )

  (setq temp_branch_end_pt (polar csp_pt final_branch_angle branch_length))
  (command "_.PLINE" csp_pt temp_branch_end_pt "")
  (setq temp_branch_axis_ent (entlast)) ; Зберігаємо ім'я тимчасової полілінії

  ;; --- Проектування P5 на вісь відгалуження (навіть на тимчасову) ---
  ;; Нам потрібен VLA-об'єкт для vlax-curve-getClosestPointTo
  (setq branch_axis_obj (vlax-ename->vla-object temp_branch_axis_ent))
  (setq p5_projected_on_branch (vlax-curve-getClosestPointTo branch_axis_obj p5_coords))
  ;(command "_.POINT" p5_projected_on_branch) ; Для візуалізації

  ;; --- Переміщення блоку P5 на спроектовану точку (ЗБЕРІГАЮЧИ Z!) ---
  (if p5_block_vla
    (progn
      ;; Формуємо кінцеву цільову точку для блоку: XY зі спроектованої, Z з оригіналу P5
      (setq final_p5_target_pt (list (car p5_projected_on_branch)
                                     (cadr p5_projected_on_branch)
                                     (caddr p5_coords))) ; <--- ЗБЕРІГАЄМО ОРИГІНАЛЬНУ Z
      
      (command "_move" (vlax-vla-object->ename p5_block_vla) "" 
               "_none" p5_coords ; Використовуємо оригінальну точку P5 як базову для переміщення
               "_none" final_p5_target_pt)
      (princ "\nБлок P5 переміщено на спроектовану точку, ЗБЕРІГАЮЧИ оригінальну Z-координату.")
    )
    (princ "\nПомилка: Не вдалося перемістити блок P5, VLA-об'єкт не знайдено.")
  )

  ;; --- Обрізка/зміна довжини осі відгалуження (надійний підхід) ---
  (if (and temp_branch_axis_ent branch_axis_obj)
    (progn
      ;; 1. Видалити тимчасову полілінію відгалуження
      (vla-delete branch_axis_obj)
      (setq branch_axis_obj nil) ; Очищаємо змінну
      (princ "\nТимчасова вісь відгалуження видалена.")

      ;; 2. Створити нову, коректну полілінію від ЦСП до спроектованої P5
      ;; Z-координата для полілінії буде взята з Z ЦСП, щоб вона була горизонтальною, якщо ЦСП горизонтальний.
      (setq p5_proj_for_pline_branch (list (car p5_projected_on_branch) (cadr p5_projected_on_branch) (caddr csp_pt)))
      
      (command "_.PLINE" csp_pt p5_proj_for_pline_branch "") ; Створюємо нову полілінію
      (setq branch_axis_obj (vlax-ename->vla-object (entlast))) ; Отримуємо VLA-об'єкт нової, коректної полілінії
      (princ "\nНова вісь відгалуження від ЦСП до спроектованої P5 створена.")
    )
    (princ "\nПомилка: Не вдалося обрізати/змінити вісь відгалуження.")
  )

  (princ "\n--- Осі стрілочного переводу побудовано, блоки переміщено та осі скориговано! ---")
  (princ "\nСкрипт завершено.")
  (princ) ; Тихий вихід

  ;; Відновити оригінальні налаштування AutoCAD
  (setq *oldEcho* nil)
  (setq *oldOsmode* nil)
  (setq *oldCmdDia* nil)
  (setq *oldOsmodeZ* nil)

) ; кінець defun c:DrawSwitchAxisPro

;; ============================================================
;; == Повідомлення про завантаження ==
;; ============================================================
(princ "\nLISP 'DrawSwitchAxisPro' завантажено.")
(princ "\nДля запуску введіть DrawSwitchAxisPro.")
(princ)