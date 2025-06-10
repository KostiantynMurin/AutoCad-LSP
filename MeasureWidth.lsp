;; =================================================================== ;;
;; Скрипт для вимірювання ширини між двома полілініями.                ;;
;; Версія 9.0: Принципово новий метод через вибір об'єктів.            ;;
;; Команда для запуску: WWP                                              ;;
;; =================================================================== ;;

(defun c:WWP ( / *error* old_cmdecho old_osmode pline1 pline2 pline1_vla pline2_vla
                p_approx p_on_pl1 p_on_pl2 dist dist_text mid_p text-ent
                text-style text-height text-color text-angle cur-layer
             )
  (vl-load-com) ; Завантажуємо функції Visual LISP

  ;; Функція для коректної обробки помилок та натискання Esc
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nПомилка: " msg))
    )
    (princ)
  )

  ;; Зберігаємо поточні налаштування AutoCAD
  (setq old_cmdecho (getvar "CMDECHO")
        old_osmode  (getvar "OSMODE")
  )
  (setvar "CMDECHO" 0)

  ;; --- Налаштування параметрів тексту ---
  (setq text-style  "Д-431"
        text-height 1.0
        text-color  5
        text-angle  0.0
  )
  
  ;; --- КРОК 1: ВИБІР ДВОХ ПОЛІЛІНІЙ ---
  (princ "\nОберіть дві полілінії для вимірювання ширини між ними.")
  (setq pline1 (car (entsel "\nОберіть першу полілінію: ")))
  (if (null pline1) (princ "\nКоманду скасовано.") (progn
    (setq pline2 (car (entsel "\nОберіть другу полілінію: ")))
    (if (null pline2) (princ "\nКоманду скасовано.") (progn
      
      (setq pline1_vla (vlax-ename->vla-object pline1))
      (setq pline2_vla (vlax-ename->vla-object pline2))
      
      ;; --- КРОК 2: ЦИКЛ ВИМІРЮВАННЯ ---
      (while
        (setq p_approx (getpoint "\n>> Вкажіть приблизну точку для виміру (або Enter/Esc для виходу): "))
        
        ;; Знаходимо найближчі точки на полілініях до вказаної
        (setq p_on_pl1 (vlax-curve-getClosestPointTo pline1_vla p_approx))
        (setq p_on_pl2 (vlax-curve-getClosestPointTo pline2_vla p_approx))
        
        (setq dist (distance p_on_pl1 p_on_pl2))
        (setq dist_text (rtos dist 2 2))
        
        (setq mid_p (mapcar (function (lambda (a b) (/ (+ a b) 2.0))) p_on_pl1 p_on_pl2))
        
        (setq cur-layer (getvar "CLAYER"))
        
        ;; Створюємо текст в середній точці
        (entmake
          (list '(0 . "TEXT") (cons 1 dist_text) (cons 10 mid_p) (cons 40 text-height)
                (cons 50 text-angle) (cons 7 text-style) (cons 8 cur-layer)
                (cons 62 text-color) '(72 . 4) '(73 . 2)
          )
        )
        (setq text-ent (entlast))

        ;; Запускаємо команду ПЕРЕМІСТИТИ
        (setvar "CMDECHO" 1)
        (command "_MOVE" text-ent "" mid_p pause)
        (setvar "CMDECHO" 0)
      )
    ))
  ))
  
  ;; Відновлюємо збережені налаштування AutoCAD
  (setvar "CMDECHO" old_cmdecho)
  (setvar "OSMODE" old_osmode)
  (princ)
)

(princ "\nСкрипт 'WWP' (v9.0, вибір об'єктів) завантажено. Введіть команду WWP.")
(princ)