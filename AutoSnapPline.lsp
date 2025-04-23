;; AutoSnapPline.lsp (����� 8.8 - ������ ������� FASTLINE)
;; �������: ����� -> ��'���. ����������: Entmake ������, 2D ³������.

;; ��������� ����� ��� �����'���������� ���������� ������ ��� AutoSnapPline
(setq *g_autosnap_last_radius* nil) ; ���������� �� nil

; --- �������� ������� ��� ��������� ������� ������ ---
; (��� ���)
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

; --- �������� ������� ��� ������ ����������� ---
; (��� ���)
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

; --- ������� ������� ������� (�������� ���������� �����) ---
(defun c:AutoSnapPline ( / *error* old_osmode old_cmdecho old_blipmode ; ������� ����
                         snap_radius start_ent start_pt ent_data initial_ent_type ; ����� ���
                         gr_status gr_data cursor_pt draw_color marker_size_ss loop_counter ; ����
                         ll ur filter nearby_ss found_now_ents i ename edata obj_pt ; ����� ��'����
                         highlighted_ents ; ������ ���������
                         detection_counter detection_threshold ; ����������
                         cursor_pt_2d obj_pt_2d dist_2d ; 2D �������
                         vertices_list temp_pline_segment_color pline_dxf vertex ; Entmake
                         prompt_radius_str 
                         )
  (vl-load-com)

  ; --- �������� ������� ---
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if old_blipmode (setvar "BLIPMODE" old_blipmode))
    (setq highlighted_ents (clear_highlights highlighted_ents))
    (redraw)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
        (princ (strcat "\n���������/�������: " msg)))
    (princ)
  )

  ; --- ���������� �� ������������ ��������� ������ ---
  (setq old_cmdecho (getvar "CMDECHO"))
  (setq old_osmode (getvar "OSMODE"))
  (setq old_blipmode (getvar "BLIPMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "BLIPMODE" 0)

  ; --- 1. �������� ����� ³����������/������ (� ���'����) ---
  ; ����������� / ������������ ����������� �������� (����. 1.0 �� �������������)
  (if (or (null (boundp '*g_autosnap_last_radius*)) (null *g_autosnap_last_radius*) (not (numberp *g_autosnap_last_radius*)))
      (setq *g_autosnap_last_radius* 1) ; ��������� �������� �� �������������, ���� �� ������
  )
  ; ���������� ������� � ��������� �� �������������
  (setq prompt_radius_str (strcat "\n������ ����� �����������/������ <" (rtos *g_autosnap_last_radius* 2 4) ">: "))
  (initget 6) ; ���������� ���� �� �������� �������� (2+4)
  ; �������� �������, ��������� �������� �� �������������
  ; getdist ������� *g_autosnap_last_radius*, ���� ���������� ������� Enter
  (setq snap_radius (getdist prompt_radius_str *g_autosnap_last_radius*))

  ; �������� �� �������� ������ �������� (getdist ������� nil ��� ESC)
  (if (or (null snap_radius) (<= snap_radius 0))
      (*error* "������� �������� ����� ��� ���������.")
      ; ���� ����� �������, �������� ���� ��� ���������� ����
      (setq *g_autosnap_last_radius* snap_radius)
  )

  ; --- 2. �������� ��������� ��'��� �� ���� ��� ---
  (setq start_pt nil initial_ent_type nil)
  (while (not start_pt)
    (setq start_ent (entsel (strcat "\n������� ��������� ��'��� POINT, CIRCLE ��� INSERT (����� ������: " (rtos snap_radius 2 2) "): ")))
    (cond
      ((null start_ent) (*error* "���� ���������."))
      ((listp start_ent)
       (setq ent_data (entget (car start_ent)))
       (setq initial_ent_type (cdr (assoc 0 ent_data)))
       (if (member initial_ent_type '("POINT" "CIRCLE" "INSERT"))
         (setq start_pt (cdr (assoc 10 ent_data)))
         (progn
            (princ (strcat "\n������� ��� ��'����: " initial_ent_type ". ������� POINT, CIRCLE ��� INSERT."))
            (setq initial_ent_type nil)
         )
       )
      )
      (t (princ "\n������� ����.")(exit))
    )
  )
  (if (or (not start_pt) (not initial_ent_type))
      (*error* "�� ������� ��������� �������� ����� ��� ��� ��'����.")
  )
  (setq vertices_list (list start_pt))

  ; --- ����������� ��� ����� ---
  (setq loop_counter 0)
  (setq draw_color 30)
  (setq temp_pline_segment_color 1)
  (setq marker_size_ss (* (getvar "VIEWSIZE") 0.015))
  (setq highlighted_ents nil)
  (setq detection_counter 0)
  (setq detection_threshold 5)

  (princ "\n--- ����� ²����������, ������ �� ��������� ��˲˲Ͳ� (v8.8) ---") ; �������� �����
  (princ (strcat "\n������ '" initial_ent_type "' � 2D ����� " (rtos snap_radius) ". ��� �� ���������� ��� ��������� �������."))
  (princ "\n�������� Enter/��� ��� ���������� �����, Esc ��� ����������.")

  ; --- ������������� ���� ---
  (while T
     (setq loop_counter (1+ loop_counter))
     (setq gr_data (grread T 2 0))
     (setq gr_status (car gr_data))
     (setq cursor_pt (cadr gr_data))

     (redraw) ; ����� ������� �� �������� ��������

     ; ��������� ���������� ��������
     (if (> (length vertices_list) 1)
        (progn
          (setq i 0)
          (while (< i (- (length vertices_list) 1))
             (grdraw (nth i vertices_list) (nth (1+ i) vertices_list) temp_pline_segment_color -1)
             (setq i (1+ i))
          )
        )
     )

     ; ������� ����
     (cond
       ; ���: ������ �������
       ((= gr_status 3)
         (if highlighted_ents
           (progn
             (setq target_ename (car highlighted_ents))
             (if (and target_ename (= (type target_ename) 'ENAME) (setq edata (entget target_ename)))
               (progn
                 (setq target_pt (cdr (assoc 10 edata)))
                 (if target_pt
                   (progn
                     (princ (strcat "\n������ �������: " (vl-princ-to-string target_pt)))
                     (setq vertices_list (append vertices_list (list target_pt)))
                     (setq highlighted_ents (clear_highlights highlighted_ents))
                   )
                   (princ "\n�������: ����������.")
                 )
               )
               (princ "\n�������: ��� ��'����.")
             )
           )
         )
       ) ; ʳ���� status 3

       ; ��� ����: ������� ������� �� �����������
       ((and (= gr_status 5) cursor_pt)
         (progn
           (draw_radius_markers cursor_pt snap_radius draw_color marker_size_ss)
           (setq detection_counter (1+ detection_counter))
           (if (>= detection_counter detection_threshold)
             (progn
               (setq detection_counter 0)
               ; ���� ������/ϳ���������� (2D ³������)
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
                   ) ; ����� repeat
                 ) ; ����� progn if nearby_ss
               ) ; ����� if nearby_ss
               ; ��������� ������������
               (setq highlighted_ents (clear_highlights highlighted_ents))
               (if found_now_ents
                  (progn
                     (foreach ent found_now_ents
                        (if (= (type ent) 'ENAME) (redraw ent 3))
                     )
                     (setq highlighted_ents found_now_ents)
                  )
               )
               ; --- ʳ���� ����� ������/ϳ���������� ---
             ) ; ����� progn ��� �����, �� ���������� ����
           ) ; ����� if ��� �������� ������ ���������
         ) ; ����� progn ��� ������� ��䳿 ���� ����
       ) ; ʳ���� status 5

       ; ������ ������: �������� ������ (Enter/���) ��� ��������� (Esc)
       ((or (= gr_status 11) (= gr_status 25) (and (= gr_status 2) (= (cadr gr_data) 13))) ; Enter ��� ���
          (if (> (length vertices_list) 1)
             (progn
                (princ (strcat "\n��������� ������ � " (itoa (length vertices_list)) " ������..."))
                (setq pline_dxf (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                      (cons 90 (length vertices_list))
                                      '(70 . 0)
                                )
                )
                (foreach vertex vertices_list
                   (setq pline_dxf (append pline_dxf (list (cons 10 vertex))))
                )
                (entmake pline_dxf)
                (princ " ������.")
             )
             (princ "\n����������� ������ (������� ����� 2) ��� ��������� �����.")
          )
          (princ "\n����������.")
          (exit)
       )
       ((and (= gr_status 2) (= (cadr gr_data) 27)) ; Esc
          (*error* "��������� ������������ (Esc).")
          (exit)
       )

       ; ���� ������� �����������
       (t nil)

     ) ; ����� ��������� cond

  ) ; ����� while

  ; --- �������� ��� ����������� ����� ---
  (*error* nil)
  (princ)
) ; ����� c:AutoSnapPline

; --- === �������� ������� (�����) === ---

; �������� ���� ASPL
(defun c:ASPL ()
  (c:AutoSnapPline)
  (princ)
)

; ������� ���� PCON (Polyline Connect / Point Connect)
(defun c:PCON ()
  (c:AutoSnapPline)
  (princ)
)

; �� ���� ���� FASTLINE <<-- ������
(defun c:FASTLINE ()
  (c:AutoSnapPline) ; ��������� ������� �������
  (princ)
)

; --- ����������� ��� ������������ (� �������� ������) ---
(princ "\n������ AutoSnapPline (v8.8) �����������.") ; <<-- �̲���� �����
(princ "\n������� �������: AutoSnapPline, ASPL, PCON, FASTLINE") ; <<-- ������ FASTLINE
(princ)