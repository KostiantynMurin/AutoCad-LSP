;; ��������� �������� ����� ����� (���������� ������/�������� �������)
;; ���Ѳ� ��� `vlax-curve-isKindOf` (��� ��� �� �������� �� ����� ������� vlax-curve-*)

(defun C:DrawSlope (/ *error* old_osmode old_cmdecho e_top p_top obj_top e_bot p_bot obj_bot interval total_len cur_dist pt_on_top perp_angle pt_on_bottom stroke_len long_len short_len is_long_stroke end_pt acadObj acadDoc esel_top esel_bot clayer_name clayer_data clayer_flags ent_type_top ent_type_bot) ; ������ ���� ��� ���� ��'����

  ;; --- �������� ������� ---
  (defun *error* (msg)
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if acadDoc (vla-EndUndoMark acadDoc))
    (cond ((not msg))
          ((wcmatch (strcase msg T) "*BREAK,*CANCEL*,*EXIT*,*����� �����������*"))
          ((princ (strcat "\n������� DrawSlope: " msg)))
    )
    (princ)
  )

  ;; --- ������� ������� ---
  (vl-load-com) ; ������ ����������� Visual LISP (��� �� ���� �������!)
  (setq acadObj (vlax-get-acad-object))
  (setq acadDoc (vla-get-ActiveDocument acadObj))

  ;; --- �������� ������� ���� ---
  (setq old_osmode (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))

  ;; --- �������� ��������� ���� � ������� ����� ����� ---
  (setq clayer_name (getvar "CLAYER"))
  (setq clayer_data (tblsearch "LAYER" clayer_name))
  (if (not clayer_data)
      (progn (princ (strcat "\n�������: �� ������� ������ ��� ��� ��������� ���� '" clayer_name "'.")) (*error* "������� ������� �� ����� ����"))
      (progn
          (setq clayer_flags (cdr (assoc 70 clayer_data)))
          (if (= 1 (logand 1 clayer_flags))
              (progn (princ (strcat "\n�������� ��� '" clayer_name "' �����������. ����������� ���� ��� ����� �������� ���.")) (*error* "�������� ��� �����������"))
              (vla-StartUndoMark acadDoc) ; ������ Undo ����� ���� ��� �� �����������
          )
      )
  )

  ;; --- �������� ��'����� ����'���� �� ��� ������ ---
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  ;; --- �������� ������ ���� (������) ---
  (setq e_top nil obj_top nil) ; ������������ obj_top �� nil
  (while (not e_top)
    (setq esel_top (entsel "\n������� ������ ����� (˳��, ������, ����, ������, ����): "))
    (if esel_top
      (progn
        (setq e_top (car esel_top))
        (setq p_top (cadr esel_top))
        ;; ������������� �������� ���� ��'���� (��� vlax-curve-isKindOf)
        (setq ent_type_top (cdr (assoc 0 (entget e_top))))
        (if (not (member ent_type_top '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "SPLINE" "ELLIPSE")))
            (progn
                (princ (strcat "\n�������� ��'��� ('" ent_type_top "') �� �����������. ������� ˳��, ������, ����, ������ ��� ����."))
                (setq e_top nil) ; ������� ����, ��� �������� �����
            )
            ;; ���� ��� ��������, �����Ӫ�� VLA ��'��� (��� �� ������� ��� ����� �������!)
            (progn
                ;; ��������� ����� ����������Ӫ VLAX - ���� ��������� �������!
                (setq obj_top (vlax-ename->vla-object e_top))
                ;; ��������� ��������, �� ������� �������� VLA ��'���
                (if (not obj_top)
                    (progn
                        (princ (strcat "\n������� ��������� VLA-��'���� ��� ������ ���� '" ent_type_top "'. �������, Visual LISP �� ������."))
                        (*error* "�� ������� �������� VLA-��'���")
                    )
                )
            )
        )
      )
      (progn (princ "\n��'��� �� �������, �������� ���������.") (*error* "��������� ������������"))
    )
  )

  ;; --- �������� ����� ���� (ϳ�����) ---
  (setq e_bot nil obj_bot nil) ; ������������ obj_bot �� nil
  (while (not e_bot)
    (setq esel_bot (entsel "\n������� ������ ����� (˳��, ������, ����, ������, ����): "))
    (if esel_bot
      (progn
        (setq e_bot (car esel_bot))
        (setq p_bot (cadr esel_bot))
        (if (= e_top e_bot)
           (progn (princ "\n������ �� ������ �� ������ ���� ����� � ��� �� ��'�����.")(setq e_bot nil))
           (progn
             ;; ������������� �������� ���� ��'����
             (setq ent_type_bot (cdr (assoc 0 (entget e_bot))))
             (if (not (member ent_type_bot '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "SPLINE" "ELLIPSE")))
                 (progn
                     (princ (strcat "\n�������� ��'��� ('" ent_type_bot "') �� �����������. ������� ˳��, ������, ����, ������ ��� ����."))
                     (setq e_bot nil) ; ������� ����
                 )
                 ;; ���� ��� ��������, �����Ӫ�� VLA ��'���
                 (progn
                    ;; ��������� ����� ����������Ӫ VLAX - ���� ��������� �������!
                    (setq obj_bot (vlax-ename->vla-object e_bot))
                    ;; ��������� ��������, �� ������� �������� VLA ��'���
                    (if (not obj_bot)
                        (progn
                            (princ (strcat "\n������� ��������� VLA-��'���� ��� ������ ���� '" ent_type_bot "'. �������, Visual LISP �� ������."))
                            (*error* "�� ������� �������� VLA-��'���")
                        )
                    )
                 )
             )
           )
        )
      )
      (progn (princ "\n��'��� �� �������, �������� ���������.") (*error* "��������� ������������"))
    )
  )

  ;; --- �������� �������� ---
  (setq interval nil)
  (while (not interval)
    (initget 6) ; �� ����, �� ��'����
    (setq interval (getdist "\n������ �������� �� �������� ������ ������: "))
    (if (not interval)
        (progn (princ "\n�������� ���������.") (*error* "��������� ������������"))
        (if (<= interval 1e-6)
            (progn (princ "\n�������� ������� ���� ���������� ������.") (setq interval nil))
        )
    )
  )

  ;; --- ���� ���������� �� ��������� ---
  ;; !!! ������Ͳ ����� �������������� ��ز VLAX-CURVE-* ����ֲ� !!!
  ;; !!! ���� VISUAL LISP �� ����ު ��������, ��� ���� ������� !!!
  (princ "\n���������� ��������� �����...")
  ;; ��������� ����� ����������Ӫ VLAX - ���� ��������� �������!
  (setq total_len (vlax-curve-getDistAtParam obj_top (vlax-curve-getEndParam obj_top)))
  (setq cur_dist 0.0)
  (setq is_long_stroke T)

  (princ "\n��������� �������...")
  (while (<= cur_dist (+ total_len 1e-6))
    ;; ������Ͳ ����� �������������� VLAX - ������ ��������� �������!
    (setq pt_on_top (vlax-curve-getPointAtDist obj_top cur_dist))
    (setq pt_on_bottom (vlax-curve-getClosestPointTo obj_bot pt_on_top))

    (if (and pt_on_top pt_on_bottom)
      (progn
        (setq long_len (distance pt_on_top pt_on_bottom))
        (setq short_len (/ long_len 2.0))
        (setq stroke_angle (angle pt_on_top pt_on_bottom))

        (if is_long_stroke (setq stroke_len long_len) (setq stroke_len short_len))

        (if (> stroke_len 1e-6)
           (progn
              (setq end_pt (polar pt_on_top stroke_angle stroke_len))
              (entmake (list (cons 0 "LINE") (cons 10 pt_on_top) (cons 11 end_pt)))
           )
        )
        (setq is_long_stroke (not is_long_stroke))
      )
      (princ (strcat "\n��������� ������ �������� ����� �� ����� ��� ������ " (rtos cur_dist) "."))
    )
    (setq cur_dist (+ cur_dist interval))
  )
  (princ " ���������.")

  ;; --- ³������� ������� ���� ---
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)

  (princ)
)

; --- ����������� ��� ������������ ---
(princ "\nLISP 'DrawSlope' (��� vlax-curve-isKindOf) �����������. ������ 'DrawSlope'.")
(princ)