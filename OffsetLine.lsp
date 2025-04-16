;;; OffsetBlue.lsp


;;; ������� ������ ���� �� ��� ����� �� ������� 0.8,
;;; ����� ���� ���� � ������ ���� (������ 7)
;;; �� ���������� ������� �� 0.30 ��.

(defun C:OffsetLineToBlack (/ *error* doc obj sel ptoff new_obj obj_data new_obj_data)
  ;; ������� ������� �������
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\n�������: " msg))
    )
    (princ) ; ����� �����
  )

  ;; �������� �������� �������� (��� ������ ����� AutoCAD)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  ;; ������ �������� Undo (�������� ������� ��� �� �������)
  (vla-startundomark doc)

  ;; 1 & 2: ����� �� ���� ��'���� (�� ��� �����)
  (setq sel nil) ; �������� ����
  (while (not sel) ; �����������, ���� �� ���� ������ ���������� ��'���
    (setq obj (entsel "\n������ ��� ��� ������ ��� �������: "))
    (if obj
      (progn
        (setq obj_ent (car obj))
        (setq obj_data (entget obj_ent))
        (setq obj_type (cdr (assoc 0 obj_data))) ; �������� ��� ��'����
        (if (or (= obj_type "LINE") (= obj_type "LWPOLYLINE") (= obj_type "POLYLINE") (= obj_type "SPLINE") (= obj_type "ARC") (= obj_type "CIRCLE") (= obj_type "ELLIPSE")) ; �������� ����
          (setq sel obj_ent) ; �������� ��'� �������, ���� ��� ��������
          (princ "\n������� ��'��� �� ����������� ��� ������� ���� ��������. ������ ��� ��� ������.")
        )
      )
      (progn ; ���� ����� �� ������ (��������� Esc ��� Enter)
        (princ "\n���� ���������.")
        (setq sel T) ; ���������� ��������� ��� ������ � ����� while
      )
    )
  )

  ;; ���� ��'��� ���� ������ ������ (sel �� T � �� nil)
  (if (and sel (/= sel T))
    (progn
      ;; 3: ����� �� ������� ������� �������
      (setq ptoff (getpoint "\n������ ����� �� ������ �������: "))
      (if ptoff
        (progn
          ;; 4: ��������� ������� ������� (OFFSET)
          ;; ������������� command ��� ������� ���������� ������� AutoCAD
          ;; "_OFFSET" - ������� ������� ( _ ��� ��������� ����)
          ;; 0.8      - ������� �������
          ;; sel      - ��'��� ��� ������� (���� ��'� �������)
          ;; ptoff    - �����, �� ����� �������
          ;; ""       - ���������� ������� OFFSET
          (command "_OFFSET" 0.8 sel ptoff "")

          ;; 5: ��������� ���������� ���������� ��'���� �� ���� ���� �������
          (setq new_obj (entlast))
          (if (and new_obj (/= new_obj sel))
            (progn
              (setq new_obj_data (entget new_obj))

              ;; --- ������������ True Color ������ (RGB 0,0,0) ---
              ;; ������������� DXF ��� 420 ��� True Color.
              ;; �������� - 32-���� ���� ����� 0x00RRGGBB. ��� ������� (0,0,0) �� 0.
              (if (assoc 420 new_obj_data)
                (setq new_obj_data (subst '(420 . 0) (assoc 420 new_obj_data) new_obj_data))
                (setq new_obj_data (append new_obj_data '((420 . 0))))
              )
              ;; ��� �����������, �� True Color �� ��������,
              ;; ������������ ��������� ���� (62) � ByLayer (256).
              (if (assoc 62 new_obj_data)
                  (setq new_obj_data (subst '(62 . 256) (assoc 62 new_obj_data) new_obj_data))
                  (setq new_obj_data (append new_obj_data '((62 . 256))))
              )

              ;; --- ���� ������� �� �� 0.30 �� (����������) ---
              ;; DXF ��� 370, �������� 30 ��� 0.30 ��
              (if (assoc 370 new_obj_data)
                 (setq new_obj_data (subst '(370 . 30) (assoc 370 new_obj_data) new_obj_data))
                 (setq new_obj_data (append new_obj_data '((370 . 30))))
              )

              ;; ����������� ���� �� ��'����
              (entmod new_obj_data)
              (princ (strcat "\n��'��� ������ �� 0.8, ����������� � ������ �� ����������� ������� 0.30 ��."))
            )

            (princ "\n�� ������� �������� ������� ��'���.")
          )
        )
        (princ "\n������� ����� ���������.")
      )
    )
    (if (not (eq sel T)) (princ "\n�� ������� ������ ��'���.")) ; �����������, ���� ���� �� ������
  )

  ;; ��������� �������� Undo
  (vla-endundomark doc)

  (princ) ; ����� ����� � ������� (�� ���������� ��������� ��������)
)

;; ============================================================
;; == ���������� ������� ������� ��� ������� ==
;; ============================================================
(defun c:OLTB () (c:OffsetLineToBlack)) ;; OLB - OffsetLineBlue

;; ����������� ��� ������������
(princ "\nLISP ������ 'OffsetLine.lsp' �����������. ������ OLTB ��� OFFSETLINETODLACK ��� ������� ��� �� ������ ���� �� ������ � ����� 0.30��.")
(princ)