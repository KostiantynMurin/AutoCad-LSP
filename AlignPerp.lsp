;; LISP ������ ��� AutoCAD: ����������� ��'���� ��������������� � ����� ��������
;; ��'� �������: AlignPerp
;; �����: AI Assistant (Gemini)
;; ����: 2025-04-14
;; �����: 2.1 (��������� ����������, �������� �����, ˳��, ������)
;; ����:
;;   1. ���������� ������� ���� ����, ˲Ͳ� ��� ��˲˲Ͳ�.
;;   2. ���������� ������� ������� AlignPerp.
;;   3. ������ ���� ���� �� ��� �����, �� ����������� �������� ��'���.
;;   4. ���� ������� ��������:
;;      - ��� ���ʲ� �� ˲Ͳ�: ������ ������� ��'��� ������� ����� ��������
;;        ���, ��� �� ���� ���������������� �� ���� ��/����� � ��� �����.
;;        ��'��� �� �����������, ���� �����������.
;;      - ��� ��˲˲Ͳ�: ������� �� ���������� (�������� ������������),
;;        ������� ��������� ���� ���� ����������������.
;;

(vl-load-com) ; ������������, �� VLA ������� �������

(defun c:AlignPerp (/ *error* oldCmdEcho oldOsmode selSet objEnt objVla objType minPt maxPt expFactor p1 p2 ssCandidates i candEnt candVla intPoints intPtArr intPt param tangentVec tangentAng perpAng currentAng rotAngleRad rotAngleDegrees foundIntersection intersectionProcessed)

  ;; --- �������� ������� ---
  (defun *error* (msg)
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho))
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if (and *acadDoc* (not (vlax-object-released-p *acadDoc*)))
        (vla-EndUndoMark *acadDoc*)
    )
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (princ (strcat "\n�������: " msg))
    )
    (princ (strcat "\n��������� " command-name " ���������."))
    (princ) ; ����� �����
  )

  ;; --- ������� ������� ---
  (setq command-name "AlignPerp") ; �������� ��'� ������� ��� ����������
  (setq oldCmdEcho (getvar "CMDECHO"))
  (setq oldOsmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)

  (setq *acadDoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark *acadDoc*)

  (setq foundIntersection nil)     ; ���������, �� �������� ���� � ���� �������
  (setq intersectionProcessed nil) ; ���������, �� ������� ���� ��������� (� ��������� ��� �������������)

  ;; --- ��������� ���������� ��������� ��'���� ---
  (prompt (strcat "\n��������� ���� ���������� �������� ����, ˲Ͳ� ��� ��˲˲Ͳ�... �������� " command-name))
  (setq selSet (ssget "_P"))

  ;; --- �������� ������ ---
  (if (or (not selSet) (/= (sslength selSet) 1))
    (progn (princ "\n���� �����, ������� в��� ���� ��'��� (����, ˳�� ��� ������) ����� �������� �������.") (*error* nil))
  )

  (setq objEnt (ssname selSet 0))
  (setq objVla (vlax-ename->vla-object objEnt))
  (setq objType (strcase (vla-get-ObjectName objVla))) ; �������� ��� ��'���� � ��������� ������

  ;; �������� ���� ��'����
  (if (not (wcmatch objType "*BLOCK*,*LINE*")) ; *LINE* ������ LINE �� LWPOLYLINE
      (progn (princ "\n�������� ��'��� �� � ������, ˳�� ��� ������.") (*error* nil))
  )

  (princ (strcat "\n������� ��'����: " (vl-princ-to-string objType)))

  ;; --- ��������� ����������� ���������� ��'���� ��� ������ ��� ����� ---
  (vl-catch-all-apply 'vla-GetBoundingBox (list objVla 'minPt 'maxPt))
  (if (or (not minPt) (not maxPt))
      ;; ������ �������� ���������� ��� ���/������, ���� GetBoundingBox �� ���������
      (if (wcmatch objType "*LINE*")
          (progn
            (setq minPt (vlax-get objVla (if (wcmatch objType "*POLYLINE*") 'Coordinates 'StartPoint)))
            (setq maxPt (vlax-get objVla (if (wcmatch objType "*POLYLINE*") 'Coordinates 'EndPoint)))
            ;; ��� ������ ���������� - �� ������, ����� ������ min/max
            (if (wcmatch objType "*POLYLINE*")
                (let ((coords (vlax-safearray->list (vlax-variant-value minPt))) x y minx miny maxx maxy)
                     (setq minx (car coords) miny (cadr coords) maxx minx maxy miny)
                     (while coords
                       (setq x (car coords) y (cadr coords))
                       (if (< x minx) (setq minx x))
                       (if (< y miny) (setq miny y))
                       (if (> x maxx) (setq maxx x))
                       (if (> y maxy) (setq maxy y))
                       (setq coords (cddr coords))
                     )
                     (setq minPt (list minx miny 0.0) maxPt (list maxx maxy 0.0)) ; ���������� 2D
                )
                ;; ��� �� StartPoint/EndPoint ��� � �������
                (setq minPt (list (apply 'min (list (car minPt) (car maxPt))) (apply 'min (list (cadr minPt) (cadr maxPt))) 0.0))
                (setq maxPt (list (apply 'max (list (car minPt) (car maxPt))) (apply 'max (list (cadr minPt) (cadr maxPt))) 0.0))
            )
          )
          (progn (princ "\n�� ������� �������� ���������� ��������� ��� ���������� ��'����.") (*error* nil))
      )
  )


  ;; ���������� ������� ������ ����� �� ��� ��������
  (setq expFactor 1.0)
  (setq p1 (list (- (car minPt) expFactor) (- (cadr minPt) expFactor) (caddr minPt)))
  (setq p2 (list (+ (car maxPt) expFactor) (+ (cadr maxPt) expFactor) (caddr maxPt)))

  ;; --- ����� ˲Ͳ� �� ��˲˲Ͳ� � ��������� ������ ---
  (princ "\n����� ���/������, �� ����������� �������� ��'���...")
  (setq ssCandidates (ssget "_C" p1 p2 '((0 . "*LINE")))) ; �������� LINE �� LWPOLYLINE

  (if (not ssCandidates)
    (progn (princ "\n������� ��������� ��'���� �� �������� ��� ��� ������ ��� �������� ��������.") (*error* nil))
  )

  ;; --- �������� �� ��������� ����/������� ��� ������ �������� ---
  (setq i 0)
  (while (and (< i (sslength ssCandidates)) (not intersectionProcessed)) ; ����������� ���� ����� ������ �������
    (setq candEnt (ssname ssCandidates i))
    (setq i (1+ i))

    ;; ���������� ��� �������� ��'���
    (if (/= objEnt candEnt)
      (progn
        (setq candVla (vlax-ename->vla-object candEnt))
        (princ (strcat "\n  �������� �������� �: " (vla-get-ObjectName candVla)))

        (setq intPoints (vl-catch-all-apply 'vlax-invoke-method (list objVla 'IntersectWith candVla acExtendNone)))

        (if (vl-catch-all-error-p intPoints)
            (princ (strcat "\n    ������� ��� �������: " (vl-catch-all-error-message intPoints)))
          (if (and intPoints (not (vl-catch-all-error-p intPoints)) (= (vlax-variant-type intPoints) (+ vlax-vbArray vlax-vbDouble)))
             (progn
                (setq intPtArr (vlax-safearray->list (vlax-variant-value intPoints)))
                (if (> (length intPtArr) 0)
                  (progn
                    (setq intPt (list (car intPtArr) (cadr intPtArr) (caddr intPtArr)))
                    (princ (strcat "\n    �������� ������� � �����: " (vl-princ-to-string intPt)))
                    (setq foundIntersection T) ; ��������, �� ������� ����

                    ;; --- ��������� ������� �� �������������� ��������� � ����� �������� ---
                    (setq param (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list candVla intPt)))
                    (if (vl-catch-all-error-p param)
                      (princ (strcat "\n      ������� ��������� ��������� �����-���������: " (vl-catch-all-error-message param)))
                      (progn
                        (setq tangentVec (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list candVla param)))
                        (if (vl-catch-all-error-p tangentVec)
                          (princ (strcat "\n      ������� ��������� ������� ������� ���������: " (vl-catch-all-error-message tangentVec)))
                          (if (< (distance '(0 0 0) tangentVec) 1e-9)
                            (princ "\n      ��������� ��������� ������� ��������� � ��� �����.")
                            (progn
                              ;; --- ���������� ����������������� ���� ---
                              (setq tangentAng (angle '(0 0 0) tangentVec))
                              (setq perpAng (+ tangentAng (/ pi 2.0)))

                              ;; --- ������� �������� � ��������� �� ���� ��������� ��'���� ---
                              (cond
                                ((wcmatch objType "*BLOCK*")
                                 (setq currentAng (vla-get-Rotation objVla)) ; ��� ����� (������)
                                 (setq rotAngleRad (- perpAng currentAng))
                                 (setq rotAngleDegrees (/ (* rotAngleRad 180.0) pi))
                                 (princ (strcat "\n    ������� ����� ������� ����� �������� �� " (rtos rotAngleDegrees 2 4) " �������."))
                                 (command "_.ROTATE" objEnt "" "_non" intPt rotAngleDegrees)
                                 (setq intersectionProcessed T) ; ���������, �� ��������
                                )
                                ((wcmatch objType "ACDBLINE") ; ҳ���� ��� LINE (�� LWPOLYLINE)
                                 (setq currentAng (vla-get-Angle objVla)) ; ��� �� (������)
                                 (setq rotAngleRad (- perpAng currentAng))
                                 (setq rotAngleDegrees (/ (* rotAngleRad 180.0) pi))
                                 (princ (strcat "\n    ������� ˲Ͳ� ������� ����� �������� �� " (rtos rotAngleDegrees 2 4) " �������."))
                                 (command "_.ROTATE" objEnt "" "_non" intPt rotAngleDegrees)
                                 (setq intersectionProcessed T) ; ���������, �� ��������
                                )
                                ((wcmatch objType "*POLYLINE*") ; ��� LWPOLYLINE
                                 (princ "\n    ������������: �������� ��'��� - ������. ������� ������ �� ���������� ��� ��������.")
                                 (setq intersectionProcessed T) ; ���������, �� �������� (�������������)
                                )
                                (T ; ���� ������ ���� (����������� ����� ������)
                                 (princ (strcat "\n    �������� ��� ��'���� ��� ��������: " objType))
                                 (setq intersectionProcessed T) ; ���������, ��� ����� � �����
                                )
                              );; cond
                            ) ; progn - ���������� ���� � �������/������������
                          ) ; if - �������� ������� ������� �������
                        ) ; if - ������� ��������� ������� �������
                      ) ; progn - ��������� ������� �������
                    ) ; if - ������� ��������� ���������

                  ) ; progn - ������� ����� ��������
                  (princ "\n    ����� ����� �������� �������.")
                ) ; if length > 0
             ) ; progn - �������� ���� variant
             (princ "\n    ������� �� �������� ��� ��������� ����������� ��� �����.")
          ) ; if - �������� ���� variant/�������
        ) ; if - ������� ������ IntersectWith
      ) ; progn - ������� ���������
    ) ; if - ���������� ��� ��'���
  ) ; while

  ;; --- ���������� ---
  (if (not foundIntersection)
      (princ "\n�� �������� �������� �� �������� ��'����� �� �����/�������� �������.")
      (if intersectionProcessed
          (princ (strcat "\n������� �������� ��������� ��� ��'���� ����: " objType))
          (princ "\n�������� �������, ��� ������� ������� ��� ������� �������/����.")
      )
  )

  (setvar "CMDECHO" oldCmdEcho)
  (setvar "OSMODE" oldOsmode)
  (vla-EndUndoMark *acadDoc*)

  (setq *acadDoc* nil)
  (princ) ; ����� �����
)

;; --- ʳ���� ������� ---
(princ (strcat "\nLISP ������ AlignPerp �����������. �������� ����в�� ����, ˲Ͳ� ��� ��˲˲Ͳ�, ���� ������ ALIGNPERP ��� �������."))
(princ)