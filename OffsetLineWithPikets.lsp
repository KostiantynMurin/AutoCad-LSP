;; --- ���Ѳ�: �������-��-������� (XY) + ����� ������� + ���������� Z ����� + ��˲� �̲����� --- ;; *** �������� ���� ***
;; �����������:
;; 1. ���� ������� ˳�� (LINE) ��� ����� ������ (LWPOLYLINE) �� ��������� ������� OFFSET.
;; 2. �������� ����������� ������� ������� �������.
;; 3. ��������� ����� "PIKET", �� ���������� �� �������� ��ղ��ί �����,
;;    ��������� Ҳ���� X �� Y ���������� (� �������� vertexTol).
;; 4. ������� ������� ����� �� XY-���������� ��������� ������ �̲���ί �����.
;;    ��� ����� Z-���������� ����� ���в������� (�� ��������� �� Z ������ �����). ;; *** �������� ���� ***
;; 5. ����� �������� �̲���� ���/������ � ���� RGB(39, 118, 187).
;;
;; ������: OFFSETLINEWITHPIKETS ��� OLWP

(vl-load-com) ; ������������, �� VLISP ������� �������

;; --- �������� ������� ��� ��������� ����� X �� Y ��������� � ����� ������� ---
(defun points-equal-2d (pt1 pt2 tol)
  (if (and pt1 pt2 (= (length pt1) 3) (= (length pt2) 3) (> tol 0.0)) ; �������� ������� �����
      (and
        (< (abs (- (car pt1) (car pt2))) tol)   ; �������� X
        (< (abs (- (cadr pt1) (cadr pt2))) tol) ; �������� Y
        ;; Z ����������
      )
      (progn (princ "\n������� � points-equal-2d: ����� ���������.") nil) ; ��������� nil � ��� �������
  )
)


;; ============================================================
;; == ������� ����ֲ� ==
;; ============================================================
(defun c:OffsetLineWithPikets ( / *error* oldEcho oldOsmode oldCmdDia oldOsmodeZ ; ������ oldOsmodeZ
                                vertexTol selEnt entData entType curveObj ssAllPikets i blkEnt blkData blkPtList_orig ssPiketsOnVertices idx vertexPt origCoords offsetDist offsetObjList newCurveObj offsetCoords targetVertexPt moveData vertCountMatch newCurveEnt entTypeExpected foundMatch colorObj acadObj acadDoc errorResult targetR targetG targetB trueColorValue newCurveData finalTargetPt ; ������ ����
                               )
  ;; --- �������� ������� ������� ������� ��� ���������� ����������� ---
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if oldCmdDia (setvar "CMDDIA" oldCmdDia)) ; ³������� ������
    (if oldOsmodeZ (setvar "OSNAPZ" oldOsmodeZ)) ; *** ²������� OSNAPZ ***
    (if ssPiketsOnVertices (sssetfirst nil nil)) ; ����� ��������
    (princ "\n*** ������� LISP ��� ���������: ")
    (if msg (princ msg))
    (princ " ***")
    (princ)
  )

;; --- �������� ������� ��� �������� ��������� � Line �� LWPolyline (����������) ---
  (defun ParseRawCoords (obj / ptList currentPt n rawCoords coordCount isLWPoly isLine elevation objName objNameResult) ; ������ isLine
    (setq ptList nil
          ; �������� ��'� ��'���� ��������
          objNameResult (vl-catch-all-apply 'vla-get-ObjectName (list obj))
    )
    ; ���������, �� ���� ������� ��� �������� ����
    (if (vl-catch-all-error-p objNameResult)
        (progn (princ "\n������� ��������� ���� ��'���� � ParseRawCoords.") nil) ; ���� ���� �������
        ; ���� ������� �� ����
        (progn
          ; *** �����������: �������� ��������� ������� ***
          (setq objName objNameResult) ; objNameResult ������ ����� ���� "AcDbPolyline"

          ; ����� ������������� ��������� ��'� objName
          (setq isLWPoly (= objName "AcDbPolyline")
                isLine (= objName "AcDbLine") ; *** ������ �������� �� LINE ***
                ; �������� ���������� ��������
                rawCoords (vl-catch-all-apply 'vla-get-Coordinates (list obj))
          )
          ; ���������, �� ���� ������� ��� �������� ���������
          (if (or (vl-catch-all-error-p rawCoords) (null rawCoords))
              (progn (princ "\n������� ��������� ��������� � ParseRawCoords.") nil)
              ; ���� ���������� �������� ������
              (progn
                (setq rawCoords (vlax-safearray->list (vlax-variant-value rawCoords))
                      coordCount (length rawCoords)
                      n 0
                )
                (cond
                  ; ������� LWPOLYLINE
                  ((and isLWPoly (> coordCount 0))
                   (setq elevation (vla-get-Elevation obj))
                   (while (< n coordCount)
                     (setq currentPt (list (nth n rawCoords) (nth (1+ n) rawCoords) elevation))
                     (setq ptList (cons currentPt ptList))
                     (setq n (+ n 2))
                   )
                  )
                  ; ������� LINE
                  ((and isLine (> coordCount 0))
                   (while (< n coordCount)
                     (setq currentPt (list (nth n rawCoords) (nth (1+ n) rawCoords) (nth (+ n 2) rawCoords)))
                     (setq ptList (cons currentPt ptList))
                     (setq n (+ n 3))
                   )
                  )
                  ; ���� ���� (���� ������ ���������� �������� ��� �����)
                  (T (princ (strcat "\n�����: �������������� ��� ��'���� '" objName "' ��� ParseRawCoords")))
                )
                (reverse ptList) ; ��������� ����� � ����������� �������
              )
          )
        )
    )
  ) ; ����� ParseRawCoords

  ;; --- ������� ������� ����� ---

  ;; ============================================================
  ;; == ������������ �������в� ������� ==
  ;; ============================================================
  ;; -- ֳ������ ���� ��� ������ �� (RGB) --
  (setq targetR 39)  ; �������� ��������� (0-255)
  (setq targetG 118) ; ������� ��������� (0-255)
  (setq targetB 187) ; ���� ��������� (0-255)

  ;; -- ������ ��� ��������� XY ��������� ����� � �������� --
  (setq vertexTol 0.01)
  ;; ============================================================

  (princ (strcat "\nֳ������ ���� ��� ������ ��: RGB(" (itoa targetR) "," (itoa targetG) "," (itoa targetB) ")"))
  (princ (strcat "\n����������� XY-������ ������ ����� �� �������� (vertexTol): " (rtos vertexTol 2 8)))

  ;; �������� ������ ������������
  (setq oldEcho (getvar "CMDECHO"))
  (setq oldOsmode (getvar "OSMODE"))
  (setq oldCmdDia (getvar "CMDDIA"))
  (setq oldOsmodeZ (getvar "OSNAPZ")) ; *** �������� �������� OSNAPZ ***
  (setvar "CMDECHO" 0) ; �������� ��� ������
  (setvar "CMDDIA" 0) ; �������� ������ (��� OFFSET + pause)
  (setvar "OSNAPZ" 0) ; *** ��������� ������������ OSNAPZ � 0, ��� �����������, �� ������� MOVE ������� � 3D �������, �� �� �� �������� ***
                     ; *** ���� ����� *����* �������������� Z-���������� ***

  ;; --- 1. ���� ��/����� ---
  (setq selEnt nil)
  (while (not selEnt)
    (setq selEnt (entsel "\n������! ������ ��� ��� ������ ��� �������: "))
    (if selEnt
      (progn
        (setq entData (entget (car selEnt))
              entType (cdr (assoc 0 entData)))
        (if (or (= entType "LINE") (= entType "LWPOLYLINE"))
          (setq curveObj (vlax-ename->vla-object (car selEnt))) ; �������� VLA ��'���
          (progn
            (princ "\n�������� ��'��� �� � ��� ��� ������. ��������� �� ���.")
            (setq selEnt nil) ; ������� ����, ��� ���� �����������
          )
        )
      )
      (*error* "���� ��������� ������������") ; �����, ���� ���������� �������� Esc
    )
  )
  (princ (strcat "\n�������: " entType " <" (vl-princ-to-string (car selEnt)) ">"))

  ;; --- 2. ����� ����� "PIKET" �� �������� ����� (��������� XY) ---
  (setq ssPiketsOnVertices (ssadd)) ; ����������� ���������� ������ ��� ��������� �����
  (setq moveData nil)              ; ����������� ������ ��� ����� ����������
  (setq ssAllPikets (ssget "_X" '((0 . "INSERT") (2 . "PIKET")))) ; ������ �Ѳ ����� PIKET

  (setq origCoords (ParseRawCoords curveObj)) ; �������� ���������� ������ ��ղ��ί �����

  (if (null origCoords)
      (*error* (strcat "�� ������� �������� ���������� ������ ��� ��'���� " entType))
  )
  (princ (strcat "\n�������� " (itoa (length origCoords)) " ������ � ������� �����."))

  (if ssAllPikets
    (progn
      (princ (strcat "\n�������� " (itoa (sslength ssAllPikets)) " ����� 'PIKET'. �������� ��������� XY �� �������� (� �������� " (rtos vertexTol 2 8) ")..."))
      (setq i 0)
      (repeat (sslength ssAllPikets)
        (setq blkEnt (ssname ssAllPikets i))
        (setq blkData (entget blkEnt))
        (setq blkPtList_orig (cdr (assoc 10 blkData))) ; ���������� 3D ����� ������� �����

        (if blkPtList_orig ; ��������, �� ������� �������� ����� �������
          (progn
            (setq idx 0 foundMatch nil)
            ;; ���� �� ��� �������� ������� �����
            (while (and (< idx (length origCoords)) (not foundMatch))
              (setq vertexPt (nth idx origCoords)) ; 3D ���������� ������� �������

              ;; ----> ��� ������� ��в������ <----
              ;; ������������� points-equal-2d ��� ��������� ����� X �� Y
              (if (points-equal-2d blkPtList_orig vertexPt vertexTol)
                (progn
                  (princ (strcat "\n  -> [�������� XY] ���� <" (vl-princ-to-string blkEnt) "> �� ������ #" (itoa idx) " � OrigZ=" (rtos (caddr blkPtList_orig))))
                  (ssadd blkEnt ssPiketsOnVertices) ; ������ ���� �� ������ ���������
                  ;; �������� ��'� �����, ���� ���ò������ 3D �����, �� ������ �������
                  (setq moveData (cons (list blkEnt blkPtList_orig idx) moveData))
                  (setq foundMatch T) ; ���������, �� �������� ����������, � ������� �� ���������� �����
                )
              ) ; ����� if ���������
              (setq idx (1+ idx)) ; ��������� ������ �������
            ) ; ����� while �� ��������
          )
          (princ (strcat "\n�����: �� ������� �������� ����� ������� ��� ����� <" (vl-princ-to-string blkEnt) ">"))
        )
        (setq i (1+ i)) ; ��������� ���� � ssAllPikets
      ) ; ����� repeat �� ������
      (setq moveData (reverse moveData)) ; ³������� ������� ���������
      (princ (strcat "\n�������� " (itoa (sslength ssPiketsOnVertices)) " ����� ��/��� XY ������ (� �������� " (rtos vertexTol 2 8) ")."))
    )
    (princ "\n����� � ������ 'PIKET' �� ������� � ��������.")
  )

  ;; --- 3. �������� ��������� ����� �� ����� ������ ������� ---
  (if (> (sslength ssPiketsOnVertices) 0)
    (progn
      (princ (strcat "\n�������� ��������� �� �������� ����� (" (itoa (sslength ssPiketsOnVertices)) " ��.)."))
      (sssetfirst nil ssPiketsOnVertices) ; ������� ������� �����

      (setq offsetDist (getdist "\n������! ����� ������� �������: "))

      (if offsetDist
        (progn
          ;; --- 4. ��������� ������� ����� �� ��������� ������� OFFSET ---
          (princ "\n��������� ������� �� ��������� ������� OFFSET...")
          (setvar "CMDECHO" 0) ; ������������, �� ��� ��������
          (setq entTypeExpected entType) ; �����'����� ���������� ��� ��'����

          ;; ��������� ������� OFFSET, pause �������� ����������� ������� �������
          (command "_offset" offsetDist (car selEnt) pause "")

          (setq newCurveEnt (entlast)) ; �������� �����Ͳ� ��������� ��'���

          ;; --- �������� �� ��������� ������ ��'���� ---
          (if (or (null newCurveEnt)
                  ;; ����������, �� ��� ���������� ��'���� ������� �����������
                  (/= (cdr (assoc 0 (entget newCurveEnt))) entTypeExpected)
                  ;; ����������, �� �� ��� ����� ��'��� (������� �� �������)
                  (= (car selEnt) newCurveEnt)
              )
            (progn
              (princ "\n*** �������: �� ������� �������� ��������� ������� ��'��� ���� ������� OFFSET.")
              (princ "\n    �������, ������� ���� ���������, ������� �������, ��� 'entlast' ������ �� �� ��� ��'���.")
              (*error* "������� ��������� ���������� �������")
            )
            ;; ���� ����� ��'��� ���������
            (progn
              ;; �������� VLA-��'��� ���� ������ �����
              (setq newCurveObj (vlax-ename->vla-object newCurveEnt))
              (princ (strcat "\n[OK] ������� �������� OFFSET ��������. ����� ��'���: <" (vl-princ-to-string newCurveEnt) ">"))

              ;; ==================================================================
              ;; ---> �������: ��� �̲�� ������� �̲���ί ˲Ͳ� <---
              ;; ==================================================================
              (princ "\n���� ������������ �������� ��'���� ����� entmod (DXF)...")
              (if newCurveEnt
                  (progn
                    (setq newCurveData (entget newCurveEnt)) ; �������� DXF ���

                    (if newCurveData
                      (progn
                        ;; --- ���������� �������� TrueColor � RGB ������ ---
                        (setq trueColorValue (+ (* targetR 65536) (* targetG 256) targetB))

                        ;; --- 1. ������������ True Color RGB ---
                        (princ (strcat "\n  ������������ TrueColor (DXF 420) �� " (itoa trueColorValue) "..."))
                        (if (assoc 420 newCurveData)
                          (setq newCurveData (subst (cons 420 trueColorValue) (assoc 420 newCurveData) newCurveData))
                          (setq newCurveData (append newCurveData (list (cons 420 trueColorValue))))
                        )

                        ;; --- 2. ������������ ��������� ������� � ByLayer (DXF 62 = 256) ---
                        (princ "\n  ������������ Color (DXF 62) �� ByLayer (256)...")
                        (if (assoc 62 newCurveData)
                          (setq newCurveData (subst (cons 62 256) (assoc 62 newCurveData) newCurveData))
                          (setq newCurveData (append newCurveData (list (cons 62 256))))
                        )

                        ;; --- 3. ������������ ��� ---
                        (if (entmod newCurveData)
                          (princ " [OK]")
                          (princ "\n*** �������: entmod �� ������� ������� ��'���.")
                        )
                      )
                      (princ "\n*** �������: �� ������� �������� DXF ��� (entget) ��� �������� ��'����.")
                    )
                  )
                  (princ "\n*** �����: ���� ���� ��'���� (newCurveEnt) ��� ���� ������������.")
              )
              ;; ==================================================================
              ;; ---> ʲ����: ��� �̲�� ������� �̲���ί ˲Ͳ� <---
              ;; ==================================================================

              ;; �������� �� ������� ���������� ������ �̲���ί �����
              (setq offsetCoords (ParseRawCoords newCurveObj))
              (if (null offsetCoords)
                  (*error* "�� ������� �������� ���������� ������ ������ �����!")
              )
              (princ (strcat "\n�������� " (itoa (length offsetCoords)) " ������ � ������ �����."))

              ;; �������� ���������� ������� ������
              (setq vertCountMatch (= (length origCoords) (length offsetCoords)))
              (if (not vertCountMatch)
                  (princ "\n!!! ������������: ʳ������ ������ � ������� �� ������ ������ �� �������! ��������� ���������� ����� ���� ���� ��������.")
              )

              ;; --- 5. ���������� ����� �� ������� ������� (Z ���� ���������) --- ;; *** �������� �������� ***
              (princ "\n���������� ����� �� XY ��������� ������ ������ �� (���в����� Z)...") ; *** �������� ��²�������� ***
              (foreach dataItem moveData
                  (setq blkEnt (nth 0 dataItem))
                  (setq blkPtList_orig (nth 1 dataItem)) ; ���������� 3D ����� ����� (Xo, Yo, Zo)
                  (setq idx (nth 2 dataItem))           ; ������ �������� �������

                  (princ (strcat "\n  ������� ����� <" (vl-princ-to-string blkEnt) "> � ������� #" (itoa idx)))
                  (princ (strcat "\n    ���������� ����� �����: " (vl-princ-to-string blkPtList_orig)))

                  (if (and blkPtList_orig (< idx (length offsetCoords))) ; �������� �� ������ ������ � �� � ���������� �����
                    (progn
                      ;; �������� 3D ����� �������� ������� �� ������ �����
                      (setq targetVertexPt (nth idx offsetCoords)) ; �� ����� (X_new, Y_new, Z_new_line_elev)
                      (princ (strcat "\n    ������� ���. ����� #" (itoa idx) ": " (vl-princ-to-string targetVertexPt)))

                      ;; ----> ������������� Բ�����ί ֲ����ί ����� ǲ ���������� Z <----
                      (setq finalTargetPt (list
                                            (car targetVertexPt)     ; X � ���� �������
                                            (cadr targetVertexPt)    ; Y � ���� �������
                                            (caddr blkPtList_orig) ; Z � ���ò�����ί ����� �����
                                          )
                      )
                      (princ (strcat "\n    Գ������ ������� ����� (Xn, Yn, Zo): " (vl-princ-to-string finalTargetPt)))


                      ;; ----> �����Ӫ�� ����̲����� �� �������������� ����� <----
                      ;; ���������� � ���������� ����� ����� ����� �� ���� ����� (Xn, Yn, Zo).
                      ;; Z ���������� ����� �����������, �� �� ������� �� � finalTargetPt.
                      (command "_move" blkEnt "" "_none" blkPtList_orig "_none" finalTargetPt)
                      (princ "\n    [INFO] ������� MOVE �������� (��������� Z �����).") ; *** �������� ��²�������� ***
                    )
                    (princ (strcat "\n    �������: ������ ������� #" (itoa idx) " �������� ��� ������ ����� (����: " (itoa (1- (length offsetCoords))) ") ��� ������� ���������� ����� �����. ���� �� ���������."))
                  )
              ) ; ����� foreach
              (princ "\n[OK] ������ ���������� ����� �� ������� � ����������� Z ���������.") ; *** �������� ��²�������� ***
            ) ; ����� progn �������� �������
          ) ; ����� if �������� entlast
        )
        (princ "\n³������ �� �������. �������� ���������.")
      ) ; ����� if offsetDist
    )
    (princ "\n�� �������� ����� 'PIKET' �� XY-����������� ������ ������� ����� (� �������� " (rtos vertexTol 2 8) "). ͳ���� ��������.")
  ) ; ����� if > 0 ��������� �����

  ;; --- 6. �������� �� ���������� ����������� ---
  (if oldEcho (setvar "CMDECHO" oldEcho))
  (if oldOsmode (setvar "OSMODE" oldOsmode))
  (if oldCmdDia (setvar "CMDDIA" oldCmdDia)) ; ³������� ������
  (if oldOsmodeZ (setvar "OSNAPZ" oldOsmodeZ)) ; *** ²������� ���������� OSNAPZ ***
  (sssetfirst nil nil) ; ����� ��������
  (princ "\n������ ���������.")
  (princ) ; ����� �����
) ; ����� defun c:OffsetLineWithPikets

;; ============================================================
;; == ���������� ������� ������� ��� ������� ==
;; ============================================================
(defun c:OLWP () (c:OffsetLineWithPikets)) ;; OLWP - Offset Line With Pikets

(princ "\nLISP 'OffsetLineWithPikets' (XY �������-��-������� + ���� ������� + ���������� Z ����� + ���� �������) �����������.") ; *** �������� ���� ***
(princ "\n��� ������� ������ OLWP ��� OFFSETLINEWITHPIKETS.")
(princ)