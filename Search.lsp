;; --- LISP ������ ��� ������ ����� �� �������� ������� � ������ (v2) ---
;; --- ������ ������� CHECKPOINTS (v3) ---

;; ��������� ����� ��� ��������� ���������� ������
(setq *g_last_search_result* nil)

;; --- �������� �������: ����� ��� �������� ������� (������� �� �������) ---
;; ������ �� ��������� ����� 'find' �� ����� 'replace' � ����� 'source'.
;; ������� ����� ����� � ������.
(defun str-replace (find replace source / len-f pos result rest)
  (setq len-f (strlen find))
  (if (= 0 len-f) ; ���� ������� ����� �������, ����� �� ������
      source
      (progn
        (setq result "" rest source)
        (while (setq pos (vl-string-search find rest 0)) ; ������ find � ������� rest
          (setq result (strcat result (substr rest 1 pos) replace)) ; ������ ������� �� �������� + �����
          (setq rest (substr rest (+ pos len-f 1))) ; ������� ������� (���� ���������� �����)
        )
        (strcat result rest) ; ������ �������, �� ����� ���� �������� find
      )
  )
)

;; ====================================================================
;; ������ 1: ����� �� ��Ĳ����� ���ʲ� (v4 - ����� �� �������� �����)
;; ====================================================================
;; �������: SEARCH
;; ���� ����� "PIKET" �� ��������� "������", ��Ĳ�ߪ ��, ���� ��������
;; �������� ����������� � ��������� �����,
;; �� ���в��� ���� ������ � ��������� ����� *g_last_search_result*.

(defun c:SEARCH ( / keyword ssAll i ename edata bname hasAttribs
                  attEname attEdata attTag attValue ssFound foundCount )
  ;; --- �������� �������� ����������� ---
  (setq keyword (getstring T "\n������ ������� �������� �������� '������' ��� ������: ")) ; ������ �������

  ;; ������� ��������� ��������� ����� ����� �������
  (setq *g_last_search_result* nil)

  ;; --- ���������, �� ������ ������� ����� ---
  (if (and keyword (/= "" keyword))
    (progn ;; ����������, ����� ���� ������� ����� �� ������
      (princ (strcat "\n����� ����� 'PIKET', �� ������� '������' ���������� � '" keyword "'...")) ; ������ �����������

      ;; --- ����������� ---
      (setq ssFound (ssadd)) ;; �������� ������� ���� ������ ��� ����������
      (setq foundCount 0)    ;; ������������ �������� ���������

      ;; --- �������� �� ��'���� ---
      (setq ssAll (ssget "_X"))

      ;; --- ����� ��������� ����� ---
      (if ssAll
        (progn
          (setq i 0)
          (repeat (sslength ssAll)
            (setq ename (ssname ssAll i)) ;; �������� ��'� ��'����
            (if (not (null (entget ename))) ; �������� ��������� ��'����
              (progn
                (setq edata (entget ename))   ;; �������� ��� ��'���� (DXF ����)
                (if (eq "INSERT" (cdr (assoc 0 edata)))
                  (progn
                    (setq bname (strcase (cdr (assoc 2 edata)))) ;; ����� �����
                    (if (eq "PIKET" bname)
                      (progn
                        (setq hasAttribs (assoc 66 edata))
                        (if (and hasAttribs (= 1 (cdr hasAttribs)))
                          (progn
                            (setq attEname (entnext ename)) ;; ������ �������
                            (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                              (setq attTag (strcase (cdr (assoc 2 attEdata)))) ;; ���
                              (setq attValue (cdr (assoc 1 attEdata)))       ;; ��������
                              (if (and (eq "������" attTag)
                                       attValue
                                       ;; ***** ������� �̲�: ������������ wcmatch *****
                                       ;; ����������, �� �������� �������� (� ��������� ������)
                                       ;; ������� ������� "������������*" (����� � ��������� ������)
                                       (wcmatch (strcase attValue) (strcat (strcase keyword) "*"))
                                       ;; ***** ʲ���� �̲� *****
                                  )
                                (progn
                                  (ssadd ename ssFound) ;; ������ ���� �� ������
                                  (setq foundCount (1+ foundCount))
                                  (setq attEname nil) ; �������� ���� while ��� ��������� �����
                                )
                              )
                              (if attEname (setq attEname (entnext attEname))) ;; ��������� �������
                            ) ;; ����� while
                          ) ;; ����� progn (�� ��������)
                        ) ;; ����� if (�� ��������)
                      ) ;; ����� progn (��'� PIKET)
                    ) ;; ����� if (��'� PIKET)
                  ) ;; ����� progn (�� INSERT)
                ) ;; ����� if (�� INSERT)
              ) ; ����� progn (��'��� ����)
            ) ; ����� if (��'��� ����)
            (setq i (1+ i)) ;; ��������� ��'���
          ) ;; ����� repeat
        ) ;; ����� progn (ssAll ����)
        (princ "\n� �������� ���� ��'���� ��� ������.")
      ) ;; ����� if (ssAll ����)

      ;; --- Գ��������: ��������, ��� �� ���������� ���������� ---
      (if (> foundCount 0)
        (progn
          ;; �������� ��������� ���� ������ � ��������� �����
          (setq *g_last_search_result* ssFound)
          ;; ��Ĳ���� ������� ��'���� ��� �����������
          (sssetfirst nil ssFound)
          (princ (strcat "\n�������� �� ��Ĳ���� " (itoa foundCount) " �����. ��������� ��������� ��� ������� CHECKPOINTS, PASTEHERE ��� REPLACENAME."))
        )
        (progn ;; ���� ����� �� ��������
           (setq *g_last_search_result* nil) ; ������������, �� ��������� �������
           (princ (strcat "\n����� 'PIKET', �� ������� '������' ���������� � '" keyword "', �� ��������.")) ; ������ �����������
        )
      )
    ) ;; ����� progn (������� ����� ������)
    (princ "\n������� ����� �� �������. ����� ���������.") ;; �����������, ���� ������� ����� ������
  ) ;; ����� if (�������� ��������� �����)
  (princ) ;; ������ �����
) ;; ����� defun c:SEARCH


;; ====================================================================
;; ������ 2: ������� � ����� ����������� ���������� ������
;; ====================================================================
;; �������: PASTEHERE
;; ���� ���� ������, ���������� �������� SEARCH � ����� *g_last_search_result*,
;; � �������� � ����� ������� ������� ��'���� � ����� ������
;; ��'���(�) � ������ ����� (�� ���� ��������� �� ��������� COPYBASE).
;; (��� ���� ������� ���������� ��� ���)
(defun c:PASTEHERE ( / *error* ss i ename edata ipoint oldCmdEcho oldOsmode pasteCount )

  ;; --- ������� ������� ������� ---
  (defun *error* (msg)
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho)) ; ³������� CMDECHO
    (if oldOsmode (setvar "OSMODE" oldOsmode))   ; ³������� OSMODE
    (command "_.UNDO" "_End")                    ; ��������� ���������� UNDO
    (cond ((not msg))                            ; ����� ��� ����������� (���������, ESC)
          ((vl-string-search "Function cancelled" msg)) ; ���������� ��������
          ((vl-string-search "quit / exit abort" msg))  ; ���������� ��������
          (T (princ (strcat "\n�������: " msg)))        ; ���� �������
    )
    (setq *error* nil) ; ������� �������� �������
    (princ)
  )

  ;; --- �������� ������ ����� ---
  (if (= 0 (getvar "CLIPROPS")) ; ��������, �� ������ ����� ��� AutoCAD
    (progn
      (alert "����� ����� ������� ��� �� ������ ����� AutoCAD.\n�������� �������� ��'���(�) �� ��������� '�������� � ������� ������' (_COPYBASE).")
      (exit) ; ����� � �������
    )
  )

  ;; --- �������� �������� �� ������� ���������� ���������� ������ ---
  (if (and (boundp '*g_last_search_result*) ; �� ����� ����?
           *g_last_search_result* ; �� ���� �� nil?
           (= 'PICKSET (type *g_last_search_result*)) ; �� �� ����� ���� ������?
           (> (sslength *g_last_search_result*) 0) ; �� ���� �� �������?
      )
    (progn ;; � ���� ���������� ������
      (setq ss *g_last_search_result*) ;; ����������� ���������� ���� ������
      (princ (strcat "\n��������������� ���������� ��������� ������ � " (itoa (sslength ss)) " ��'����."))
      (princ "\n���������� ��'����(��) � ������ ����� � ����� �� �������...")

      ;; --- ������������ ���������� �� UNDO ---
      (setq oldCmdEcho (getvar "CMDECHO"))
      (setq oldOsmode (getvar "OSMODE"))
      (setvar "CMDECHO" 0) ;; �������� ��� ������
      (setvar "OSMODE" 0)  ;; �������� ��'����� ����'����
      (command "_.UNDO" "_Begin") ;; ������ ���������� UNDO
      (setq pasteCount 0) ;; ˳������� ������� �������

      ;; --- ���� �� ���������� ��'����� �� ������� ---
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i)) ;; �������� ��'� ��'���� � ������ ������
        (if (setq edata (entget ename)) ; ���������, �� ��'��� �� ����
          (progn
            ;; ��������, �� �� ���� (INSERT)
            (if (eq "INSERT" (cdr (assoc 0 edata)))
              (progn
                (setq ipoint (cdr (assoc 10 edata))) ;; �������� ����� ������� �����
                (if ipoint
                  (progn
                    (command "_.PASTECLIP" ipoint) ;; �������� � ������ � ����� �������
                    (setq pasteCount (1+ pasteCount)) ;; �������� ��������
                  )
                  ;; ������������, ���� �� ������� �������� ����� �������
                  (princ (strcat "\n ������������: �� ������� �������� ����� ������� ��� ��'����: " (vl-princ-to-string ename)))
                )
              )
              ;; ������������, ���� ��'��� � ������ - �� ����
              (princ (strcat "\n ������������: ��'��� � ��������� ������ �� � ������ (INSERT): " (vl-princ-to-string ename)))
            )
          )
          ;; ������������, ���� ��'��� � ������ ��� �� ����
          (princ (strcat "\n ������������: ��'��� � ��������� ������ ��� �� ����: " (vl-princ-to-string ename)))
        )
        (setq i (1+ i)) ;; ��������� ��'��� � ������
      ) ;; ����� ����� repeat

      ;; --- ³��������� ���������� �� ���������� UNDO ---
      (setvar "CMDECHO" oldCmdEcho)
      (setvar "OSMODE" oldOsmode)
      (command "_.UNDO" "_End") ;; ��������� ���������� UNDO

      ;; --- Գ������ ����������� ---
      (princ (strcat "\n���������. ��'���(�) � ������ ����� ��������� " (itoa pasteCount) " ���(��)."))

    )
    ;; --- ���� ���������� ������ ���� ��� ���� ������ ---
    (princ "\n�� �������� ������ ���������� ������������ ������. �������� ��������� ������� SEARCH.")
  ) ;; ����� if (�������� *g_last_search_result*)

  ;; --- ������� �� ����� ---
  (setq *error* nil) ; ������� �������� �������
  (princ) ;; ������ �����
) ;; ����� defun c:PASTEHERE

;; ====================================================================
;; ������ 3: ����²��� �� ����������� ���������� Z �� ��������� "�������"
;; ====================================================================
;; �������: CHECKPOINTS
;; ���� ���� ������, ���������� �������� SEARCH � ����� *g_last_search_result*.
;; ��� ������� ����� "PIKET" � ����� ������� �������� �������� "�������"
;; � ����������� Z ����� ������� �����.
;; �������� ��� ��� ���������, ��Ĳ�ߪ ����� ����� � ������������
;; �� ������ ����� �� ����������� Z ����������.

(defun c:CHECKPOINTS ( / *error* ss totalCount i ename edata ipoint zCoord
                       attEname attEdata attTag otmetkaStr otmetkaNum
                       diffList diffCount fuzz modCount answer oldCmdecho
                       ssDiff ; <--- ������ �������� ����� ��� ������ ������ ������
                     )

  ;; --- ������� ������� ������� ---
  (defun *error* (msg)
    ;; ³������� CMDECHO, ���� ���� ���� ������
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    ;; ���� ����� UNDO ���� ���������, ��� �� ��������� ��������
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command "_.UNDO" "_End"))
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\n�������: " msg)))
    )
    (setq *error* nil)
    (princ)
  )

  ;; --- ����������� ---
  (setq diffList nil         ; ������ ����� � ������������ [(ename . (otmetkaStr . zCoord))]
        diffCount 0        ; ˳������� ����� � ������������
        fuzz 1e-6          ; ������ ��� ��������� ������ �����
        modCount 0         ; ˳������� ������� �����
        oldCmdecho (getvar "CMDECHO")
  )
  (setvar "CMDECHO" 0) ; ��������� �������� ��� ������

  ;; --- �������� �������� �� ������� ���������� ���������� ������ ---
  (if (and (boundp '*g_last_search_result*)
           *g_last_search_result*
           (= 'PICKSET (type *g_last_search_result*))
           (> (sslength *g_last_search_result*) 0)
      )
    (progn ;; � ���� ���������� ������
      (setq ss *g_last_search_result*)
      (setq totalCount (sslength ss))
      (princ (strcat "\n�������� " (itoa totalCount) " ����� � ����������� ���������� ������..."))

      ;; --- ���� �� ���������� ��'����� ---
      (setq i 0)
      (repeat totalCount
        (setq ename (ssname ss i))
        (if (setq edata (entget ename)) ; ���������, �� ��'��� �� ����
          (if (eq "INSERT" (cdr (assoc 0 edata))) ; ���������, �� �� ����
            (progn
              (setq ipoint (cdr (assoc 10 edata))) ; �������� ����� �������
              (setq zCoord (caddr ipoint))       ; �������� Z ����������
              (setq otmetkaStr nil)              ; ������� �������� �������� ��� ��������� �����

              ;; --- ����� �������� "�������" ---
              (if (and (assoc 66 edata) (= 1 (cdr (assoc 66 edata)))) ; �� � ��������?
                (progn
                  (setq attEname (entnext ename))
                  (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                    (setq attTag (strcase (cdr (assoc 2 attEdata))))
                    (if (eq "�������" attTag)
                      (progn
                        (setq otmetkaStr (cdr (assoc 1 attEdata))) ; �������� �������� ��������
                        (setq attEname nil) ; �������� ����� �������� ��� ����� �����
                      )
                      (setq attEname (entnext attEname)) ; ��������� �������
                    )
                  ) ; end while
                )
              ) ; end if has attributes

              ;; --- ��������� �������� �������� �� Z ���������� ---
              (if otmetkaStr
                (progn
                  (setq otmetkaNum (distof otmetkaStr)) ; ���������� ������������ ����� � �����
                  (if otmetkaNum ; ��������, �� ������� ������������
                    ;; �������� � ��������
                    (if (not (equal zCoord otmetkaNum fuzz))
                      (progn
                        ;; ������ ���������� ��� ���� �� ������ �����������
                        (setq diffList (cons (cons ename (cons otmetkaStr zCoord)) diffList))
                        (setq diffCount (1+ diffCount))
                      )
                    )
                    (princ (strcat "\n ������������: �� ������� ������������ �������� '" otmetkaStr "' �������� '�������' � ����� ��� �����: " (vl-princ-to-string ename)))
                  )
                )
                (princ (strcat "\n ������������: �� �������� ������� '�������' ��� �����: " (vl-princ-to-string ename)))
              )
            ) ; end progn (is insert)
            (princ (strcat "\n ������������: ��'��� � ��������� ������ �� � ������ (INSERT): " (vl-princ-to-string ename)))
          ) ; end if (is insert)
          (princ (strcat "\n ������������: ��'��� � ��������� ������ ��� �� ����: " (vl-princ-to-string ename)))
        ) ; end if (entget)
        (setq i (1+ i))
      ) ; end repeat

      ;; --- ��� ��� ���������� �������� �� ��Ĳ����� ���������� ---
      (if (= diffCount 0)
        (progn
           (princ (strcat "\n�������� ���������. �� " (itoa totalCount) " ����� ����� �������� ���������� Z �� �������� �������� '�������'."))
           ;; ����� ��������, ���� ����������� ����
           (sssetfirst nil nil)
        )
        (progn ;; �������� ���������
          (princ (strcat "\n�������� ���������. ������ ���������: " (itoa totalCount) " �����."))
          (princ (strcat "\n�������� ����������� � Z-��������� �� ������� '�������': " (itoa diffCount) " �����."))
          (princ "\n������ ����� � ������������ (������� '�������' | ���������� Z):")
          ;; ������� ������ ����� � ������������
          (foreach item diffList
            (princ (strcat "\n  - ���� <" (vl-princ-to-string (car item)) ">: '" (car (cdr item)) "' | " (rtos (cdr (cdr item)))))
          )

          ;; ***** ������� �̲�: ��������� �� �������� ������ ������ *****
          (setq ssDiff (ssadd)) ; �������� ������� ����
          (if diffList ; ��������, �� ������ �� ������� (���� diffCount > 0 ��� �� �������)
              (foreach item diffList
                 (if (entget (car item)) ; ��������� �������� ��������� ��'���� ����� ����������
                    (ssadd (car item) ssDiff) ; ������ ename ������� ����� � ������������
                 )
              )
          )
          (if (> (sslength ssDiff) 0)
             (progn
                (princ (strcat "\n������� " (itoa (sslength ssDiff)) " ����� � ������������."))
                (sssetfirst nil ssDiff) ; ������� ����� �� �����
             )
             (progn
                (princ "\n�� ������� �������� ���� ������ ��� �������� ����� � ������������.")
                (sssetfirst nil nil)    ; ���� � ����� ������� ssDiff �������, ����� ��������
             )
          )
          ;; ***** ʲ���� �̲� *****

          ;; --- ����� �� ����������� ---
          (initget "Yes No") ; ��������� ����� �� ������
          (setq answer (getkword "\n\n������ ���������� Z ��� ��� ����� �������� �� �������� '�������'? [Yes/No]: "))

          (if (eq answer "Yes")
            (progn
              ;; --- ��������� ��� ---
              (princ "\n������� ����...")
              ;; *** ��� ������ � UNDOCTL ��������, ����������� �� ������������ AutoCAD ***
              (command "_.UNDO" "_Begin") ; ������ ���������� UNDO

              (foreach item diffList
                 (setq ename (car item))
                 (setq otmetkaStr (car (cdr item)))
                 (if (setq edata (entget ename))
                    (progn
                      (setq ipoint (cdr (assoc 10 edata)))
                      (setq otmetkaNum (distof otmetkaStr))
                      (if otmetkaNum
                        (progn
                          (setq new_ipoint (list (car ipoint) (cadr ipoint) otmetkaNum))
                          (setq edata (subst (cons 10 new_ipoint) (assoc 10 edata) edata))
                          (if (entmod edata)
                            (setq modCount (1+ modCount))
                            (princ (strcat "\n ������� ���� Z ���������� ��� �����: " (vl-princ-to-string ename)))
                          )
                        )
                      )
                    )
                    (princ (strcat "\n �������: �� ������� �������� ��� ��� ����� " (vl-princ-to-string ename) " �� ��� ������ ����."))
                 )
              ) ; end foreach item

              (command "_.UNDO" "_End") ; ��������� ���������� UNDO
              (princ (strcat "\n������ ������ Z ���������� ��� " (itoa modCount) " � " (itoa diffCount) " �����."))
              ;; *** ��� ���������� UNDOCTL �������� ***

            ) ; end progn (answer = Yes)
            (princ "\n���� �� ������������.")
          ) ; end if (answer = Yes)
        ) ; end progn (diffCount > 0)
      ) ; end if (= diffCount 0)
    ) ; end progn (results are valid)
    ;; --- ���� ���������� ������ ���� ��� ���� ������ ---
    (princ "\n�� �������� ������ ���������� ������������ ������ (� ������� SEARCH). �������� ��������� ������� SEARCH.")
  ) ; end if (check *g_last_search_result*)

  ;; --- ³��������� ���������� �� ����� ---
  (setvar "CMDECHO" oldCmdecho) ; ³������� CMDECHO
  (setq *error* nil) ; ������� �������� �������
  (princ) ;; ������ �����
) ;; ����� defun c:CHECKPOINTS


;; ====================================================================
;; ������ 4: ��̲�� ϲ������� � ������Ҳ "������"
;; ====================================================================
;; �������: REPLACENAME
;; ���� ���� ������, ���������� �������� SEARCH � ����� *g_last_search_result*.
;; ������ �������� ��� ������ �� �������� ��� �����.
;; ������ �� ��������� ����� �������� �� ����� � �������
;; �������� "������" ��� ������� ����� � ������.

(defun c:REPLACENAME ( / *error* ss i ename edata
                       attEname attEdata attTag currentVal newVal
                       findStr replaceStr modCount oldCmdecho )

  ;; --- ������� ������� ������� ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho)) ; ³������� CMDECHO
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command "_.UNDO" "_End")) ; ��������� UNDO ���� �������
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\n�������: " msg)))
    )
    (setq *error* nil)
    (princ)
  )

  ;; --- ����������� ---
  (setq modCount 0) ; ˳������� ������� �����
  (setq oldCmdecho (getvar "CMDECHO"))
  ;(setvar "CMDECHO" 0) ; ����� �������� ���, ���� ������

  ;; --- �������� �������� �� ������� ���������� ���������� ������ ---
  (if (and (boundp '*g_last_search_result*)
           *g_last_search_result*
           (= 'PICKSET (type *g_last_search_result*))
           (> (sslength *g_last_search_result*) 0)
      )
    (progn ;; � ���� ���������� ������
      (setq ss *g_last_search_result*)
      (princ (strcat "\n���� ��������� " (itoa (sslength ss)) " ����� � ����������� ���������� ������."))

      ;; --- �������� ����� ��� ������ �� ����� ---
      (setq findStr (getstring T "\n������ ��������, ��� ������� � ������� '������': "))
      (if (= "" findStr)
          (progn
            (princ "\n�������: ϳ������� ��� ������ �� ���� ���� ���������. ����� ���������.")
            (exit)
          )
      )
      (setq replaceStr (getstring T "\n������ ��������, �� ��� �������: "))

      (princ (strcat "\n������� ����� '" findStr "' �� '" replaceStr "' � ��������� '������'..."))

      ;; --- ������� ����� UNDO ---
      (command "_.UNDO" "_Begin")

      ;; --- ���� �� ���������� ��'����� ---
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i))
        (if (setq edata (entget ename)) ; ���������, �� ��'��� �� ����
          (if (and (eq "INSERT" (cdr (assoc 0 edata))) ; ���������, �� �� ����
                   (assoc 66 edata) (= 1 (cdr (assoc 66 edata)))) ; � �� �� ��������
            (progn
              ;; --- ����� �������� "������" �� ���� ���� ---
              (setq attEname (entnext ename))
              (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                (setq attTag (strcase (cdr (assoc 2 attEdata)))) ; ��� (��� ��������)

                (if (eq "������" attTag)
                  (progn
                    (setq currentVal (cdr (assoc 1 attEdata))) ; ������� �������� ��������
                    ;; �������� ����� �� ��������� �������� �������
                    (setq newVal (str-replace findStr replaceStr currentVal))

                    ;; ���� �������� ��������, ������� �������
                    (if (not (eq currentVal newVal))
                      (progn
                        (setq attEdata (subst (cons 1 newVal) (assoc 1 attEdata) attEdata))
                        (if (entmod attEdata)
                            (setq modCount (1+ modCount)) ; �������� �������� ������� ���
                            (princ (strcat "\n ������� ��������� �������� ��� �����: " (vl-princ-to-string ename)))
                        )
                      )
                      ;(princ (strcat "\n � ����� " (vl-princ-to-string ename) " �������� '" findStr "' �� �������� � ������� '������'."))
                    )
                    (setq attEname nil) ; �������� ����� �������� ��� ����� �����
                  )
                  ;; ���� �� �� ������� "������", ������� �� ����������
                  (setq attEname (entnext attEname))
                ) ; ����� if (eq "������" attTag)
              ) ; ����� while (������ ��������)
            ) ; ����� progn (�� ���� � ����������)
          ) ; ����� if (�� ���� � ����������)
        ) ; ����� if (entget)
        (setq i (1+ i))
      ) ; ����� repeat

      ;; --- ���������� ����� UNDO ---
      (command "_.UNDO" "_End")

      ;; --- Գ������ ����������� ---
      (princ (strcat "\n���������. �������� ����� � ������� '������' ��� " (itoa modCount) " �����."))

    ) ; end progn (results are valid)
    ;; --- ���� ���������� ������ ���� ��� ���� ������ ---
    (princ "\n�� �������� ������ ���������� ������������ ������ (� ������� SEARCH). �������� ��������� ������� SEARCH.")
  ) ; end if (check *g_last_search_result*)

  ;; --- ³��������� ���������� �� ����� ---
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho)) ; ³������� CMDECHO, ���� ��������
  (setq *error* nil) ; ������� �������� �������
  (princ) ;; ������ �����
) ;; ����� defun c:REPLACENAME


;; --- ����������� ��� ������������ ---
(princ "\nLISP-������� (v3) �����������.")
(princ "\n�������:")
(princ "\n  SEARCH      - ����� ����� 'PIKET' �� ��������� '������', �������� �� ���������� ����������.")
(princ "\n  PASTEHERE   - ������� ��'���� � ������ (_COPYBASE) � ����� ����������� ���������� SEARCH.")
(princ "\n  CHECKPOINTS - �������� ���������� Z ���������� �� �������� '�������' � ������ � ����������� ���������� SEARCH, � ��������� �����������.")
(princ "\n  REPLACENAME - ���� �������� �������� ������ (���� ���� ������������� ����� �����)")
(princ) ;; ������ �����