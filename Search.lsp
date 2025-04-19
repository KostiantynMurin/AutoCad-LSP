;; --- LISP ������ ��� ������ ����� �� �������� �������/��������/�������������� (v5 - ���������� ������� ���� g_last_search_result) ---
;; --- ������ ������� CHECKPOINTS (v3) ---
;; --- ������ ������� REPLACENAME (v3) ---
;; --- ������������ PASTEHERE, CHECKPOINTS, REPLACENAME ��� ������ � ������ ������� (v4) ---
;; --- ������ ������� RENAME_OKM (v5) ---

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
;; ������ 1: ����� �� ��Ĳ����� ���ʲ� (v5 - ���������� �������� ���������� ��� ���������)
;; ====================================================================
;; �������: SEARCH
;; ���� ����� "PIKET" �� ��������� "������", ��Ĳ�ߪ ��, ���� ��������
;; �������� ����������� � ��������� �����,
;; �� ���в��� ���� ������ � ��������� ����� *g_last_search_result*.
;; ��� ��������� ���� ���� ������������ ��������� PASTEHERE, CHECKPOINTS, REPLACENAME, RENAME_OKM.

(defun c:SEARCH ( / keyword ssAll i ename edata bname hasAttribs
                  attEname attEdata attTag attValue ssFound foundCount *error* old_g_last_search) ; ������ *error* �� old_g_last_search

  ;; --- ������� ������� ������� ��� ���������� �������� ---
  (defun *error* (msg)
    (if (and old_g_last_search (not *g_last_search_result*)) ; ���� ����� ��� ����������/��������, � ������ ��������� �������
        (setq *g_last_search_result* nil) ; ���� �������, ���� ��������� ����� ESC
    )
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\n������� � SEARCH: " msg)))
    )
    (setq *error* nil) ; ������� �������� �������
    (princ)
  )

  ;; �������� �������� ���� �� ������� ���������� ����� ESC
  (setq old_g_last_search *g_last_search_result*)
  ;; ������� ��������� ��������� ����� ����� �������
  (setq *g_last_search_result* nil)

  ;; --- �������� �������� ����������� ---
  (setq keyword (getstring T "\n������ ������� �������� �������� '������' ��� ������: ")) ; ������ �������

  ;; --- ���������, �� ������ ������� ����� ---
  (if (and keyword (/= "" keyword))
    (progn ;; ����������, ����� ���� ������� ����� �� ������
      (princ (strcat "\n����� ����� 'PIKET', �� ������� '������' ���������� � '" keyword "'...")) ; ������ �����������

      ;; --- ����������� ---
      (setq ssFound (ssadd)) ;; �������� ������� ���� ������ ��� ����������
      (setq foundCount 0)      ;; ������������ �������� ���������

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
                              (setq attValue (cdr (assoc 1 attEdata)))      ;; ��������
                              (if (and (eq "������" attTag)
                                       attValue
                                       ;; ����������, �� �������� �������� (� ��������� ������)
                                       ;; ������� ������� "������������*" (����� � ��������� ������)
                                       (wcmatch (strcase attValue) (strcat (strcase keyword) "*"))
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
          (princ (strcat "\n�������� �� ��Ĳ���� " (itoa foundCount) " �����. ��������� ��������� ��� ��������� ������������ ��������� CHECKPOINTS, PASTEHERE, REPLACENAME ��� RENAME_OKM.")) ; �������� �����������
        )
        (progn ;; ���� ����� �� ��������
           ;; ������������, �� ��������� ������� (��� ������� �� �������, ��� ��� ������)
           (setq *g_last_search_result* nil)
           (princ (strcat "\n����� 'PIKET', �� ������� '������' ���������� � '" keyword "', �� ��������.")) ; ������ �����������
        )
      )
    ) ;; ����� progn (������� ����� ������)
    (progn ;; <-- �̲��: ���� ������� ����� �� ������ ��� ������ (��� ESC)
      (princ "\n������� ����� �� ������� ��� ����� ��������� (ESC).")
      (setq *g_last_search_result* nil) ; ���� ������� ���������, ���� ����� �� �������
    )
  ) ;; ����� if (�������� ��������� �����)
  (setq *error* nil) ; ������� �������� ������� ��� ���� �������
  (princ) ;; ������ �����
) ;; ����� defun c:SEARCH


;; ====================================================================
;; ������ 2: ������� � ����� ����������� ���������� ��� �������� ��'��Ҳ� (v5)
;; ====================================================================
;; �������: PASTEHERE (v5 - ����� g_last_search_result ���� ���������)
;; ���� ���� ������, ���������� �������� SEARCH � ����� *g_last_search_result*,
;; ��� ��'����, ������ ������������ (���������� ��� �� ��� ��������� �������).
;; �������� � ����� ������� ������� ��'���� (�����/�������) � ����� ������
;; ��'���(�) � ������ ����� (�� ���� ��������� �� ��������� COPYBASE).
(defun c:PASTEHERE ( / *error* ss ss_source i ename edata ipoint oldCmdEcho oldOsmode pasteCount )

  ;; --- ������� ������� ������� ---
  (defun *error* (msg)
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho)) ; ³������� CMDECHO
    (if oldOsmode (setvar "OSMODE" oldOsmode))   ; ³������� OSMODE
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command "_.UNDO" "_End")) ; ��������� UNDO ���� �������
    (cond ((not msg))                       ; ����� ��� ����������� (���������, ESC)
          ((vl-string-search "Function cancelled" msg)) ; ���������� ��������
          ((vl-string-search "quit / exit abort" msg))  ; ���������� ��������
          (T (princ (strcat "\n�������: " msg)))      ; ���� �������
    )
    ;; <-- �̲��: �������� ���������� ������ � ��������� ������� -->
    (setq *g_last_search_result* nil)
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

  ;; --- ���������� �������� ������ ������ (ss) ---
  (setq ss nil ss_source "") ; �����������
  (cond
    ;; 1. ��������� ���������� ��������� SEARCH
    ((and (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "����������� ���������� ������ (" (itoa (sslength ss)) " ��.)"))
    )
    ;; 2. ��������� ��������� ������ (PickFirst)
    ((setq ss (car (ssgetfirst))) ; �������� ���������� ������ ��'����
     (setq ss_source (strcat "������� ������ (" (itoa (sslength ss)) " ��.)"))
    )
    ;; 3. ��������� ����������� ������� ��'����
    (T
     (princ "\n�� �������� ����������� ���������� ������ ��� ���������� ������.")
     ;; ����� �� ���� ����� (INSERT)
     (princ "\n������� ��'���� (�����), � ����� ������� ���� ������� �������� � ������: ")
     (setq ss (ssget '((0 . "INSERT")))) ; Գ���� ��� �����
     (if ss
       (setq ss_source (strcat "����� �������� ��'���� (" (itoa (sslength ss)) " ��.)"))
       (progn (princ "\n��'���� �� �������. ������� ���������.") (exit)) ; �����, ���� ����� �� �������
     )
    )
  )

  ;; --- ������� ����� ������� ---
  (if ss
    (progn ;; � ������ ���� ������
      (princ (strcat "\n���������� ��'����(��) � ������ ����� � ����� ������� " ss_source "..."))

      ;; --- ������������ ���������� �� UNDO ---
      (setq oldCmdEcho (getvar "CMDECHO"))
      (setq oldOsmode (getvar "OSMODE"))
      (setvar "CMDECHO" 0) ;; �������� ��� ������
      (setvar "OSMODE" 0)  ;; �������� ��'����� ����'����
      (command "_.UNDO" "_Begin") ;; ������ ���������� UNDO
      (setq pasteCount 0) ;; ˳������� ������� �������

      ;; --- ���� �� ��'����� � ������ �� ������� ---
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i)) ;; �������� ��'� ��'���� � ������ ������
        (if (setq edata (entget ename)) ; ���������, �� ��'��� �� ����
          (progn
            ;; ��������, �� �� ���� (INSERT) - ��� ������������� � ssget, ��� �������� ��� ��������
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
              ;; ������������, ���� ��'��� � ������ - �� ���� (����������� ����� ������ ssget)
              (princ (strcat "\n ������������: ��'��� � ������ �� � ������ (INSERT): " (vl-princ-to-string ename)))
            )
          )
          ;; ������������, ���� ��'��� � ������ ��� �� ����
          (princ (strcat "\n ������������: ��'��� � ���������/������� ������ ��� �� ����: " (vl-princ-to-string ename)))
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
    ;; --- ���� ss ��������� nil (���� � ���������� �����, ��� ��� ���� �������) ---
    (princ "\n�� ������� ��������� ��'���� ��� �������.")
  ) ;; ����� if (ss)

  ;; --- ������� �� ����� ---
  ;; <-- �̲��: ������ ������� ��������� ������ ��� ����� � PASTEHERE -->
  (setq *g_last_search_result* nil)
  (setq *error* nil) ; ������� �������� �������
  (princ) ;; ������ �����
) ;; ����� defun c:PASTEHERE


;; ====================================================================
;; ������ 3: ����²��� �� ����������� ���������� Z �� ��������� "�������" (v5)
;; ====================================================================
;; �������: CHECKPOINTS (v5 - ����� g_last_search_result ���� ���������)
;; ���� ���� ������ "PIKET" � ���������� SEARCH, ��� ������ ������������.
;; ��� ������� ����� "PIKET" � ����� ������� �������� �������� "�������"
;; � ����������� Z ����� ������� �����.
;; �������� ��� ��� ���������, ��Ĳ�ߪ ����� ����� � ������������
;; �� ������ ����� �� ����������� Z ����������.
(defun c:CHECKPOINTS ( / *error* ss ss_source totalCount i ename edata ipoint zCoord
                       attEname attEdata attTag otmetkaStr otmetkaNum
                       diffList diffCount fuzz modCount answer oldCmdecho
                       ssDiff )

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
    ;; <-- �̲��: �������� ���������� ������ � ��������� ������� -->
    (setq *g_last_search_result* nil)
    (setq *error* nil)
    (princ)
  )

  ;; --- ����������� ---
  (setq diffList nil      ; ������ ����� � ������������ [(ename . (otmetkaStr . zCoord))]
        diffCount 0       ; ˳������� ����� � ������������
        fuzz 1e-6         ; ������ ��� ��������� ������ �����
        modCount 0        ; ˳������� ������� �����
        oldCmdecho nil    ; ���������� ����� getvar
        ss nil            ; ������� ���� ������
        ss_source ""      ; ������� ������ ��� ����������
  )
  (setq oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0) ; ��������� �������� ��� ������

  ;; --- ���������� �������� ������ ������ (ss) ---
  (cond
    ;; 1. ��������� ���������� ��������� SEARCH
    ((and (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "����������� ���������� ������ (" (itoa (sslength ss)) " ��.)"))
    )
    ;; 2. ��������� ��������� ������ (PickFirst)
    ((setq ss (car (ssgetfirst)))
     ;; ��������� ��������� ��������� ������, ��������� ����� ����� PIKET � ����������
     (if ss
       (progn
         (setq ss (ssget "_P" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Գ������� pickfirst set
         (if (or (null ss) (= 0 (sslength ss)))
             (setq ss nil) ; ���� ���� ���������� ����� �� ����������
             (setq ss_source (strcat "������� ������ (������������� �� " (itoa (sslength ss)) " ����� 'PIKET')"))
         )
       )
     )
    )
    ;; 3. ��������� ����������� ������� ��'����
    (T
     (princ "\n�� �������� ����������� ���������� ������ ��� ���������� ���������� ������.")
     (princ "\n������� ����� 'PIKET' ��� �������� Z-���������� �� �������� '�������': ")
     (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Գ���� ��� PIKET � ����������
     (if ss
       (setq ss_source (strcat "����� �������� ����� 'PIKET' (" (itoa (sslength ss)) " ��.)"))
       (progn (princ "\n����� 'PIKET' �� �������. ������� ���������.") (exit))
     )
    )
  )

  ;; --- ������� ����� �������� ---
  (if ss
    (progn
      (setq totalCount (sslength ss))
      (princ (strcat "\n�������� " (itoa totalCount) " ����� � " ss_source "..."))

      ;; --- ���� �� ��'����� � ������ ---
      (setq i 0)
      (repeat totalCount
        (setq ename (ssname ss i))
        (if (setq edata (entget ename)) ; ���������, �� ��'��� �� ����
          ;; �������� ���� ��'���� (�� ���� INSERT, ��� ssget ��� �������)
          (if (eq "INSERT" (cdr (assoc 0 edata)))
            (progn
              (setq ipoint (cdr (assoc 10 edata))) ; �������� ����� �������
              (setq zCoord (caddr ipoint))       ; �������� Z ����������
              (setq otmetkaStr nil)              ; ������� �������� ��������

              ;; --- ����� �������� "�������" ---
              ;; �������� �������� �������� (�� ����, �� �����������)
              (if (and (assoc 66 edata) (= 1 (cdr (assoc 66 edata))))
                (progn
                  (setq attEname (entnext ename))
                  (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                    (setq attTag (strcase (cdr (assoc 2 attEdata))))
                    (if (eq "�������" attTag)
                      (progn
                        (setq otmetkaStr (cdr (assoc 1 attEdata))) ; �������� �������� ��������
                        (setq attEname nil) ; �������� ����� ��������
                      )
                      (setq attEname (entnext attEname)) ; ��������� �������
                    )
                  ) ; end while
                )
                (princ (strcat "\n ������������: ���� " (vl-princ-to-string ename) " �� �� �������� (�����������)."))
              ) ; end if has attributes

              ;; --- ��������� �������� �������� �� Z ���������� ---
              (if otmetkaStr
                (progn
                  (setq otmetkaNum (distof (str-replace "," "." otmetkaStr))) ; ���������� ������������ ����� � ����� (�������� ���� �� ������)
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
            (princ (strcat "\n ������������: ��'��� � ������ �� � ������ (INSERT): " (vl-princ-to-string ename)))
          ) ; end if (is insert)
          (princ (strcat "\n ������������: ��'��� � ���������/������� ������ ��� �� ����: " (vl-princ-to-string ename)))
        ) ; end if (entget)
        (setq i (1+ i))
      ) ; end repeat

      ;; --- ��� ��� ���������� �������� �� ��Ĳ����� ���������� ---
      (if (= diffCount 0)
        (progn
           (princ (strcat "\n�������� ���������. �� " (itoa totalCount) " ���������� ����� ����� �������� ���������� Z �� �������� �������� '�������'."))
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

          ;; ��������� �� �������� ������ ������ ����� � ���������� �����
          (setq ssDiff (ssadd)) ; �������� ������� ����
          (if diffList
              (foreach item diffList
                  (if (entget (car item)) ; ��������� �������� ���������
                    (ssadd (car item) ssDiff) ; ������ ename
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
                (sssetfirst nil nil) ; ����� ��������
              )
          )

          ;; --- ����� �� ����������� ---
          (initget "��� ͳ") ; ��������� ����� �� ������ (����������)
          (setq answer (getkword "\n\n������ ���������� Z ��� ��� ����� �������� �� �������� '�������'? [���/ͳ]: "))

          (if (eq answer "���")
            (progn
              ;; --- ��������� ��� ---
              (princ "\n������� ����...")
              (command "_.UNDO" "_Begin") ; ������ ���������� UNDO

              (foreach item diffList
                (setq ename (car item))
                (setq otmetkaStr (car (cdr item)))
                (if (setq edata (entget ename))
                  (progn
                    (setq ipoint (cdr (assoc 10 edata)))
                    (setq otmetkaNum (distof (str-replace "," "." otmetkaStr))) ; ������������, �� ���������� ���������
                    (if otmetkaNum
                      (progn
                        (setq new_ipoint (list (car ipoint) (cadr ipoint) otmetkaNum))
                        (setq edata (subst (cons 10 new_ipoint) (assoc 10 edata) edata))
                        (if (entmod edata)
                          (setq modCount (1+ modCount))
                          (princ (strcat "\n ������� ���� Z ���������� ��� �����: " (vl-princ-to-string ename)))
                        )
                      )
                       (princ (strcat "\n ������� ����������� '" otmetkaStr "' ��� ����� " (vl-princ-to-string ename) " �� ��� ������ ����."))
                    )
                  )
                  (princ (strcat "\n �������: �� ������� �������� ��� ��� ����� " (vl-princ-to-string ename) " �� ��� ������ ����."))
                )
              ) ; end foreach item

              (command "_.UNDO" "_End") ; ��������� ���������� UNDO
              (princ (strcat "\n������ ������ Z ���������� ��� " (itoa modCount) " � " (itoa diffCount) " �����."))
            ) ; end progn (answer = ���)
            (princ "\n���� �� ������������.")
          ) ; end if (answer = ���)
        ) ; end progn (diffCount > 0)
      ) ; end if (= diffCount 0)
    ) ; end progn (ss is valid)
    (princ "\n�� ������� ��������� ��'���� ��� �������.")
  ) ; end if (ss)

  ;; --- ³��������� ���������� �� ����� ---
  (setvar "CMDECHO" oldCmdecho) ; ³������� CMDECHO
  ;; <-- �̲��: ������ ������� ��������� ������ ��� ����� � CHECKPOINTS -->
  (setq *g_last_search_result* nil)
  (setq *error* nil) ; ������� �������� �������
  (princ) ;; ������ �����
) ;; ����� defun c:CHECKPOINTS


;; ====================================================================
;; ������ 4: ��̲�� ϲ������� � ������Ҳ "������" (v5)
;; ====================================================================
;; �������: REPLACENAME (v5 - ����� g_last_search_result ���� ���������)
;; ���� ���� ������ "PIKET" � ���������� SEARCH, ��� ������ ������������.
;; ������ �������� ��� ������ �� �������� ��� �����.
;; ������ �� ��������� ����� �������� �� ����� � �������
;; �������� "������" ��� ������� ����� � ������. (������� �� �������!)
(defun c:REPLACENAME ( / *error* ss ss_source i ename edata
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
    ;; <-- �̲��: �������� ���������� ������ � ��������� ������� -->
    (setq *g_last_search_result* nil)
    (setq *error* nil)
    (princ)
  )

  ;; --- ����������� ---
  (setq modCount 0        ; ˳������� ������� �����
        oldCmdecho nil
        ss nil            ; ������� ���� ������
        ss_source ""      ; ������� ������ ��� ����������
  )
  (setq oldCmdecho (getvar "CMDECHO"))
  ;(setvar "CMDECHO" 0) ; ����� �������� ���, ���� ������

 ;; --- ���������� �������� ������ ������ (ss) ---
  (cond
    ;; 1. ��������� ���������� ��������� SEARCH
    ((and (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (setq ss_source (strcat "����������� ���������� ������ (" (itoa (sslength ss)) " ��.)"))
    )
    ;; 2. ��������� ��������� ������ (PickFirst)
    ((setq ss (car (ssgetfirst)))
     ;; ��������� ��������� ��������� ������, ��������� ����� ����� PIKET � ����������
     (if ss
       (progn
         (setq ss (ssget "_P" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Գ������� pickfirst set
         (if (or (null ss) (= 0 (sslength ss)))
             (setq ss nil) ; ���� ���� ���������� ����� �� ����������
             (setq ss_source (strcat "������� ������ (������������� �� " (itoa (sslength ss)) " ����� 'PIKET')"))
         )
       )
     )
    )
    ;; 3. ��������� ����������� ������� ��'����
    (T
     (princ "\n�� �������� ����������� ���������� ������ ��� ���������� ���������� ������.")
     (princ "\n������� ����� 'PIKET', � ���� ������� ������� �������� �������� '������': ")
     (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Գ���� ��� PIKET � ����������
     (if ss
       (setq ss_source (strcat "����� �������� ����� 'PIKET' (" (itoa (sslength ss)) " ��.)"))
       (progn (princ "\n����� 'PIKET' �� �������. ������� ���������.") (exit))
     )
    )
  )

  ;; --- ������� ����� ����� ---
  (if ss
    (progn ;; � ������ ���� ������
      (princ (strcat "\n���� ��������� " (itoa (sslength ss)) " ����� � " ss_source "."))

      ;; --- �������� ����� ��� ������ �� ����� ---
      (setq findStr (getstring T "\n������ ��������, ��� ������� � ������� '������' (������� �� �������): "))
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

      ;; --- ���� �� ��'����� � ������ ---
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i))
        (if (setq edata (entget ename)) ; ���������, �� ��'��� �� ����
          ;; �������� ���� ��'���� �� �������� �������� (�� ���� �� ��������)
          (if (and (eq "INSERT" (cdr (assoc 0 edata)))
                   (assoc 66 edata) (= 1 (cdr (assoc 66 edata))))
            (progn
              ;; --- ����� �������� "������" �� ���� ���� ---
              (setq attEname (entnext ename))
              (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                (setq attTag (strcase (cdr (assoc 2 attEdata)))) ; ��� (��� ��������)

                (if (eq "������" attTag)
                  (progn
                    (setq currentVal (cdr (assoc 1 attEdata))) ; ������� �������� ��������
                    ;; �������� ����� �� ��������� �������� ������� (������� �� �������)
                    (setq newVal (str-replace findStr replaceStr currentVal))

                    ;; ���� �������� ��������, ������� �������
                    (if (not (equal currentVal newVal)) ; ������������� equal ��� �����
                      (progn
                        (setq attEdata (subst (cons 1 newVal) (assoc 1 attEdata) attEdata))
                        (if (entmod attEdata)
                          (setq modCount (1+ modCount)) ; �������� �������� ������� ���
                          (princ (strcat "\n ������� ��������� �������� ��� �����: " (vl-princ-to-string ename)))
                        )
                      )
                    )
                    (setq attEname nil) ; �������� ����� �������� ��� ����� �����
                  )
                  ;; ���� �� �� ������� "������", ������� �� ����������
                  (setq attEname (entnext attEname))
                ) ; ����� if (eq "������" attTag)
              ) ; ����� while (������ ��������)
            ) ; ����� progn (�� ���� � ����������)
            (princ (strcat "\n ������������: ��'��� " (vl-princ-to-string ename) " �� � ������ 'PIKET' � ���������� (�����������)."))
          ) ; ����� if (�� ���� � ����������)
          (princ (strcat "\n ������������: ��'��� � ���������/������� ������ ��� �� ����: " (vl-princ-to-string ename)))
        ) ; ����� if (entget)
        (setq i (1+ i))
      ) ; ����� repeat

      ;; --- ���������� ����� UNDO ---
      (command "_.UNDO" "_End")

      ;; --- Գ������ ����������� ---
      (princ (strcat "\n���������. �������� ����� � ������� '������' ��� " (itoa modCount) " �����."))

    ) ; end progn (ss is valid)
    (princ "\n�� ������� ��������� ��'���� ��� �������.")
  ) ; end if (ss)

  ;; --- ³��������� ���������� �� ����� ---
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho)) ; ³������� CMDECHO, ���� ��������
  ;; <-- �̲��: ������ ������� ��������� ������ ��� ����� � REPLACENAME -->
  (setq *g_last_search_result* nil)
  (setq *error* nil) ; ������� �������� �������
  (princ) ;; ������ �����
) ;; ����� defun c:REPLACENAME


;; ====================================================================
;; ������ 5: ��������� ������ ���� ϲ��Ҳ� �� ��������� "������" (v5.2)
;; ====================================================================
;; �������: RENAME_OKM (v5.2 - ������ �������� �� ������������ ����� ����� ������)
;; ���� ���� ������ "PIKET" � ���������� SEARCH, ��� ������ ������������.
;; ��� ������� ����� "PIKET":
;; 1. ������ ����� � ����� � ������� "������" (����., � "���(22)12" -> "22").
;; 2. ���� ���������� ��������� ��'��� (TEXT ��� MTEXT) � ����� �������� ������.
;; 3. ���� ����� ��������, �� ���������� � "�" � �� �� ��� ���������� � ����� �������:
;;    - ����� ���������� ��� ���������� ���������.
;; ϳ��� �������� ��� �����:
;; 4. ��Ĳ�ߪ �� ������� ��'����, �� ���������� ���������.
;; 5. ������ ����������� ������������ �� ����.
;; 6. ���� �����������, ������� �����.

(defun c:RENAME_OKM ( / *error* ss ss_source i enamePiket edataPiket attEname attEdata attTag
                        attrValNomera blockPt openParen closeParen
                        extractedNum searchDist ssTextAll j textEnt textData textPt textVal
                        newTextVal textFoundForBlock updatedCount processedCount totalCount
                        oldCmdecho fuzz updatedTextEnts
                        ;; --- ��� ���� ��� �������� �� ������������ ---
                        texts_to_update_info ssHighlight potentialUpdateCount answer actualUpdateCount
                     )

  ;; --- ������� ������� ������� ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if (= 8 (logand 8 (getvar "UNDOCTL"))) (command-s "_.UNDO" "_End"))
    (cond ((not msg))
          ((vl-string-search "Function cancelled" msg))
          ((vl-string-search "quit / exit abort" msg))
          (T (princ (strcat "\n�������: " msg)))
    )
    (setq *g_last_search_result* nil) ; �������� ���������� ������
    (setq *error* nil)
    (princ)
  )

  ;; --- ����������� ---
  (setq updatedCount 0 ; ˳������� ��������� ��������� ������ (��������� �, �� ��� ��������)
        processedCount 0
        oldCmdecho nil
        ss nil
        ss_source ""
        fuzz 1e-9
        updatedTextEnts nil ; ������ ��� ����������/��������� ������ (��� �������� ����������)
        ;; --- ����������� ����� ������ ---
        texts_to_update_info nil ; ������ ��� ��������� ���������� ��� ���������: ((textEnt newTextVal enamePiket) ...)
        ssHighlight nil          ; ���� ������ ��� ��������
        potentialUpdateCount 0   ; ˳������� ������, �� ������� ���������� ����
        actualUpdateCount 0      ; ˳������� ������, �� ���� ����� ϲ��� ������������
        answer nil               ; ³������ �����������
  )
  (setq oldCmdecho (getvar "CMDECHO"))
  ;(setvar "CMDECHO" 0)

  ;; --- �������� ����� ������ ������ ---
  (setq searchDist (getdist "\n������ ����������� ������� ��� ������ ������ ��� ����� PIKET: "))
  (if (or (null searchDist) (<= searchDist 0))
    (progn (princ "\n������ ������� ������. ������� ���������.") (exit))
  )

  ;; --- ���������� �������� ������ ������ (ss) ��� ����� PIKET ---
   (cond
    ;; 1. ��������� ���������� ��������� SEARCH
    ((and (boundp '*g_last_search_result*)
          *g_last_search_result*
          (= 'PICKSET (type *g_last_search_result*))
          (> (sslength *g_last_search_result*) 0)
     )
     (setq ss *g_last_search_result*)
     (if (and ss (> (sslength ss) 0))
         (setq ss_source (strcat "����������� ���������� ������ (" (itoa (sslength ss)) " ����� 'PIKET')"))
         (setq ss nil ss_source "����������� ���������� ������ (��� �� ������� ��� �����������)")
     )
    )
    ;; 2. ��������� ��������� ������ (PickFirst)
    ((setq ss (cadr (ssgetfirst)))
      (if ss
        (progn
          (setq ss (ssget "_I" '((0 . "INSERT")(2 . "PIKET")(66 . 1)))) ; Գ�������
          (if (or (null ss) (= 0 (sslength ss)))
              (setq ss nil ss_source "������� ������ (��� ���� �� ������ ����� 'PIKET' � ����������)")
              (setq ss_source (strcat "������� ������ (������������� �� " (itoa (sslength ss)) " ����� 'PIKET')"))
          )
        )
        (setq ss nil)
      )
    )
    ;; 3. ��������� ����������� ������� ��'����
    (T
     (princ "\n�� �������� ����������� ���������� ������ ��� ���������� ���������� ������.")
     (princ "\n������� ����� 'PIKET', ��� ���� ������� ������� �����: ")
     (setq ss (ssget '((0 . "INSERT")(2 . "PIKET")(66 . 1))))
     (if ss
       (setq ss_source (strcat "����� �������� ����� 'PIKET' (" (itoa (sslength ss)) " ��.)"))
       (progn (princ "\n����� 'PIKET' �� �������. ������� ���������.") (exit))
     )
    )
  )

  ;; --- ������� ����� ������ �� ����� ��������� �� ��������� ---
  (if ss
    (progn
      (setq totalCount (sslength ss))
      (princ (strcat "\n����� ��������� ��������� ���� ��� " (itoa totalCount) " ����� 'PIKET' � " ss_source "..."))

      ;; (�� �������� UNDO ���, �� �� �� ������� �����)

      (setq ssTextAll (ssget "_X" '((0 . "TEXT,MTEXT"))))
      (if (null ssTextAll) (princ "\n������������: � �������� �� �������� ������ ��������� ��'���� (TEXT ��� MTEXT)."))

      ;; --- ���� �� ��������/��������� ������ PIKET ---
      (setq i 0)
      (repeat totalCount
        (setq enamePiket (ssname ss i))
        (setq extractedNum nil attrValNomera nil)
        (setq textFoundForBlock nil) ; ������� ��������� ��� ������� �����

        (if (setq edataPiket (entget enamePiket))
          (progn
            (setq processedCount (1+ processedCount))
            (setq blockPt (cdr (assoc 10 edataPiket)))

            ;; --- ����� �������� "������" ---
            (if (and (assoc 66 edataPiket) (= 1 (cdr (assoc 66 edataPiket))))
              (progn
                (setq attEname (entnext enamePiket))
                (while (and attEname (eq "ATTRIB" (cdr (assoc 0 (setq attEdata (entget attEname))))))
                  (setq attTag (strcase (cdr (assoc 2 attEdata))))
                  (if (eq "������" attTag)
                    (progn (setq attrValNomera (cdr (assoc 1 attEdata))) (setq attEname nil))
                    (setq attEname (entnext attEname))
                  )
                )
              )
            )

            ;; --- ��������� ������ � �������� ---
            (if attrValNomera
              (progn
                (setq openParen (vl-string-search "(" attrValNomera))
                (setq closeParen (vl-string-search ")" attrValNomera (if openParen (+ openParen 1) 0)))
                (if (and openParen closeParen (> closeParen openParen))
                  (setq extractedNum (substr attrValNomera (+ openParen 2) (- closeParen openParen 1)))
                )
              )
            )

            ;; --- ����� ������, ���� ����� �������� � � ������� ��'���� ---
            (if (and extractedNum ssTextAll blockPt)
              (progn
                 (setq j 0)
                 ;; ���� �� �Ѳ� ��������� ��'�����
                 (while (and (< j (sslength ssTextAll)) (not textFoundForBlock))
                   (setq textEnt (ssname ssTextAll j))
                   (if (setq textData (entget textEnt))
                     (progn
                        (setq textPt (cdr (assoc 10 textData)))
                        (setq textVal (cdr (assoc 1 textData)))

                        ;; �������� ������, �������� "�" � �� �� ��� ��� ����� ��� ����������
                        (if (and textPt textVal
                                 (<= (distance blockPt textPt) searchDist)
                                 (= (vl-string-search "�" textVal) 0)
                                 (not (member textEnt updatedTextEnts))
                            )
                          (progn
                              ;; ��� ����� �������� ��� ������ �����
                              (setq updatedCount (1+ updatedCount)) ; ������ �� ������� �������
                              (setq newTextVal (strcat "�" extractedNum))

                              ;; ����������, �� ����� ��� �� ��������� ��������
                              (if (not (equal textVal newTextVal))
                                 (progn
                                    ;; --- �̲��: �� ���������, � ���в����� ���������� ---
                                    (setq texts_to_update_info (cons (list textEnt newTextVal enamePiket) texts_to_update_info))
                                    (setq potentialUpdateCount (1+ potentialUpdateCount)) ; ������ ����� �, �� ���������� ����
                                    (princ (strcat "\n   * �������� �� ���������: <" (vl-princ-to-string textEnt) "> ('" textVal "' -> '" newTextVal "') ��� ����� <" (vl-princ-to-string enamePiket) ">"))
                                 )
                                 ;;(princ (strcat "\n   - ����� <" (vl-princ-to-string textEnt) "> ��� ����� <" (vl-princ-to-string enamePiket) "> ��� �� ��������� ��������: '" textVal "'"))
                              )

                              ;; ������ ����� �� ������ ����������, ��� ����� ���� ���� �� ����
                              (setq updatedTextEnts (cons textEnt updatedTextEnts))
                              (setq textFoundForBlock T) ; ���������, �� ��� ����� ����� ����� ��������
                          )
                        ) ; ����� if (�������� ������, �������� �� ������)
                     )
                   )
                   (setq j (1+ j))
                 ) ; end while (����� ������)
              )
            )
          )
        ) ; end if (entget enamePiket)
        (setq i (1+ i))
      ) ; end repeat (�� ������ PIKET)

      ;; --- �������� ��������� ��������� �� ����� �� ������������ ---
      (if (> potentialUpdateCount 0)
        (progn
          (princ (strcat "\n\n�������� " (itoa potentialUpdateCount) " ��������� ����, �� ���������� ���������."))
          ;; --- ��������� ������ ������ ��� �������� ---
          (setq ssHighlight (ssadd))
          (foreach item texts_to_update_info
            (if (entget (car item)) ; �������� ��������� ����� ����������
                (ssadd (car item) ssHighlight)
            )
          )

          (if (> (sslength ssHighlight) 0)
            (progn
              (princ (strcat "\n������� " (itoa (sslength ssHighlight)) " ��������� ��'���� ��� ��������."))
              (sssetfirst nil ssHighlight) ; ������� ������� ������

              ;; --- ����� �� ������������ ---
              (initget "��� ͳ")
              (setq answer (getkword "\n\n������� ������ ������� ����? [���/ͳ]: "))

              (if (eq answer "���")
                (progn
                  ;; --- ��������� ��� ---
                  (princ "\n������� ���������...")
                  (command "_.UNDO" "_Begin")
                  (foreach item texts_to_update_info
                     (setq textEnt (car item))
                     (setq newTextVal (cadr item))
                     (setq enamePiket (caddr item)) ; ����� ��������������� ��� ���������

                     (if (setq textData (entget textEnt))
                       (progn
                          (setq currentTextVal (cdr (assoc 1 textData))) ; �������� ������� �������� ����� �����
                          (setq textData (subst (cons 1 newTextVal) (assoc 1 textData) textData))
                          (if (entmod textData)
                            (progn
                               (princ (strcat "\n  ��������: <" (vl-princ-to-string textEnt) "> ('" currentTextVal "' -> '" newTextVal "')"))
                               (setq actualUpdateCount (1+ actualUpdateCount))
                            )
                            (princ (strcat "\n  ������� ��������� ������ <" (vl-princ-to-string textEnt) ">"))
                          )
                       )
                       (princ (strcat "\n  �������: �� ������� �������� ��� ��� ������ <" (vl-princ-to-string textEnt) "> �� ��� ������ ���������."))
                     )
                  )
                  (command "_.UNDO" "_End")
                  (princ (strcat "\n������ �������� " (itoa actualUpdateCount) " ��������� ����."))
                )
                ;; --- ���� ���������� ������ "ͳ" ��� �������� ---
                (progn
                  (princ "\n���� ��������� ������������. ������� ���� �� ��������.")
                  (sssetfirst nil nil) ; ����� ��������
                )
              )
            )
            ;; --- ���� �� ������� �������� ���� ��� �������� ---
            (princ "\n�� ������� �������� ���� ������ ��� �������� ������.")
          )
        )
        ;; --- ���� �� �������� ������, �� ���������� ��������� ---
        (progn
           (princ "\n\n�� �������� ��������� ����, �� ���������� ���������.")
           (if updatedTextEnts (sssetfirst nil nil)) ; ����� ��������, ���� ���� ���� ������� �����
        )
      )

      ;; --- Գ������� ��� (����� �������) ---
      (princ (strcat "\n\n�������� ���������."))
      (princ (strcat "\n������ ����� 'PIKET' ��� ������� (� " ss_source "): " (itoa totalCount)))
      (princ (strcat "\n������� ��������� �����: " (itoa processedCount)))
      (princ (strcat "\n�������� ��������� ��������� ���� ��� �����: " (itoa updatedCount))) ; ��, �� ������ (��������� ��� ��������)
      (if (> potentialUpdateCount 0) (princ (strcat "\n� ��� ����������� ���������: " (itoa potentialUpdateCount))))
      (if (eq answer "���") (princ (strcat "\n�������� �������� ���� ������������: " (itoa actualUpdateCount))))


    ) ; end progn (ss is valid)
    (princ "\n�� ������� ��������� ��'���� ��� ������� (���� ����� 'PIKET' � ������).")
  ) ; end if (ss)

  ;; --- ³��������� ���������� �� ����� ---
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (setq *g_last_search_result* nil) ; �������� ���������� ������
  (setq *error* nil)
  (princ) ;; ������ �����
) ;; ����� defun c:RENAME_OKM

;; --- ����������� ��� ������������ ---
(princ "\nLISP-������� (v5.2 �����������.") ; �������� �����

(princ "\n�������:")
(princ "\n  SEARCH         - ����� ����� 'PIKET' �� ��������� '������', �������� �� ���������� ����������.")
(princ "\n  PASTEHERE      - ������� ��'���� � ������ � ����� ����� (� ����������� ������ ��� �������� ������).")
(princ "\n  CHECKPOINTS    - �������� Z ���������� �� �������� '�������' � ������ 'PIKET' (� ����������� ������ ��� �������� ������), � ��������� �����������.")
(princ "\n  REPLACENAME    - ����� �������� � ������� '������' ����� 'PIKET' (� ����������� ������ ��� �������� ������, ������� �� �������).")
(princ "\n  RENAME_OKM     - ��������� ������ ('�...') ��� ����� 'PIKET' (� ��������� �� �������������).") ; <-- �������� ����

(princ) ;; ������ �����