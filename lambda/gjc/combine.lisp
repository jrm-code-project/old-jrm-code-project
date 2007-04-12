;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-


(DEFUN COMBINE-FILES (WILDCARD OUTPUT)
  (LET ((L (SORT (DELQ NIL (MAPCAR #'CAR (FS:DIRECTORY-LIST WILDCARD))) #'FILENAME-LESSP)))
    (FORMAT T "~&;COMBINING ~D FILES INTO ~A~%" (LENGTH L) OUTPUT)
    (WITH-OPEN-FILE (O OUTPUT :OUT)
      (DO ((J 1 (1+ J))
           (F L (CDR F)))
          ((NULL F))
        (WITH-OPEN-FILE (I (CAR F))
          (FORMAT T "~&; ~D ~S -> ~S~%" J (SEND (SEND I :TRUENAME) :STRING-FOR-EDITOR)
                  (SEND (SEND O :TRUENAME) :STRING-FOR-EDITOR))
          (STREAM-COPY-UNTIL-EOF I O))))))


(DEFUN FILENAME-LESSP (P1 P2)
  ;; I CREATED FILENAMES LIKE OUTPUT1, OUTPUT2, ... OUTPUT9, OUTPUT10, ...
  ;; FOR WHICH ALPHALESSP FAILS. FOO.
  (LET ((N1 (SEND P1 :NAME))
        (N2 (SEND P2 :NAME)))
    (LESSP (PARSE-NUMBER N1 (LENGTH "OUTPUT"))
           (PARSE-NUMBER N2 (LENGTH "OUTPUT")))))
