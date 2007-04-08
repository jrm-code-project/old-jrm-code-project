;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:T -*-

;;; This code implements the ISPELL interface that can be
;;; used both from ZWEI and the Lisp Listener

;;; I'm not sure what the state of this code was at MIT, but
;;; Pace and GJC have hacked it here at LMI, changing the protocal
;;; from the ISPELL server back to be that which EMACS(TECO) and ISPELL on ITS
;;; use to communicate, so it will work between LAMBDA's and their friendly NuMachines.
;;;
;;; (C) Enchancments Copyright LISP Machine, Inc. 1984
;;;   See filename "Copyright" for
;;; licensing and release information.


;; Bug: check spelling of doesn't or isn't is confused by the '
;; bug for spaces in certain spots.  optimizations could be done.
;; There are places where the code could be NES'd
;; don't forget to use site info to determine spell hosts.
;; keep local list of correct words, hash table, etc.

;;query replace stuff is hopelessly broken.

;(DEFVAR *ISPELL-INIT-LOADED* NIL "T iff we have loaded the user's ispell init file")

;the above should be done at the oz level, but lets consider it anyway.

(DEFUN CORRECT-SPELLING (WORD-OR-WORDLIST &OPTIONAL (STREAM QUERY-IO) IN-EDITOR-P)
  "Given a word or string of words, return a list of words which
corresponds to those word or words spelled correctly.  In the case
where the user has to supply information, ask the questions on stream STREAM.
If IN-EDITOR-P is T, we will modify the buffer to reflect the user's wishes."
  (mapcar #'(lambda (result)
              (correct-spelling-1 (car result) (cdr result) stream in-editor-p))
          (alist-of-check-spelling-wordlist
            (cond ((stringp word-or-wordlist)
                   word-or-wordlist)
                  ((listp word-or-wordlist)
                   (format nil "~{~A~^ ~}" word-or-wordlist))
                  ('else
                   (ferror nil "not string or list: ~S" word-or-wordlist))))))

(DEFUN CORRECT-SPELLING-1 (WORD RESULT STREAM IN-EDITOR-P)
  "Auxilary function used by CORRECT-SPELLING.  Given a word and the
result given by the spell server, return the correct spelling of a
given word.   Ask questions of the user on stream STREAM.  If
IN-EDITOR-P is T,  we will change the buffer to have the correct word in it."
  (COND ((eq nil RESULT)                ;word is spelled incorrectly
         (IF IN-EDITOR-P (TYPEIN-LINE "Couldn't find it."))
         WORD)
        ((CONSP RESULT)  ;word isn't spelled correctly
         (IF IN-EDITOR-P (TYPEIN-LINE "Couldn't find it.   Choices listed above."))
         (LET ((REPLACEMENT
                 (USER-CHOOSE-WORD WORD RESULT STREAM IN-EDITOR-P)))
           (IF IN-EDITOR-P  ;fix up the screen
               (SEND STANDARD-OUTPUT ':MAKE-COMPLETE))
           (AND (NOT (EQUALP REPLACEMENT WORD))
                IN-EDITOR-P
                (REPLACE-WORD-WITH WORD REPLACEMENT
                  (LET ((*QUERY-IO* STREAM))
                    (Y-OR-N-P "Should I query replace your change as well? "))))))
        ((EQ t RESULT)          ;word exists.
         (IF IN-EDITOR-P (TYPEIN-LINE "Found it."))
         WORD)
        ('else
         (IF IN-EDITOR-P (TYPEIN-LINE "Found it because of ~A." result))
         word)))

;;;; for the ispell server
(DEFUN AVAILABLE-ISPELL-HOST (&OPTIONAL ERROR-OK (TIMEOUT 240.))
  "Return any host that is up which supports the ispell protocol.
If no such host, signal an error, unless ERROR-OK is T."
  (LET ((UP-HOST (CHAOS:UP-HOSTS (GET-SITE-OPTION :ISPELL-SERVER-HOSTS) 1 TIMEOUT)))
    (AND (NULL UP-HOST)
         (NOT ERROR-OK)
         (FERROR 'SYS:NO-SERVER-UP "No host which supports the spell protocol is up now."))
    (CAR UP-HOST)))

(defun ispell-user (input-string &optional (host (AVAILABLE-ISPELL-HOST)))
  "connect to the available ispell server"
  (with-open-stream (s (chaos:open-stream host "ISPELL"))
    (send s :line-out input-string)
    (send s :force-output)
    (send s :eof)
    (with-output-to-string (out)
      (stream-copy-until-eof s out))))

(DEFUN ALIST-OF-CHECK-SPELLING-WORDLIST (words &aux checker)
  "WORDS is a string of words. return an alist (<word> . <correct-spellings>)
where <correct-spellings> is T  meaning <word> is correct, or it is a list of
possible corrections."
  (cond ((and (setq checker (find-package "SPELL"))
              (setq checker (intern-soft "SPELL-WORD" checker))
              (fboundp checker))
         (mapcar #'(lambda (word)
                     (let ((result (funcall checker word)))
                       (cons word
                             (IF (and (stringp result) (string-equal result word))
                                    t
                               result))))
                 (tokens-from-string words)))
        ('else
         (let ((server-result (ispell-user words)))
           ;; The result from the TOPS-20 spell server is on one line:
           ;; tokens of NIL, T, or a list (TOKEN TOKEN TOKEN)
           ;; The result from ITS/UNIX is
           ;; seperate lines per word:
           ;;  *                   ok.
           ;;  + <root-word>       stripped down to this root word.
           ;;  & <choices...>
           ;;  #                   not found.
           (mapcar #'cons
                   (mapcar #'string (tokens-from-string words))
                   (with-input-from-string (rs server-result)
                     (do ((line)(l))
                         ((null (setq line (readline rs nil)))
                          (nreverse l))
                       (SELECTOR (CHAR line 0) CHAR=
                         (#/; ())
                         (#/* (push t l))
                         (#/# (push nil l))
                         (#/+ (push t l))
                         (#/& (push (tokens-from-string line 1) l))
                         (t (ferror nil "unknown result from spell server ~S" line))))))))))


(defun tokens-from-string (string &optional (start 0) (end (length string)))
  (do ((l nil (if token (cons token l) l))
       (token))
      ((= start end)
       (nreverse l))
    (multiple-value (token start) (token-from-string string start end))))


(defun token-from-string (string start end)
  (prog (j)
        (setq j start)
        eat-whitespace
        (cond ((= j end) (return (values nil j)))
              ((mem #'char-equal (aref string j) '(#\space #\tab))
               (setq j (1+ j))
               (go eat-whitespace)))
        (setq start j)
        accumulate-token
        (cond ((or (= j end) (mem #'char-equal (aref string j) '(#\space #\tab)))
               (return (values (substring string start j) j)))
              ('else
               (setq j (1+ j))
               (go accumulate-token)))))


;;user interface
(DEFUN USER-CHOOSE-WORD (WORD CHOICES &OPTIONAL (STREAM QUERY-IO) IN-EDITOR-P &AUX NEW-WORD)
  "Provide the user with a list of words which may be the word he wants.
We must return a real word, according to what the user said."
  (LET ((RESULT (QQUERY CHOICES
                        (FORMAT NIL
                                (IF IN-EDITOR-P
                                    "Choose the letter corresponding to the correct spelling of the word ~A.
Type ~:@c to leave the word alone,
or type ~C to enter the new spelling from the keyboard.

If you change the word, you will be asked whether to
query-replace all occurrences of it."
                                  "Choose the letter corresponding to the correct spelling of the word ~A.
Type ~:@c to leave the word alone,
or type ~C to enter the new spelling from the keyboard.")
                                WORD #/SPACE #/R)
                        (LIST #/SPACE #/R #/r)
                        STREAM)))
    (COND ((STRINGP RESULT)  ;;its the word itself.
           (SETQ NEW-WORD RESULT))
          ((= RESULT 0.)  ;they typed a space.
           (SETQ NEW-WORD WORD))
          ((OR (= RESULT 1.) (= RESULT 2.))  ;;they typed in an R.
           (SETQ NEW-WORD (GET-CORRECT-SPELLING-FROM-USER WORD STREAM))))
    NEW-WORD))

(DEFUN GET-CORRECT-SPELLING-FROM-USER (WORD &OPTIONAL (STREAM QUERY-IO))
  "Ask the user how to spell WORD, and return his reponse as an answer."
  (FORMAT STREAM "~%Please type in the correct spelling of the word ~A.  (End with Return)~&"
          WORD)
  (READLINE-TRIM STREAM))

;;make this a keyword coded function like fquery.  But this will do for now.
;;but if char in char-set is also a generatable character (ie a thru s, and r)
(DEFUN QQUERY (LIST-OF-CHOICES &OPTIONAL (STRING-TO-SHOW "") CHAR-SET (STREAM QUERY-IO))
  "Provide another user-choice facility that should be very quick.
Provide a list of choices, for which we want one and only one of those choices
as a value to return.  STRING-TO-SHOW is a string used for documentation.
CHAR-SET is magic list of characters that we will allow the user to type.
If a CHAR in CHAR-SET is typed, return the number corresponding to
which character was typed.  Note that the case is preserved in the CHAR-SET, if you
want to find both a upper and lower case letter, you must include both.
How this function works will be drastically changed in the future,  for
now it just claims to work most of the time."
  ;;this function should be improved, and use keywords for arguments.
  (LET ((L (LENGTH LIST-OF-CHOICES)))
    (COND ((< L 27) ;we can use a one character response.
           (SEND STREAM ':FRESH-LINE)
           (SEND STREAM ':LINE-OUT STRING-TO-SHOW)
           (LOOP FOR N FROM 1 TO L
                 DOING
                 ;;test for lossage cases
                 (SEND STREAM ':LINE-OUT
                       (FORMAT NIL "~C) ~A" (+ #/A N -1) (NTH (1- N) LIST-OF-CHOICES))))
           (LET ((N (QQUERY-VALID-CHAR L (MAPCAR #'CHARACTER CHAR-SET))))
             (IF (> (1+ N) L) (- N L) (NTH N LIST-OF-CHOICES))))
          (T        ;we can't use a one character response.
           (LET ((CHOICES-ALIST
                   (LOOP FOR CHOICES IN LIST-OF-CHOICES
                         COLLECT CHOICES)))
                 (TV:MENU-CHOOSE CHOICES-ALIST STRING-TO-SHOW))))))

(DEFUN QQUERY-VALID-CHAR (N &OPTIONAL CHAR-SET &AUX CHAR POS)
  "Return a number in the range of 0 to n-1 which represents a letter of the alphabet
that the user typed.  If the user types a character in CHAR-SET return
N plus the char's position in the CHAR-SET"
  (IF (AND (= N 0) (NULL CHAR-SET))
      (FERROR NIL "There must be at least one valid character to type."))
  (*CATCH 'CHAR
    (LOOP UNTIL NIL ;forever
          (SETQ CHAR (SEND QUERY-IO ':TYI))
          (COND ((SETQ POS (FIND-POSITION-IN-LIST CHAR CHAR-SET))
                 (*THROW 'CHAR (+ N POS)))
                ((AND (SETQ CHAR (- (CHAR-UPCASE CHAR) #/A)) (< CHAR N) (> CHAR -1))
                 (*THROW 'CHAR CHAR))
                (T
                 (SEND QUERY-IO ':STRING-OUT
                       (FORMAT NIL "~%Please type ~@[a letter between A and ~C~]~{ or ~:@c~}."
                               (AND (> N 0) (+ #/A N -1)) CHAR-SET))))))) ;;win for 0?

;;ZWEI side of the picture.  We want to implement meta-$ and meta-x correct spelling

(DEFUN CURRENT-WORD-BP ()
  "Return the BP that represents the BP that begins the current word."
  (FORWARD-WORD (FORWARD-WORD (FORWARD-CHAR (POINT) -1 T) 1 T) -1 T))

(DEFCOM COM-CORRECT-WORD-SPELLING
  "Correct the spelling of the word at point.
Uses a SPELL server on some file server host.
If word is incorrect, a list of possible corrections is printed.
You can choose one of them to replace just this occurrence
or all occurrences of the word you checked."
  ()
  ;Figure out what the current word is.  Make it work for end/beginning of buffer.
  (LET* ((BP1 (CURRENT-WORD-BP))
         (BP2 (FORWARD-WORD BP1 1 T))
         (WORD (STRING-INTERVAL BP1 BP2)))
;        (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON NIL)  ;This should never be T for long.
    (TYPEIN-LINE (STRING-APPEND "Checking the spelling of " WORD #/.)) ;format,
    (CONDITION-CASE ()
        (CORRECT-SPELLING WORD STANDARD-OUTPUT T)  ;are we sure about the stream?
      (:NO-ERROR
       DIS-TEXT) ;;success
      (SYS:NO-SERVER-UP
       (TYPEIN-LINE "No spell server host is up now.")
       DIS-NONE)))
  DIS-TEXT) ;;temp crock

;; lossage in terms of next word!!

;;;fox boo too foo zoo for  fi fo fum mooo moooo bar fdoo moo

(DEFUN REPLACE-WORD-WITH (WORD REPLACEMENT &OPTIONAL QUERY-REPLACE-P BP1 BP2)
  "Replace WORD located between BP1 and BP2 with REPLACEMENT.
If QUERY-REPLACE-P, than query replace whole buffer as well."
  (PREPARE-WINDOW-FOR-REDISPLAY *WINDOW*)
;;foe bar baz for boo too foo fox foo fi fo
  (IF (NULL BP1)
      (SETQ BP1 (CURRENT-WORD-BP)))
  (IF (NULL BP2)
      (SETQ BP2 (FORWARD-WORD BP1 1 T)))
  (LET ((*CASE-REPLACE* T)
        (REPLACEMENT (STRING-DOWNCASE REPLACEMENT))
        (WORD (STRING-DOWNCASE WORD)))  ;for case-replace
    (WITH-UNDO-SAVE ("Spelling correction" BP1 BP2 T)
      (SETQ BP2 (CASE-REPLACE BP1 BP2 REPLACEMENT))
      (MOVE-BP (POINT) BP2))
    (WHEN QUERY-REPLACE-P
      (POINT-PDL-PUSH (COPY-BP (POINT)) *WINDOW*)
      (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*))
      (QUERY-REPLACE (POINT) (INTERVAL-LAST-BP *INTERVAL*)
                     (STRING-DOWNCASE WORD) (STRING-DOWNCASE REPLACEMENT)))))


(defcom com-correct-spelling
  "Run the spelling corrector on the buffer." ()
  (cond ((not (fboundp 'spell-word))
         (if (y-or-n-p "Load ISPELL? ")
             (make-system 'ispell)
           (barf))))
  (let (bp1 bp2)
    (cond ((window-mark-p *window*)
           (setq bp1 (mark) bp2 (point))
           (or (bp-< bp1 bp2) (psetq bp1 bp2 bp2 bp1)))
          (t
           (setq bp1 (interval-first-bp *interval*))
           (setq bp2 (interval-last-bp *interval*))))
    (with-undo-save ("Spelling correction" bp1 bp2 t)
      (do ((line (bp-line bp1) (line-next line))
           (from-index (bp-index bp1) 0)
           (last-line (bp-line bp2)))
          (())

        (let ((to-index (if (eq line last-line)
                            (bp-index bp2)
                          (string-length line))))
          (correct-spelling-line line from-index to-index)
          )

        (if (eq line last-line)
            (return nil))
        )))

  (fresh-line *query-io*)
;  (send *query-io* :send-if-handles :typeout-stays)
  (format *query-io* "Spell checker done.")

  dis-text)

;don't cons if everything is spelled right,
;if not, don't worry about it
(defun correct-spelling-line (line from-index to-index)
  (do (end)
      (())
    (multiple-value-setq (from-index end)
        (find-next-word-on-line line from-index to-index))
    (cond ((null from-index) (return-from correct-spelling-line nil)))
    (let ((choices (spell-word line from-index end)))
      (cond ((or (consp choices)
                 (null choices))
             (correct-spelling-line-hard line from-index end choices to-index)
             (return-from correct-spelling-line nil)
             )
            (t
             (setq from-index end))))))

(defun correct-spelling-line-hard (line word-begin word-end choices line-end)
  (let ((word-begin-bp (create-bp line word-begin))
        (word-end-bp (create-bp line word-end :moves))
        (line-end-bp (create-bp line line-end :moves))
        next-word-begin-index next-word-end-index)
    (tagbody
        (go have-choices)
     get-next-word
        (multiple-value-setq (next-word-begin-index next-word-end-index)
          (find-next-word-on-line line (bp-index word-end-bp) (bp-index line-end-bp)))
        (cond ((null next-word-begin-index)
               (return-from correct-spelling-line-hard nil)))
        (move-bp word-begin-bp line next-word-begin-index)
        (move-bp word-end-bp line next-word-end-index)
     get-new-choices
        (setq choices (spell-word line (bp-index word-begin-bp) (bp-index word-end-bp)))
     have-choices
        (cond ((cli:listp choices)
               (let ((accept-p (fix-word word-begin-bp word-end-bp choices)))
                 (redisplay-all-windows)
                 (if accept-p
                     (go get-next-word)
                   (go get-new-choices))))))))


;returns T if shouldn't recheck.
(defun fix-word (from-bp to-bp choices)
  (cond ((null choices)
         (show-context from-bp to-bp)
         (format t "~2&Type a replacement, or just NEWLINE to accept: ")
         (get-replacement-word-from-user from-bp to-bp))
        (t
         (show-context from-bp to-bp)
         (format t "~2&Type a digit to select that word, SPACE to accept the current word,")
         (format t "~&or R to let you type a string to replace it.")
         (format t "~&")
         (do ((cl choices (cdr cl))
              (number 0 (1+ number)))
             ((null cl))
           (format t "~&~2d: ~A" number (car cl)))
         (let ((resp (fquery
                       `(:make-complete t
                         :type :tyi
                         :choices ,(list* '(#/R "R") '(#/space #/space)
                                          (loop for i from 0 by 1
                                                for c in choices
                                                collect (list i (format nil "~d" i))))
                         :stream terminal-io)
                       "Your choice: ")))
           (selectq resp
             (#/R
              (format t "~2&Replacement word: ")
              (get-replacement-word-from-user from-bp to-bp)
              nil)
             (#/space
              t)
             (t
              (cond ((numberp resp)
                     (case-replace from-bp to-bp (nth resp choices))
                     nil)
                    (t
                     (ferror nil "fqurey isn't supposed to let this happen")))))))))

(defun show-context (bp1 bp2)
  (format t "~&Can't find ~a~2&" (substring (bp-line bp1) (bp-index bp1) (bp-index bp2)))
  (let ((start-bp (forward-line bp1 -2 t))
        (end-bp (forward-line bp2 2 t))
        box-upper-left-x
        box-upper-left-y
        box-lower-right-x
        box-lower-right-y)
    (charmap (start-bp end-bp)
      (let ((current-bp (charmap-bp-before)))
        (cond ((bp-= current-bp bp1)
               (multiple-value-setq (box-upper-left-x box-upper-left-y)
                 (send *standard-output* :read-cursorpos)))
              ((bp-= current-bp bp2)
               (multiple-value-setq (box-lower-right-x box-lower-right-y)
                 (send *standard-output* :read-cursorpos))
               (incf box-lower-right-y (send *standard-output* :line-height))
               )))
      (send *standard-output* :tyo (charmap-character)))
    (send *standard-output*
          :draw-rectangle
          (- box-lower-right-x box-upper-left-x)
          (- box-lower-right-y box-upper-left-y)
          box-upper-left-x
          box-upper-left-y
          tv:alu-xor)))

(defun get-replacement-word-from-user (from-bp to-bp)
  (let ((new-word (readline)))
    (cond ((zerop (string-length new-word))
           t)
          (t
           (case-replace from-bp to-bp new-word)
           nil))))


(defun find-next-word-on-line (line from-index to-index)
  (let ((next-begin from-index)
        next-end)
    (loop when (= next-begin to-index)
          do (return-from find-next-word-on-line nil)
          until (= (word-syntax (aref line next-begin)) word-alphabetic)
          do (incf next-begin))
    (setq next-end next-begin)
    (loop when (or (= next-end to-index)
                   (= (word-syntax (aref line next-end)) word-delimiter))
          do (return (values next-begin next-end))
          do (incf next-end))))
