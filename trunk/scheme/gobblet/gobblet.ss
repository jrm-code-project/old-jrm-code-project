(module gobblet mzscheme
  (require (lib "list.ss"))

  (define-syntax bit->mask
    ;; Return a 16-bit integer with BIT turned on.
    (syntax-rules ()
      ((bit->mask bit) (vector-ref #(#x0001 #x0002 #x0004 #x0008
                                     #x0010 #x0020 #x0040 #x0080
                                     #x0100 #x0200 #x0400 #x0800
                                     #x1000 #x2000 #x4000 #x8000) bit))))


  (define-syntax bit->complement-mask
    ;; Return a 16-bit integer with BIT turned off.
    (syntax-rules ()
      ((bit->complement-mask bit) (vector-ref #(#xFFFE #xFFFD #xFFFB #xFFF7
                                                #xFFEF #xFFDF #xFFBF #xFF7F
                                                #xFEFF #xFDFF #xFBFF #xF7FF
                                                #xEFFF #xDFFF #xBFFF #x7FFF) bit))))

  (define-syntax %row
    (syntax-rules ()
      ((%row coords) (bitwise-and coords #x03))))

  (define-syntax %column
    (syntax-rules ()
      ((%column coords) (arithmetic-shift coords -2))))

  (define-syntax %column*4
    (syntax-rules ()
      ((%column*4 coords) (bitwise-and coords #xC0))))

  (define-syntax encode-coordinates
    (syntax-rules ()
      ((encode-coordinates row column) (+ row (arithmetic-shift column 2)))))

  (define (encode-coordinates1 pair)
    (+ (car pair) (arithmetic-shift (cdr pair) 2)))

  (define (decode-coordinates coords)
    (values (%row coords) (%column coords)))

  ;; A bitboard slice is simply an integer between [0, 65536)
  ;; So we can represent a bitboard as a vector of 4 slices
  (define (bitboard-allocate) (make-vector 4 0))

  (define (slice-pattern-matcher name patterns)
    (display "Computing slice pattern matcher for ")
    (display name)
    (newline)
    (flush-output)
    (time
     (let ((vector (make-vector 65536 #f)))
       (let loop ((i 0))
         (if (= i 65536)
             (lambda (slice) (vector-ref vector slice))
             (begin
               (vector-set! vector i (let loop ((pat patterns))
                                       (cond ((null? pat) #F)
                                             ((= (car pat) (bitwise-and (car pat) i)) #t)
                                             (else (loop (cdr pat))))))
               (loop (add1 i))))))))

  (define four-in-row?
    (slice-pattern-matcher "four-in-row"
                           '(#x000F #x00F0 #x0F00 #xF000
                              #x1111 #x2222 #x4444 #x8888
                              #x1248 #x8421)))

  (define three-in-row?
    (slice-pattern-matcher "three-in-row"
                           '(#x0007 #x0070 #x0700 #x7000
                             #x000B #x00B0 #x0B00 #xB000
                             #x000D #x00D0 #x0D00 #xD000
                             #x000E #x00E0 #x0E00 #xE000
                             #x1110 #x1101 #x1011 #x0111
                             #x2220 #x2202 #x2022 #x0222
                             #x4440 #x4404 #x4044 #x0444
                             #x8880 #x8808 #x8088 #x0888
                             #x0248 #x1048 #x1208 #x1240
                             #x0421 #x8021 #x8401 #x8420)))

  (define slice->coordinate-list
    (let ((vector (make-vector 65536 '())))
      (display "Computing slice coordinate lists.")
      (newline)
      (flush-output)
      (time
       (let row-loop ((row 0))
         (if (= row 4)
             (lambda (slice) (vector-ref vector slice))
             (let col-loop ((col 0))
               (if (= col 4)
                   (row-loop (add1 row))
                   ;; This permutes the coordinates to put the
                   ;; more likely good moves nearer the front.
                   (let ((r (bitwise-xor row #x03))
                         (c (bitwise-xor col #x03)))
                     (let loop ((mask (bit->mask (encode-coordinates r c)))
                                (index 0))
                       (if (= index 65536)
                           (col-loop (add1 col))
                           (begin (if (not (zero? (bitwise-and index mask)))
                                      (vector-set! vector index (append (vector-ref vector index) (list (cons r c)))))
                                  (loop mask (add1 index)))))))))))))

  (define triple-mask
    (time
     (let ((vector (make-vector 65536 0)))
       (display "Computing triple masks.")
       (newline)
       (flush-output)
       (let loop ((position 0))
         (if (= position 65536)
             (lambda (slice) (vector-ref vector slice))
             (if (three-in-row? position)
                 (let inner-loop ((result 0)
                                  (triple-masks  '(#x0007 #x0070 #x0700 #x7000
                                                          #x000B #x00B0 #x0B00 #xB000
                                                          #x000D #x00D0 #x0D00 #xD000
                                                          #x000E #x00E0 #x0E00 #xE000
                                                          #x1110 #x1101 #x1011 #x0111
                                                          #x2220 #x2202 #x2022 #x0222
                                                          #x4440 #x4404 #x4044 #x0444
                                                          #x8880 #x8808 #x8088 #x0888
                                                          #x0248 #x1048 #x1208 #x1240
                                                          #x0421 #x8021 #x8401 #x8420)))
                   (cond ((null? triple-masks)
                          (vector-set! vector position result)
                          (loop (add1 position)))
                         ((= (bitwise-and position (car triple-masks))
                             (car triple-masks))
                          (inner-loop (bitwise-ior result (car triple-masks))
                                      (cdr triple-masks)))
                         (else (inner-loop result (cdr triple-masks)))))
                 (loop (add1 position))))))))

  (define-syntax bitboard/slice
    (syntax-rules ()
      ((bitboard/slice bitboard slice-number) (vector-ref bitboard slice-number))))

  (define-syntax bitboard/set-slice!
    (syntax-rules ()
     ((bitboard/slice bitboard slice-number new-value) (vector-set! bitboard slice-number new-value))))

  (define (bitboard/copy result source)
      (bitboard/set-slice! result 0 (bitboard/slice source 0))
      (bitboard/set-slice! result 1 (bitboard/slice source 1))
      (bitboard/set-slice! result 2 (bitboard/slice source 2))
      (bitboard/set-slice! result 3 (bitboard/slice source 3))
      result)

  (define-syntax unary-bitboard-op
    (syntax-rules ()
      ((unary-bitboard-op logical-operator)
       (lambda (result source)
         (bitboard/set-slice! result 0 (logical-operator (bitboard/slice source 0)))
         (bitboard/set-slice! result 1 (logical-operator (bitboard/slice source 1)))
         (bitboard/set-slice! result 2 (logical-operator (bitboard/slice source 2)))
         (bitboard/set-slice! result 3 (logical-operator (bitboard/slice source 3)))
         result))))

  (define bitboard/not (unary-bitboard-op bitwise-not))

  (define-syntax binary-bitboard-op
    (syntax-rules ()
      ((binary-bitboard-op logical-operator)
       (lambda (result l r)
         (bitboard/set-slice! result 0 (logical-operator (bitboard/slice l 0)
                                                         (bitboard/slice r 0)))
         (bitboard/set-slice! result 1 (logical-operator (bitboard/slice l 1)
                                                         (bitboard/slice r 1)))
         (bitboard/set-slice! result 2 (logical-operator (bitboard/slice l 2)
                                                         (bitboard/slice r 2)))
         (bitboard/set-slice! result 3 (logical-operator (bitboard/slice l 3)
                                                         (bitboard/slice r 3)))
         result))))

  (define bitboard/or (binary-bitboard-op bitwise-ior))
  (define bitboard/and (binary-bitboard-op bitwise-and))
  (define bitboard/xor (binary-bitboard-op bitwise-xor))

  (define-syntax trinary-bitboard-op
    (syntax-rules ()
      ((binary-bitboard-op logical-operator)
       (lambda (result l c r)
         (bitboard/set-slice! result 0 (logical-operator (bitboard/slice l 0)
                                                         (logical-operator (bitboard/slice c 0)
                                                                           (bitboard/slice r 0))))
         (bitboard/set-slice! result 1 (logical-operator (bitboard/slice l 1)
                                                         (logical-operator (bitboard/slice c 1)
                                                                           (bitboard/slice r 1))))
         (bitboard/set-slice! result 2 (logical-operator (bitboard/slice l 2)
                                                         (logical-operator (bitboard/slice c 2)
                                                                           (bitboard/slice r 2))))
         (bitboard/set-slice! result 3 (logical-operator (bitboard/slice l 3)
                                                         (logical-operator (bitboard/slice c 3)
                                                                           (bitboard/slice r 3))))
         result))))

  (define bitboard/3or (trinary-bitboard-op bitwise-ior))
  (define bitboard/3and (trinary-bitboard-op bitwise-and))
  (define bitboard/3xor (trinary-bitboard-op bitwise-xor))

  (define *canonical-bitboard-mask* #(#xFFFF #xFFFF #xFFFF #xFFFF))

  (define-syntax canonicalize-bitboard
    (syntax-rules ()
      ((canonicalize-bitboard b) (let ((bb b)) (bitboard/and bb bb *canonical-bitboard-mask*) bb))))

  (define-syntax %bitboard-ref2
    (syntax-rules ()
      ((%bitboard-ref2 bitboard slice mask)
       (not (zero? (bitwise-and (bitboard/slice bitboard slice) mask))))))

  (define-syntax %bitboard-set2
    (syntax-rules ()
      ((%bitboard-set bitboard slice mask)
       (let ((bb bitboard)
             (s slice))
         (bitboard/set-slice! bb s (bitwise-ior (bitboard/slice bb s) mask))
         bb))))

  (define-syntax %bitboard-clear2
    (syntax-rules ()
      ((%bitboard-clear2 bitboard slice mask)
       (let ((bb bitboard)
             (s slice))
         (bitboard/set-slice! bb s (bitwise-and (bitboard/slice bb s) (bitwise-not mask)))
         bb))))

  (define-syntax %bitboard-clear2a
    (syntax-rules ()
      ((%bitboard-clear2 bitboard slice cmask)
       (let ((bb bitboard)
             (s slice))
         (bitboard/set-slice! bb s (bitwise-and (bitboard/slice bb s) cmask))
         bb))))

  (define (bitboard/ref bitboard slice row column)
    (%bitboard-ref2 bitboard slice (bit->mask (encode-coordinates row column))))

  (define (bitboard/set! bitboard slice row column)
    (%bitboard-set2 bitboard slice (bit->mask (encode-coordinates row column))))

  (define (bitboard/clear! bitboard slice row column)
    (%bitboard-clear2a bitboard slice (bit->complement-mask (encode-coordinates row column))))

  (define (show-slice slice)
    (let outer ((row 0))
      (if (= row 4)
          (newline)
          (begin
            (let inner ((col 0))
              (if (= col 4)
                  (newline)
                  (begin (display " ")
                         (display (zero? (bitwise-and (bitwise-not slice) (bit->mask (encode-coordinates row col)))))
                         (inner (add1 col)))))
            (outer (add1 row))))))

  (define (bitboard/dump-slice bitboard slice)
    (let outer ((row 0))
      (if (= row 4)
          (void)
          (begin
            (newline)
            (let inner ((col 0))
              (if (= col 4)
                  (outer (add1 row))
                  (begin (display " ")
                         (display (bitboard/ref bitboard slice row col))
                         (inner (add1 col))))))))
    (newline))

  (define (bitboard/place-piece bitboard slice target-coordinate)
    (vector (if (= slice 0)
                (bitwise-ior
                 (bit->mask target-coordinate)
                 (bitboard/slice bitboard 0))
                (bitboard/slice bitboard 0))
            (if (= slice 1)
                (bitwise-ior
                 (bit->mask target-coordinate)
                 (bitboard/slice bitboard 1))
                (bitboard/slice bitboard 1))
            (if (= slice 2)
                (bitwise-ior
                 (bit->mask target-coordinate)
                 (bitboard/slice bitboard 2))
                (bitboard/slice bitboard 2))
            (if (= slice 3)
                (bitwise-ior
                 (bit->mask target-coordinate)
                 (bitboard/slice bitboard 3))
                (bitboard/slice bitboard 3))))

  (define (bitboard/move-piece bitboard source-coordinate target-coordinate)
    (let ((source-mask (bit->mask source-coordinate)))
      (cond ((not (zero? (bitwise-and source-mask (bitboard/slice bitboard 3))))
             (vector (bitboard/slice bitboard 0)
                     (bitboard/slice bitboard 1)
                     (bitboard/slice bitboard 2)
                     (bitwise-xor
                      (bit->mask target-coordinate)
                      (bitwise-xor
                       source-mask
                       (bitboard/slice bitboard 3)))))
            ((not (zero? (bitwise-and source-mask (bitboard/slice bitboard 2))))
             (vector (bitboard/slice bitboard 0)
                     (bitboard/slice bitboard 1)
                     (bitwise-xor
                      (bit->mask target-coordinate)
                      (bitwise-xor
                       source-mask
                       (bitboard/slice bitboard 2)))
                     (bitboard/slice bitboard 3)))
            ((not (zero? (bitwise-and source-mask (bitboard/slice bitboard 1))))
             (vector (bitboard/slice bitboard 0)
                     (bitwise-xor
                      (bit->mask target-coordinate)
                      (bitwise-xor
                       source-mask
                       (bitboard/slice bitboard 1)))
                     (bitboard/slice bitboard 2)
                     (bitboard/slice bitboard 3)))
            (else (vector (bitwise-xor
                           (bit->mask target-coordinate)
                           (bitwise-xor
                            source-mask
                            (bitboard/slice bitboard 0)))
                          (bitboard/slice bitboard 1)
                          (bitboard/slice bitboard 2)
                          (bitboard/slice bitboard 3))))))

  (define-struct position
    (half-turn
     previous-move
     white-home
     black-home
     white-bitboard
     black-bitboard
     %moves
     raw-evaluation))

  (define (whose-move position)
    (if (zero? (remainder (position-half-turn position) 2))
        'white
        'black))

  (define (initial-position)
    (set! *transposition-table* (make-hash-table 'equal))
    (letrec ((position (make-position 0
                                      #f
                                      (make-vector 3 4)
                                      (make-vector 3 4)
                                      (bitboard-allocate)
                                      (bitboard-allocate)
                                      (delay (compute-moves position))
                                      0)))
      position))

  (define *transposition-table* (make-hash-table 'equal))

  (define hash-table-probe 0)
  (define hash-table-miss 0)

  (define (make-position1 half-turn move white-home black-home white-bitboard black-bitboard)
    (set! hash-table-probe (add1 hash-table-probe))
    (let* ((key (list half-turn white-home black-home white-bitboard black-bitboard))
           (probe (hash-table-get *transposition-table* key (lambda ()
                                                              (set! hash-table-miss (add1 hash-table-miss))
                                                              #f))))
      (or probe
          (letrec ((new-position (make-position half-turn
                                                move
                                                white-home
                                                black-home
                                                white-bitboard
                                                black-bitboard
                                                (delay (compute-moves new-position))
                                                0)))
            (set-position-raw-evaluation! new-position (evaluate-position new-position))
            (hash-table-put! *transposition-table* key new-position)
            new-position))))

  (define (scrub-transposition-table! half-turn)
    (hash-table-for-each *transposition-table*
                         (lambda (key value)
                           (if (<= (car key) half-turn)
                               (hash-table-remove! *transposition-table* key)))))

  (define (position/move old-position source-coordinate target-coordinate)
    (if (eq? (whose-move old-position) 'white)
        (make-position1 (add1 (position-half-turn old-position))
                        (cons source-coordinate target-coordinate)
                        (position-white-home old-position)
                        (position-black-home old-position)
                        (bitboard/move-piece (position-white-bitboard old-position)
                                             source-coordinate target-coordinate)
                        (position-black-bitboard old-position))

        (make-position1 (add1 (position-half-turn old-position))
                       (cons source-coordinate target-coordinate)
                       (position-white-home old-position)
                       (position-black-home old-position)
                       (position-white-bitboard old-position)
                       (bitboard/move-piece (position-black-bitboard old-position)
                                            source-coordinate target-coordinate))))

  (define (position/place old-position pile target-coordinate)
    (if (eq? (whose-move old-position) 'white)
        (let* ((home (position-white-home old-position))
               (slice (sub1 (vector-ref home pile))))
          (make-position1 (add1 (position-half-turn old-position))
                                                (cons pile target-coordinate)
                                                (vector (if (= pile 0)
                                                            slice
                                                            (vector-ref home 0))
                                                        (if (= pile 1)
                                                            slice
                                                            (vector-ref home 1))
                                                        (if (= pile 2)
                                                            slice
                                                            (vector-ref home 2)))
                                                (position-black-home old-position)
                                                (bitboard/place-piece (position-white-bitboard old-position)
                                                                      slice target-coordinate)
                                                (position-black-bitboard old-position)))

        (let* ((home (position-black-home old-position))
               (slice (sub1 (vector-ref home pile))))
          (make-position1 (add1 (position-half-turn old-position))
                                                (cons pile target-coordinate)
                                                (position-white-home old-position)
                                                (vector (if (= pile 0)
                                                            slice
                                                            (vector-ref home 0))
                                                        (if (= pile 1)
                                                            slice
                                                            (vector-ref home 1))
                                                        (if (= pile 2)
                                                            slice
                                                            (vector-ref home 2)))
                                                (position-white-bitboard old-position)
                                                (bitboard/place-piece (position-black-bitboard old-position)
                                                                      slice target-coordinate)))))

  (define (visible-piece-at position row col)
    (let ((wbb (position-white-bitboard position))
          (bbb (position-black-bitboard position)))
      (let loop ((slice 3))
        (cond ((< slice 0) (values #f #f))
              ((bitboard/ref wbb slice row col) (values 'white slice))
              ((bitboard/ref bbb slice row col) (values 'black slice))
              (else (loop (sub1 slice)))))))

  (define (show-board position)
    (let row-loop ((row 0))
      (if (= row 4)
          (newline)
          (begin
            (newline)
            (let col-loop ((col 0))
              (if (= col 4)
                  (row-loop (add1 row))
                  (begin
                    (display " ")
                    (call-with-values (lambda () (visible-piece-at position row col))
                                      (lambda (side value)
                                        (cond ((eq? side #f) (display " ."))
                                              ((eq? side 'white) (display  "w") (display value))
                                              (else (display  "b") (display value)))))
                    (col-loop (add1 col)))))))))

  (define (show-position position)
    (newline)
    (display "Half-turn:  ") (display (position-half-turn position))
    (display ",  ") (display (whose-move position)) (display " to move.")
    (newline)
    (display (position-black-home position))
    (show-board position)
    (display (position-white-home position))
    (newline)
;    (display "Evaluation:  ")
;    (display ;(alphabeta-board (position-moves position) 2 evaluate-position max min -65535 65535)
;            (position-raw-evaluation position))
    (flush-output))

  (define (position/place! position side size row column)
    (bitboard/set! (if (eq? side 'white)
                       (position-white-bitboard position)
                       (position-black-bitboard position))
                   size
                   row
                   column))

  (define (position/clear! position side size row column)
    (bitboard/clear! (if (eq? side 'white)
                       (position-white-bitboard position)
                       (position-black-bitboard position))
                   size
                   row
                   column))


  (define (exposed-pieces my-bitboard his-bitboard)
    ;; return a bitboard slice  showing the visible  pieces.
    (bitwise-ior
     (bitboard/slice my-bitboard 3)
     (bitwise-and
      (bitwise-not (bitboard/slice his-bitboard 3))
      (bitwise-ior
       (bitboard/slice my-bitboard 2)
       (bitwise-and
        (bitwise-not (bitboard/slice his-bitboard 2))
        (bitwise-ior
         (bitboard/slice my-bitboard 1)
         (bitwise-and
          (bitwise-not (bitboard/slice his-bitboard 1))
          (bitboard/slice my-bitboard 0))))))))

  (define (exposed-white position)
    ;; return a bitboard slice  showing the visible white pieces.
    (exposed-pieces (position-white-bitboard position) (position-black-bitboard position)))

  (define (exposed-black position)
    ;; return a bitboard slice  showing the visible black pieces.
    (exposed-pieces (position-black-bitboard position) (position-white-bitboard position)))

  (define (win-for-white? position)
    (four-in-row? (exposed-white position)))

  (define (win-for-black? position)
    (four-in-row? (exposed-black position)))

  (define (raw-bit-count position)
    (cond ((zero? position) 0)
          ((even? position) (raw-bit-count (/ position 2)))
          (else (add1 (raw-bit-count (sub1 position))))))

  (define (pair-count position)
    (let loop ((pair-count 0)
               (pair-masks '(#x0003 #x0005 #x0006 #x0009 #x000A #x000C
                             #x0030 #x0050 #x0060 #x0090 #x00A0 #x00C0
                             #x0300 #x0500 #x0600 #x0900 #x0A00 #x0C00
                             #x3000 #x5000 #x6000 #x9000 #xA000 #xC000
                             #x0011 #x0101 #x0110 #x1001 #x1010 #x1100
                             #x0022 #x0202 #x0220 #x2002 #x2020 #x2200
                             #x0044 #x0404 #x0440 #x4004 #x4040 #x4400
                             #x0088 #x0808 #x0880 #x8008 #x8080 #x8800
                             #x8001 #x8020 #x8400 #x0420 #x0401 #x0021
                             #x1008 #x0208 #x0048 #x0240 #x1040 #x1200)))
      (cond ((null? pair-masks) pair-count)
            ((= (bitwise-and (car pair-masks) position)
                (car pair-masks)) (loop (add1 pair-count) (cdr pair-masks)))
            (else (loop pair-count (cdr pair-masks))))))

  (define (triple-count position)
    (let loop ((triple-count 0)
               (triple-masks '(#x0007 #x0070 #x0700 #x7000
                               #x000B #x00B0 #x0B00 #xB000
                               #x000D #x00D0 #x0D00 #xD000
                               #x000E #x00E0 #x0E00 #xE000
                               #x1110 #x1101 #x1011 #x0111
                               #x2220 #x2202 #x2022 #x0222
                               #x4440 #x4404 #x4044 #x0444
                               #x8880 #x8808 #x8088 #x0888
                               #x0248 #x1048 #x1208 #x1240
                               #x0421 #x8021 #x8401 #x8420)))
      (cond ((null? triple-masks) triple-count)
            ((= (bitwise-and (car triple-masks) position)
                (car triple-masks)) (loop (add1 triple-count) (cdr triple-masks)))
            (else (loop triple-count (cdr triple-masks))))))

  (define *maximum-score* 9000)
  (define *minimum-score* -9000)

  (define evaluate-slice
    (time
     (let ((position-score-vector (make-vector 65536 0))
           (top-score 0))
       (display "Computing slice scores.")
       (newline)
       (flush-output)
       (let position-loop ((position 0))
         (if (= position 65536)
             (lambda (slice) (vector-ref position-score-vector slice))
             (begin (vector-set! position-score-vector position
                                 (if (four-in-row? position)
                                     *maximum-score*
                                     (+ ;(raw-bit-count position)
                                      (raw-bit-count (bitwise-and position #x9669))
                                      (* (raw-bit-count (bitwise-and position #x0660)) 2)
                                      ;; Pairs imply at least 2 raw bits,
                                      ;; So baseline score there 6.  We want 1 pair
                                      ;; to be better than that.  However, there is
                                      ;; no point in having too many pairs
                                      (let ((pc (min (pair-count position) 20)))
                                        (* pc pc 7))
                                      ;; Triples imply 3 pairs and 3
                                      ;; bits, so we want 1 triple
                                      ;; to be better than that.

                                      ;; bit 1 = 3
                                      ;; bit 2 = 3
                                      ;; bit 3 = 3
                                      ;; pair 1-2, pair 2-3, pair 1-3 = 63
                                      ;; total = 96
                                      ;; It is unlikely that more than 4 triples is useful.
                                      (let ((tc (min (triple-count position) 4)))
                                        (* tc tc tc 96)))))
                    (position-loop (add1 position))))))))

  (define epcount 0)

  (define (evaluate-position position)
    (set! epcount (+ epcount 1))
    (- (evaluate-slice
        (exposed-white position))
       (evaluate-slice
        (exposed-black position))))

  (define (m<= max left right)
    (= (max left right) right))

  (define-syntax m>=
    (syntax-rules ()
      ((m>= max left right) (let ((l left))
                              (= (max l right) l)))))

  (define-syntax m<
    (syntax-rules ()
      ((m< max left right) (not (m>= max left right)))))

  (define-syntax m>
    (syntax-rules ()
      ((m> max left right) (not (m<= max left right)))))

  (define (madd1 max n)
    (max (add1 n) (sub1 n)))

  (define (madd max a b)
    (max (+ a b) (- a b)))

  (define (choose-move max left right)
    (if (or (and (= (car left) (car right))
                 (or (and (= (random 2) 0)
                          (= (length left) (length right)))
                     (m< max (length left) (length right))))
            (m> max (car left) (car right)))
        left
        right))

  (define (minimax-moves position move-list depth evaluate max min)
    (cond ((or (zero? depth)
               (four-in-row? (exposed-white position))
               (four-in-row? (exposed-black position)))
           (cons (evaluate position) '()))
          ((null? move-list) (cons *minimum-score* '()))
          (else
           (let* ((new-pos (caddr (car move-list)))
                  (new-moves (if (> depth 1)
                                 (position-moves new-pos)
                                 '()))
                  (continuation (minimax-moves new-pos
                                               new-moves
                                               (sub1 depth)
                                               evaluate
                                               min
                                               max))
                  (best-so-far (cons (car continuation)
                                     (cons (car move-list) (cdr continuation)))))
             (cond ((null? (cdr move-list)) best-so-far)
                   (else
                    (let ((next-move (minimax-moves position
                                                    (cdr move-list)
                                                    depth
                                                    evaluate
                                                    max
                                                    min)))
                      (choose-move max best-so-far next-move))))))))

  (define (alphabeta-moves position move-list depth evaluate max min lower-bound upper-bound top-level)
    (cond ((or (zero? depth)
               (four-in-row? (exposed-white position))
               (four-in-row? (exposed-black position)))
           (cons (evaluate position) '()))
          ((null? move-list) (cons lower-bound '()))
          (else
           (let* ((new-pos (caddr (car move-list)))
                  (new-moves (if (> depth 1)
                                 (sorted-position-moves new-pos (lambda (l r) (m< max l r)))
                                 '()))
                  (continuation (alphabeta-moves new-pos
                                                 new-moves
                                                 (sub1 depth)
                                                 evaluate
                                                 min
                                                 max
                                                 upper-bound
                                                 lower-bound
                                                 #f))
                  (best-so-far (cons (car continuation)
                                     (cons (car move-list) (cdr continuation))))
                  (window-bottom (max (car best-so-far) lower-bound)))
             (if top-level
                 (begin (display ".") (flush-output)))
             (cond ((null? (cdr move-list)) best-so-far)
                   ((m>= max (car best-so-far) upper-bound) best-so-far);; prune the rest of this level.
                   (else
                    (choose-move max best-so-far
                                 (alphabeta-moves position
                                                  (cdr move-list)
                                                  depth
                                                  evaluate
                                                  max
                                                  min
                                                  window-bottom
                                                  upper-bound
                                                  top-level))))))))

  (define (mtdf position move-list depth evaluate max min lower-bound upper-bound initial-guess top-level)
    (let loop ((lower-score lower-bound)
               (upper-score upper-bound)
               (guess initial-guess)
               (best-move '()))
      (newline)
      (display guess)
      (flush-output)
      (if (m>= max lower-score upper-score)
          (begin (newline)
                 (flush-output)
                 best-move)
          (let* ((beta (if (= guess lower-score)
                           (madd1 max guess)
                           guess))
                 (beta-1 (madd1 min beta))
                 (start epcount)
                 (midmove
                  (alphabeta-moves position move-list depth evaluate
                                   max min beta-1 beta #t))
                 (end epcount)
                 (new-guess (car midmove)))
            (display "searched ")
            (display (- end start))
            (display " positions ")
            (flush-output)
            (cond ((m< max new-guess beta) ;;  fail hi
                   (display "too optimistic")
                   (flush-output)
                   (loop lower-score
                         new-guess
                         (floor (/ (+ new-guess lower-score lower-score) 3))
                         midmove))
                  ((m> max new-guess beta)
                   (display "too pessimistic")
                   (flush-output)
                   (loop new-guess
                         upper-score
                         (floor (/ (+ new-guess new-guess upper-score) 3))
                         midmove))
                  (else
                   (display "possible score (equal)")
                   (flush-output)
                   (loop new-guess
                         upper-score
                         new-guess
                         midmove)))))))

  (define-struct move
    (source
     target
     score
     from-position
     %to-position))

  (define (make-move1 source target from-position)
    (make-move source target 0 from-position (delay (%do-move from-position source target))))

  (define (compute-moves position)
    (define (compute-moves1 my-home my-bitboard his-bitboard)
      (let* ((pile-0-top   (vector-ref my-home 0))
             (pile-1-top   (vector-ref my-home 1))
             (pile-2-top   (vector-ref my-home 2))
             (slice-3-sources  (bitboard/slice my-bitboard 3))
             (slice-3-source-list (slice->coordinate-list slice-3-sources))
             (slice-3-occupied (bitwise-ior slice-3-sources
                                            (bitboard/slice his-bitboard 3)))
             (slice-3-targets (bitwise-and #xFFFF (bitwise-not slice-3-occupied)))
             (slice-3-target-list (slice->coordinate-list slice-3-targets))

             (slice-2-sources (bitwise-and
                               (bitboard/slice my-bitboard 2)
                               (bitwise-not slice-3-occupied)))
             (slice-2-source-list (slice->coordinate-list slice-2-sources))
             (slice-2-occupied (bitwise-ior slice-3-occupied
                                            slice-2-sources
                                            (bitboard/slice his-bitboard 2)))
             (slice-2-targets (bitwise-and #xFFFF (bitwise-not slice-2-occupied)))
             (slice-2-target-list (slice->coordinate-list slice-2-targets))

             (slice-1-sources (bitwise-and
                               (bitboard/slice my-bitboard 1)
                               (bitwise-not slice-2-occupied)))
             (slice-1-source-list (slice->coordinate-list slice-1-sources))
             (slice-1-occupied (bitwise-ior slice-2-occupied
                                            slice-1-sources
                                            (bitboard/slice his-bitboard 1)))
             (slice-1-targets (bitwise-and #xFFFF (bitwise-not slice-1-occupied)))
             (slice-1-target-list (slice->coordinate-list slice-1-targets))

             (slice-0-sources (bitwise-and
                               (bitboard/slice my-bitboard 0)
                               (bitwise-not slice-1-occupied)))

             (slice-0-occupied (bitwise-ior slice-1-occupied
                                            slice-0-sources
                                            (bitboard/slice his-bitboard 0)))
             ;; These are the empty squares.
             (slice-0-targets (bitwise-and #xFFFF (bitwise-not slice-0-occupied)))

             (slice-0-target-list
              (slice->coordinate-list slice-0-targets))

             (my-exposed   (bitwise-ior slice-3-sources
                                        slice-2-sources
                                        slice-1-sources
                                        slice-0-sources))

             (his-exposed  (bitwise-ior (bitboard/slice his-bitboard 3)
                                        (bitwise-and (bitboard/slice his-bitboard 2)
                                                     slice-3-targets)
                                        (bitwise-and (bitboard/slice his-bitboard 1)
                                                     slice-2-targets)
                                        (bitwise-and (bitboard/slice his-bitboard 0)
                                                     slice-1-targets)))
             )

        (define (compute-piece-moves source-location piece-size)
          (map (lambda (target) (make-move1 source-location target position))
               (cond ((= piece-size 3) slice-3-target-list)
                     ((= piece-size 2) slice-2-target-list)
                     ((= piece-size 1) slice-1-target-list)
                     ((= piece-size 0) slice-0-target-list)
                     (else '()))))

        (define (compute-normal-entering-moves)
          ;; If you place a gobblet from one of your external stacks
          ;; you must place it on an empty square.
          ;; You can gobble your own pieces with a gobblet taken from
          ;; an exteral stack at any time.
          (let ((from-pile0 (cond ((zero? pile-0-top) '())
                                  ((= pile-0-top 1) (map (lambda (target)
                                                           (make-move1 0 target position))
                                                         slice-0-target-list))
                                  ((= pile-0-top 2) (map (lambda (target)
                                                           (make-move1 0 target position))
                                                         (slice->coordinate-list
                                                          (bitwise-ior
                                                           slice-0-sources
                                                           slice-0-targets))))
                                  ((= pile-0-top 3) (map (lambda (target)
                                                           (make-move1 0 target position))
                                                         (slice->coordinate-list
                                                          (bitwise-ior
                                                           slice-1-sources
                                                           slice-0-sources
                                                           slice-0-targets))))
                                  ((= pile-0-top 4) (map (lambda (target)
                                                           (make-move1 0 target position))
                                                         (slice->coordinate-list
                                                          (bitwise-ior
                                                           slice-2-sources
                                                           slice-1-sources
                                                           slice-0-sources
                                                           slice-0-targets))))))

                (from-pile1 (cond ((or (zero? pile-1-top)
                                       (= pile-1-top pile-0-top)) '())
                                  ((= pile-1-top 1) (map (lambda (target)
                                                           (make-move1 1 target position))
                                                         slice-0-target-list))
                                  ((= pile-1-top 2) (map (lambda (target)
                                                           (make-move1 1 target position))
                                                         (slice->coordinate-list
                                                          (bitwise-ior
                                                           slice-0-sources
                                                           slice-0-targets))))
                                  ((= pile-1-top 3) (map (lambda (target)
                                                           (make-move1 1 target position))
                                                         (slice->coordinate-list
                                                          (bitwise-ior
                                                           slice-1-sources
                                                           slice-0-sources
                                                           slice-0-targets))))
                                  ((= pile-1-top 4) (map (lambda (target)
                                                           (make-move1 1 target position))
                                                         (slice->coordinate-list
                                                          (bitwise-ior
                                                           slice-2-sources
                                                           slice-1-sources
                                                           slice-0-sources
                                                           slice-0-targets))))))
                (from-pile2 (cond ((or (zero? pile-2-top)
                                       (= pile-2-top pile-1-top)) '())
                                  ((= pile-2-top 1) (map (lambda (target)
                                                           (make-move1 2 target position))
                                                         slice-0-target-list))
                                  ((= pile-2-top 2) (map (lambda (target)
                                                           (make-move1 2 target position))
                                                         (slice->coordinate-list
                                                          (bitwise-ior
                                                           slice-0-sources
                                                           slice-0-targets))))
                                  ((= pile-2-top 3) (map (lambda (target)
                                                           (make-move1 2 target position))
                                                         (slice->coordinate-list
                                                          (bitwise-ior
                                                           slice-1-sources
                                                           slice-0-sources
                                                           slice-0-targets))))
                                  ((= pile-2-top 4) (map (lambda (target)
                                                           (make-move1 2 target position))
                                                         (slice->coordinate-list
                                                          (bitwise-ior
                                                           slice-2-sources
                                                           slice-1-sources
                                                           slice-0-sources
                                                           slice-0-targets)))))))
               (append from-pile0 from-pile1 from-pile2)))

        (define (covering-entering-targets size)
          (slice->coordinate-list
           (bitwise-and
            (triple-mask his-exposed)
            (cond ((= size 2) (bitboard/slice his-bitboard 0))
                  ((= size 3) (bitwise-ior
                               (bitboard/slice his-bitboard 0)
                               (bitboard/slice his-bitboard 1)))
                  ((= size 4) (bitwise-ior
                               (bitboard/slice his-bitboard 0)
                               (bitboard/slice his-bitboard 1)
                               (bitboard/slice his-bitboard 2)))))))


        (define (compute-covering-entering-moves)
          ;; (newline)
          ;; (display "Opponent has 3 in a row.")
          (append
           (if (or (zero? pile-0-top)
                   (= pile-0-top 1))
               '()
               (map (lambda (target)
                      (make-move1 0 target position))
                    (covering-entering-targets pile-0-top)))
           (if (or (zero? pile-1-top)
                   (= pile-1-top 1)
                   (= pile-1-top pile-0-top))
               '()
               (map (lambda (target)
                      (make-move1 1 target position))
                    (covering-entering-targets pile-1-top)))
           (if (or (zero? pile-2-top)
                   (= pile-2-top 1)
                   (= pile-2-top pile-1-top))
               '()
               (map (lambda (target)
                      (make-move1 2 target position))
                    (covering-entering-targets pile-2-top)))))

        (define (compute-entering-moves)
          ;; If your opponent has 3 gobblets set up in a visible
          ;; line on the board, you may gobble up one of the
          ;; 3 pieces in the line with a gobblet taken directly
          ;; from one of your external stacks.
          (if (three-in-row? his-exposed)
              (append! (compute-covering-entering-moves)
                       (compute-normal-entering-moves))
              (compute-normal-entering-moves)))

        (define (mapcan2 function list1 list2)
          (if (null? list1)
              '()
              (append! (function (car list1) (car list2))
                       (mapcan2 function (cdr list1) (cdr list2)))))

        (define (mapcan1 function list1)
          (if (null? list1)
              '()
              (append! (function (car list1))
                       (mapcan1 function (cdr list1)))))

        (define (compute-board-moves)
          (mapcan2 (lambda (source-list target-list)
                    (mapcan1 (lambda (source)
                               (map (lambda (target) (make-move1 source target position)) target-list))
                             source-list))
                  (list slice-3-source-list slice-2-source-list slice-1-source-list
                        (slice->coordinate-list slice-0-sources))
                  (list slice-3-target-list slice-2-target-list slice-1-target-list slice-0-target-list)))

        (cond ((four-in-row? my-exposed)
               ;(newline)
               ;(display "No legal moves, win for ")
               ;(display (whose-move position))
               '())
              ((four-in-row? his-exposed)
               ;(newline)
               ;(display "No legal moves, lose for ")
               ;(display (whose-move position))
               '())
              (else (append (compute-board-moves)
                            (compute-entering-moves))))))

    (if (eq? (whose-move position) 'white)
        (compute-moves1 (position-white-home position)
                        (position-white-bitboard position)
                        (position-black-bitboard position))
        (compute-moves1 (position-black-home position)
                        (position-black-bitboard position)
                        (position-white-bitboard position))))

  (define (position-moves position)
    (let* ((raw-moves (force (position-%moves position))))
      (map (lambda (move)
             (let ((result (do-move position move)))
               (list move (position-raw-evaluation result) result)))
           raw-moves)))

  (define (sorted-position-moves position less-than)
    (let* ((raw-moves (force (position-%moves position)))
           (tagged-moves (map (lambda (move)
                                (let ((result (do-move position move)))
                                  (list move (position-raw-evaluation result) result)))
                              raw-moves))
           (sorted (mergesort tagged-moves
                              (lambda (left right)
                                (less-than  (cadr left)
                                            (cadr right))))))
      sorted))

  (define (%do-move position source target)
    (if (pair? source)
        (position/move position
                       (encode-coordinates1 source)
                       (encode-coordinates1 target))
        (position/place position
                        source
                        (encode-coordinates1 target))))

  (define (do-move position move)
    (%do-move position (move-source move) (move-target move)))

  (define (random-move position)
    (let ((moves (position-moves position)))
      (car (list-ref moves (random (length moves))))))

  (define (random-game)
    (let loop ((pos (initial-position)))
      (show-position pos)
      (let ((moves (position-moves pos)))
        (if (null? moves)
            #f
            (let ((the-move (list-ref moves (random (length moves)))))
              (newline)
              (newline)
              (display (car the-move))
              (loop (caddr the-move)))))))

  (define (stacked-game white-strength black-strength)
    (let loop ((pos (initial-position))
               (score-1 0)
               (score-2 0))
      (show-position pos)
      (scrub-transposition-table! (position-half-turn pos))
      (let ((moves (sorted-position-moves pos (lambda (l r) (m> max l r)))))
        (if (null? moves)
            #f
            (let ((the-move
                   (if (< (position-half-turn pos) 6)
                       (cons 0 (cons (list-ref moves (random (length moves))) '()))
                       (if (eq? (whose-move pos) 'white)
                           (let* ((start epcount)
                                  (move (time (mtdf pos moves white-strength position-raw-evaluation
                                                    max min
                                                    *minimum-score* *maximum-score*
                                                    score-2
                                                    #t)))
                                  (end epcount))
                             (newline)
                             (display "Score:  ")
                             (display (car move))
                             (display ", Searched:  ")
                             (display (- end start))
                             move)
                           (let* ((start epcount)
                                  (move (time (mtdf pos (reverse moves) black-strength position-raw-evaluation
                                                    min max
                                                    *maximum-score* *minimum-score*
                                                    score-2
                                                    #t)))
                                  (end epcount))
                             (newline)
                             (display "Score:  ")
                             (display (car move))
                             (display ", Searched:  ")
                             (display (- end start))
                             move)))))
              (newline)
              (newline)
              (display the-move)
              (loop (caddr (cadr the-move)) (car the-move) score-1))))))

  (define *interactive-game* #f)
  (define *last-computer-score* 0)

  (define (start-interactive-game)
    (set! *interactive-game* (initial-position))
    (set! *last-computer-score* 0)
    (show-position *interactive-game*))

  (define (computer-move strength)
    (scrub-transposition-table! (position-half-turn *interactive-game*))
    (let ((moves (sorted-position-moves *interactive-game* (lambda (l r) (m> max l r)))))
      (if (null? moves)
          #f
          (let ((the-move
                 (if (eq? (whose-move *interactive-game*) 'white)
                     (let* ((start epcount)
                            (move (time (mtdf *interactive-game* moves strength position-raw-evaluation
                                              max min
                                              *minimum-score* *maximum-score*
                                              *last-computer-score*
                                              #t)))
                            (end epcount))
                       (newline)
                       (display (car move))
                       (display "(")
                       (display (- end start))
                       (display ")")
                       move)
                     (let* ((start epcount)
                            (move (time (mtdf *interactive-game* (reverse moves) strength position-raw-evaluation
                                              min max
                                              *maximum-score* *minimum-score*
                                              *last-computer-score*
                                              #t)))
                            (end epcount))
                       (newline)
                       (display (car move))
                       (display "(")
                       (display (- end start))
                       (display ")")
                       move))))
            (newline)
            (set! *interactive-game* (caddr (cadr the-move)))
            (set! *last-computer-score* (car the-move))
            (show-position *interactive-game*)))))

  (define (human-move source dest)
    (scrub-transposition-table! (position-half-turn *interactive-game*))
    (let loop ((moves (position-moves *interactive-game*)))
      (cond ((null? moves) (error "Illegal move" (list source dest)))
            ((and (equal? source (move-source (caar moves)))
                  (equal? dest (move-target (caar moves))))
             (set! *interactive-game* (do-move *interactive-game* (caar moves)))
             (show-position *interactive-game*))
            (else (loop (cdr moves))))))

  (define (random-game1)
    (let loop ((pos (initial-position))
               (mov   0)
               (count 0))
      (let ((moves (position-moves pos)))
        (if (null? moves)
            (cons count mov)
            (let* ((len (length moves))
                   (the-move (list-ref moves (random len))))
              (loop (do-move pos the-move) (+ mov len) (add1 count)))))))

  (define (benchmark n)
    (let ((pos (initial-position)))
      (time (let loop ((x 0))
              (if (< x n)
                  (begin (position-moves pos)
                         (loop (add1 x))))))))


  (provide
   encode-coordinates
   bitboard/dump-slice
   bitboard/set!
   bitboard/clear!
   bitboard/ref
   bitboard/or
   bitboard/and
   bitboard/xor
   bitboard-allocate
   initial-position
   show-position
   position-moves
   do-move
   position/place!
   position/clear!
   benchmark
   position/move
   position/place
   random-move
   random-game
   random-game1
   *transposition-table*
   hash-table-probe
   hash-table-miss
   evaluate-position
   pair-count
   stacked-game
   start-interactive-game
   human-move
   computer-move
   *interactive-game*
   ))
