;;; -*-Mode:LISP; Package:USER; Base:8 -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;USER FUNCTIONS:
; (DRAW-LINE initial-X initial-Y final-X final-Y &OPTIONAL mode)
; (DRAW-CIRCLE center-X center-Y radius &OPTIONAL mode)
; (DRAW-LINE-WITH-INTENSITY
;     initial-x initial-y final-x final-y &OPTIONAL (intensity TV-ALL-PLANES-MASK))
; (DRAW-CIRCLE-WITH-INTENSITY
;     center-X center-y radius &OPTIONAL (intensity TV-ALL-PLANES-MASK))
; (DRAW-ABSOLUTE-VECTOR-RASTER-COORDS x0 y0 xf yf &OPTIONAL (draw-mode DRAW-MODE))
; (DRAW-VECTOR-RASTER-COORDS x0 y0 dx dy &OPTIONAL (draw-mode DRAW-MODE))
; (DRAW-ABSOLUTE-VECTOR-RASTER-COORDS-WITH-INTENSITY
;       x0 y0 xf yf &OPTIONAL (intensity TV-ALL-PLANES-MASK))
; (DRAW-VECTOR-RASTER-COORDS-WITH-INTENSITY
;       x0 y0 dx dy &OPTIONAL (intensity TV-ALL-PLANES-MASK))
; (DRAW-ABSOLUTE-CIRCLE-RASTER-COORDS
;       x-center y-center x-circumference y-circumference &OPTIONAL (draw-mode DRAW-MODE))
; (DRAW-CIRCLE-RASTER-COORDS x-center y-center radius &OPTIONAL (draw-mode DRAW-MODE))
; (DRAW-ABSOLUTE-CIRCLE-RASTER-COORDS-WITH-INTENSITY
;       x-center y-center x-circumference y-circumference
;               &OPTIONAL (intensity TV-ALL-PLANES-MASK))
; (DRAW-CIRCLE-RASTER-COORDS-WITH-INTENSITY
;       x-center y-center radius &OPTIONAL (intensity TV-ALL-PLANES-MASK))

;USER VARIABLES:
; DRAW-MODE the default mode for when optional mode arg not given.
;   Valid modes are IOR, ANDC, XOR.  Default default is IOR.
; TV-DEFAULT-SCREEN
;
;The coordinate system is TV coordinates except that (0,0) is at the
;center, and Y increases upwards, except that entries with -RASTER-COORDS in their
;names indeed use them ( (0,0) in the top left, X increasing to the right, Y increasing
;downwards).
;Points not on the screen are clipped. (inefficiently)
;The algorithms are due to Horn.

(DECLARE (SETQ RETAIN-VARIABLE-NAMES-SWITCH 'ARGS)
         (SPECIAL DRAW-X0 DRAW-Y0 DRAW-NEGATE-X DRAW-NEGATE-Y DRAW-SWAP-X-Y DRAW-MODE
                  DRAW-X-OFFSET DRAW-Y-OFFSET TV-BUFFER))

(SETQ DRAW-MODE 'IOR
      DRAW-X-OFFSET (TRUNC (SCREEN-WIDTH TV-DEFAULT-SCREEN) 2)
      DRAW-Y-OFFSET (TRUNC (SCREEN-HEIGHT TV-DEFAULT-SCREEN) 2))

(DEFUN DRAW-LINE (DRAW-X0 DRAW-Y0 X Y &OPTIONAL (DRAW-MODE DRAW-MODE)
                  &AUX DRAW-NEGATE-X DRAW-NEGATE-Y DRAW-SWAP-X-Y A B)
  (SETQ A (- X DRAW-X0)
        B (- Y DRAW-Y0))
  (AND (< A 0)
       (SETQ A (MINUS A)
             DRAW-NEGATE-X T))
  (AND (< B 0)
       (SETQ B (MINUS B)
             DRAW-NEGATE-Y T))
  (AND (< A B)
       (SETQ A (PROG2 NIL B (SETQ B A))
             DRAW-SWAP-X-Y T))
  (SETQ X 0 Y 0)
  (DO ((S (MINUS (TRUNC A 2))))
      ((> X A))
    (DRAW-PLOT X Y)
    (SETQ S (+ S B)
          X (1+ X))
    (AND (> S 0)
         (SETQ S (- S A)
               Y (1+ Y)))))

(DEFUN DRAW-PLOT (X Y)
  (AND DRAW-SWAP-X-Y (SETQ X (PROG2 NIL Y (SETQ Y X))))
  (AND DRAW-NEGATE-Y (SETQ Y (MINUS Y)))
  (AND DRAW-NEGATE-X (SETQ X (MINUS X)))
  (SETQ X (+ X (+ DRAW-X0 DRAW-X-OFFSET)))
  (SETQ Y (- DRAW-Y-OFFSET (+ Y DRAW-Y0)))
  (AND (>= X 0) (< X (SCREEN-WIDTH TV-DEFAULT-SCREEN)) (>= Y 0) (< Y (SCREEN-HEIGHT TV-DEFAULT-SCREEN))
       (AS-2 (COND ((EQ DRAW-MODE 'IOR) 1)
                   ((EQ DRAW-MODE 'ANDC) 0)
                   ((EQ DRAW-MODE 'XOR) (1+ (AR-2 TV-BUFFER X Y)))
                   ((FERROR NIL "~S is an unrecognized DRAW-MODE" DRAW-MODE)))
             TV-BUFFER X Y)))

(DEFUN DRAW-LINE-WITH-INTENSITY (DRAW-X0 DRAW-Y0 X Y &OPTIONAL (INTENSITY TV-ALL-PLANES-MASK)
                  &AUX DRAW-NEGATE-X DRAW-NEGATE-Y DRAW-SWAP-X-Y A B)
  (SETQ A (- X DRAW-X0)
        B (- Y DRAW-Y0))
  (AND (< A 0)
       (SETQ A (MINUS A)
             DRAW-NEGATE-X T))
  (AND (< B 0)
       (SETQ B (MINUS B)
             DRAW-NEGATE-Y T))
  (AND (< A B)
       (SETQ A (PROG2 NIL B (SETQ B A))
             DRAW-SWAP-X-Y T))
  (SETQ X 0 Y 0)
  (DO ((S (MINUS (TRUNC A 2))))
      ((> X A))
    (DRAW-PLOT-WITH-INTENSITY X Y INTENSITY)
    (SETQ S (+ S B)
          X (1+ X))
    (AND (> S 0)
         (SETQ S (- S A)
               Y (1+ Y)))))

(DEFUN DRAW-PLOT-WITH-INTENSITY (X Y INTENSITY)
  (AND DRAW-SWAP-X-Y (SETQ X (PROG2 NIL Y (SETQ Y X))))
  (AND DRAW-NEGATE-Y (SETQ Y (MINUS Y)))
  (AND DRAW-NEGATE-X (SETQ X (MINUS X)))
  (SETQ X (+ X (+ DRAW-X0 DRAW-X-OFFSET)))
  (SETQ Y (- DRAW-Y-OFFSET (+ Y DRAW-Y0)))
  (AND (>= X 0) (< X (SCREEN-WIDTH TV-DEFAULT-SCREEN)) (>= Y 0) (< Y (SCREEN-HEIGHT TV-DEFAULT-SCREEN))
       (AS-2 INTENSITY
             TV-BUFFER-PIXELS X Y)))

(DEFUN DRAW-CIRCLE (DRAW-X0 DRAW-Y0 R &OPTIONAL (DRAW-MODE DRAW-MODE)
                    &AUX DRAW-NEGATE-X DRAW-NEGATE-Y DRAW-SWAP-X-Y)
  (DRAW-SECTOR R 0)                                     ;Colorless but tasteful
  (SETQ DRAW-SWAP-X-Y T)
  (DRAW-SECTOR R 1)
  (SETQ DRAW-NEGATE-Y T)
  (DRAW-SECTOR R 0)
  (SETQ DRAW-SWAP-X-Y NIL)
  (DRAW-SECTOR R 1)
  (SETQ DRAW-NEGATE-X T)
  (DRAW-SECTOR R 0)
  (SETQ DRAW-SWAP-X-Y T)
  (DRAW-SECTOR R 1)
  (SETQ DRAW-NEGATE-Y NIL)
  (DRAW-SECTOR R 0)
  (SETQ DRAW-SWAP-X-Y NIL)
  (DRAW-SECTOR R 1))

(DEFUN DRAW-SECTOR (R Y0)       ;Y0 ARG PREVENTS HITTING SAME POINT TWICE
  (DO ((X R)                    ;UGGH!  BLETCH!!
       (Y Y0 (1+ Y))
       (S (MINUS R)))
      ((>= Y X)
       (AND (> Y0 0)
            (DRAW-PLOT X Y)))
    (DRAW-PLOT X Y)
    (SETQ S (+ S (1+ (* 2 Y))))
    (AND (> S 0)
         (SETQ S (- S (* 2 (SETQ X (1- X))))))))

(DEFUN DRAW-CIRCLE-WITH-INTENSITY (DRAW-X0 DRAW-Y0 R &OPTIONAL (INTENSITY TV-ALL-PLANES-MASK)
                    &AUX DRAW-NEGATE-X DRAW-NEGATE-Y DRAW-SWAP-X-Y)
  (DRAW-SECTOR-WITH-INTENSITY R 0 INTENSITY)                    ;Colorless but tasteful
  (SETQ DRAW-SWAP-X-Y T)
  (DRAW-SECTOR-WITH-INTENSITY R 1 INTENSITY)
  (SETQ DRAW-NEGATE-Y T)
  (DRAW-SECTOR-WITH-INTENSITY R 0 INTENSITY)
  (SETQ DRAW-SWAP-X-Y NIL)
  (DRAW-SECTOR-WITH-INTENSITY R 1 INTENSITY)
  (SETQ DRAW-NEGATE-X T)
  (DRAW-SECTOR-WITH-INTENSITY R 0 INTENSITY)
  (SETQ DRAW-SWAP-X-Y T)
  (DRAW-SECTOR-WITH-INTENSITY R 1 INTENSITY)
  (SETQ DRAW-NEGATE-Y NIL)
  (DRAW-SECTOR-WITH-INTENSITY R 0 INTENSITY)
  (SETQ DRAW-SWAP-X-Y NIL)
  (DRAW-SECTOR-WITH-INTENSITY R 1 INTENSITY))

(DEFUN DRAW-SECTOR-WITH-INTENSITY (R Y0 INTENSITY) ;Y0 ARG PREVENTS HITTING SAME POINT TWICE
  (DO ((X R)                    ;UGGH!  BLETCH!!
       (Y Y0 (1+ Y))
       (S (MINUS R)))
      ((>= Y X)
       (AND (> Y0 0)
            (DRAW-PLOT-WITH-INTENSITY X Y INTENSITY)))
    (DRAW-PLOT-WITH-INTENSITY X Y INTENSITY)
    (SETQ S (+ S (1+ (* 2 Y))))
    (AND (> S 0)
         (SETQ S (- S (* 2 (SETQ X (1- X))))))))

;DEFINITELY DOES NOT HIT FIRST GIVEN POINT
(DEFUN DRAW-ABSOLUTE-VECTOR-RASTER-COORDS (X0 Y0 XF YF &OPTIONAL (DRAW-MODE DRAW-MODE))
  (DRAW-VECTOR-RASTER-COORDS X0 Y0 (- XF X0) (- YF Y0) DRAW-MODE))

;DEFINITELY DOES NOT HIT STARTING POINT
(DEFUN DRAW-VECTOR-RASTER-COORDS (X0 Y0 DX DY &OPTIONAL (DRAW-MODE DRAW-MODE))
  ;SIMPLE-MINDED DDA VECTOR DRAWER USES RASTER COORDS
  (PROG (XSUM YSUM XINC YINC XSTEP YSTEP STEPS)
        (COND ((AND (ZEROP DX)
                    (ZEROP DY))
               (RETURN NIL)))
        (SETQ XSTEP (COND ((< DX 0) -1) (T 1)))
        (SETQ YSTEP (COND ((< DY 0) -1) (T 1)))
        (SETQ XSUM (SETQ YSUM 4000))
        (SETQ STEPS (MAX (SETQ DX (ABS DX)) (SETQ DY (ABS DY))))
        (SETQ XINC (TRUNC (* 10000 DX) STEPS))
        (SETQ YINC (TRUNC (* 10000 DY) STEPS))
  L     (COND ((< (SETQ STEPS (1- STEPS)) 0) (RETURN T)))
        (COND ((NOT (< (SETQ XSUM (+ XSUM XINC)) 10000))
                (SETQ X0 (+ X0 XSTEP))
                (SETQ XSUM (- XSUM 10000))))
        (COND ((NOT (< (SETQ YSUM (+ YSUM YINC)) 10000))
                (SETQ Y0 (+ Y0 YSTEP))
                (SETQ YSUM (- YSUM 10000))))
        (AND (>= X0 0) (< X0 (SCREEN-WIDTH TV-DEFAULT-SCREEN))
             (>= Y0 0) (< Y0 (SCREEN-HEIGHT TV-DEFAULT-SCREEN))
             (AS-2 (COND ((EQ DRAW-MODE 'IOR) 1)
                         ((EQ DRAW-MODE 'ANDC) 0)
                         ((EQ DRAW-MODE 'XOR) (1+ (AR-2 TV-BUFFER X0 Y0)))
                         ((FERROR NIL "~S is an unrecognized DRAW-MODE" DRAW-MODE)))
                   TV-BUFFER X0 Y0))
        (GO L)))

;DEFINITELY DOES NOT HIT FIRST GIVEN POINT
(DEFUN DRAW-ABSOLUTE-VECTOR-RASTER-COORDS-WITH-INTENSITY
        (X0 Y0 XF YF &OPTIONAL (INTENSITY TV-ALL-PLANES-MASK))
  (DRAW-VECTOR-RASTER-COORDS-WITH-INTENSITY X0 Y0 (- XF X0) (- YF Y0) INTENSITY))

;DEFINITELY DOES NOT HIT STARTING POINT
(DEFUN DRAW-VECTOR-RASTER-COORDS-WITH-INTENSITY
       (X0 Y0 DX DY &OPTIONAL (INTENSITY TV-ALL-PLANES-MASK))
  ;SIMPLE-MINDED DDA VECTOR DRAWER USES RASTER COORDS
  (PROG (XSUM YSUM XINC YINC XSTEP YSTEP STEPS)
        (COND ((AND (ZEROP DX)
                    (ZEROP DY))
               (RETURN NIL)))
        (SETQ XSTEP (COND ((< DX 0) -1) (T 1)))
        (SETQ YSTEP (COND ((< DY 0) -1) (T 1)))
        (SETQ XSUM (SETQ YSUM 4000))
        (SETQ STEPS (MAX (SETQ DX (ABS DX)) (SETQ DY (ABS DY))))
        (SETQ XINC (TRUNC (* 10000 DX) STEPS))
        (SETQ YINC (TRUNC (* 10000 DY) STEPS))
  L     (COND ((< (SETQ STEPS (1- STEPS)) 0) (RETURN T)))
        (COND ((NOT (< (SETQ XSUM (+ XSUM XINC)) 10000))
                (SETQ X0 (+ X0 XSTEP))
                (SETQ XSUM (- XSUM 10000))))
        (COND ((NOT (< (SETQ YSUM (+ YSUM YINC)) 10000))
                (SETQ Y0 (+ Y0 YSTEP))
                (SETQ YSUM (- YSUM 10000))))
        (AND (>= X0 0) (< X0 (SCREEN-WIDTH TV-DEFAULT-SCREEN))
             (>= Y0 0) (< Y0 (SCREEN-HEIGHT TV-DEFAULT-SCREEN))
             (AS-2 INTENSITY
                   TV-BUFFER-PIXELS X0 Y0))
        (GO L)))

;USER FUNCTION:
; (DRAW-CIRCLE-RASTER-COORDS center-X center-Y radius &OPTIONAL mode)
;USER VARIABLES:
; DRAW-MODE the default mode for when optional mode arg not given.
;   Valid modes are IOR, ANDC, XOR.  Default default is IOR.
;
;The coordinate system is TV coordinates.
;Points not on the screen are clipped. (inefficiently)
;The algorithms are due to Horn.

(DEFUN DRAW-PLOT-RASTER-COORDS (X Y)
  (AND DRAW-SWAP-X-Y (SETQ X (PROG2 NIL Y (SETQ Y X))))
  (AND DRAW-NEGATE-Y (SETQ Y (MINUS Y)))
  (AND DRAW-NEGATE-X (SETQ X (MINUS X)))
  (SETQ X (+ X DRAW-X0))
  (SETQ Y (+ Y DRAW-Y0))
  (AND (>= X 0) (< X (SCREEN-WIDTH TV-DEFAULT-SCREEN)) (>= Y 0) (< Y (SCREEN-HEIGHT TV-DEFAULT-SCREEN))
       (AS-2 (COND ((EQ DRAW-MODE 'IOR) 1)
                   ((EQ DRAW-MODE 'ANDC) 0)
                   ((EQ DRAW-MODE 'XOR) (1+ (AR-2 TV-BUFFER X Y)))
                   ((FERROR NIL "~S is an unrecognized DRAW-MODE" DRAW-MODE)))
             TV-BUFFER X Y)))

(DEFUN DRAW-PLOT-RASTER-COORDS-WITH-INTENSITY (X Y INTENSITY)
  (AND DRAW-SWAP-X-Y (SETQ X (PROG2 NIL Y (SETQ Y X))))
  (AND DRAW-NEGATE-Y (SETQ Y (MINUS Y)))
  (AND DRAW-NEGATE-X (SETQ X (MINUS X)))
  (SETQ X (+ X DRAW-X0))
  (SETQ Y (+ Y DRAW-Y0))
  (AND (>= X 0) (< X (SCREEN-WIDTH TV-DEFAULT-SCREEN)) (>= Y 0) (< Y (SCREEN-HEIGHT TV-DEFAULT-SCREEN))
       (AS-2 INTENSITY
             TV-BUFFER-PIXELS X Y)))

(DEFUN DRAW-ABSOLUTE-CIRCLE-RASTER-COORDS (X0 Y0 XC YC &OPTIONAL (DRAW-MODE DRAW-MODE)
                    &AUX MDX MDY R)
       (SETQ MDX (ABS (- XC X0)) MDY (ABS (- YC Y0)))
       (SETQ R (COND ((> MDX MDY) (+ MDX (LSH MDY -1)))
                     (T (+ MDY (LSH MDX -1)))))
       (DRAW-CIRCLE-RASTER-COORDS X0 Y0 R DRAW-MODE))

(DEFUN DRAW-CIRCLE-RASTER-COORDS (DRAW-X0 DRAW-Y0 R &OPTIONAL (DRAW-MODE DRAW-MODE)
                    &AUX DRAW-NEGATE-X DRAW-NEGATE-Y DRAW-SWAP-X-Y)
  (DRAW-SECTOR-RASTER-COORDS R 0)                                       ;Colorless but tasteful
  (SETQ DRAW-SWAP-X-Y T)
  (DRAW-SECTOR-RASTER-COORDS R 1)
  (SETQ DRAW-NEGATE-Y T)
  (DRAW-SECTOR-RASTER-COORDS R 0)
  (SETQ DRAW-SWAP-X-Y NIL)
  (DRAW-SECTOR-RASTER-COORDS R 1)
  (SETQ DRAW-NEGATE-X T)
  (DRAW-SECTOR-RASTER-COORDS R 0)
  (SETQ DRAW-SWAP-X-Y T)
  (DRAW-SECTOR-RASTER-COORDS R 1)
  (SETQ DRAW-NEGATE-Y NIL)
  (DRAW-SECTOR-RASTER-COORDS R 0)
  (SETQ DRAW-SWAP-X-Y NIL)
  (DRAW-SECTOR-RASTER-COORDS R 1))

(DEFUN DRAW-SECTOR-RASTER-COORDS (R Y0) ;Y0 ARG PREVENTS HITTING SAME POINT TWICE
  (DO ((X R)                    ;UGGH!  BLETCH!!
       (Y Y0 (1+ Y))
       (S (MINUS R)))
      ((>= Y X)
       (AND (> Y0 0)
            (DRAW-PLOT-RASTER-COORDS X Y)))
    (DRAW-PLOT-RASTER-COORDS X Y)
    (SETQ S (+ S (1+ (* 2 Y))))
    (AND (> S 0)
         (SETQ S (- S (* 2 (SETQ X (1- X))))))))

(DEFUN DRAW-ABSOLUTE-CIRCLE-RASTER-COORDS-WITH-INTENSITY
         (X0 Y0 XC YC &OPTIONAL (INTENSITY TV-ALL-PLANES-MASK)
                    &AUX MDX MDY R)
       (SETQ MDX (ABS (- XC X0)) MDY (ABS (- YC Y0)))
       (SETQ R (COND ((> MDX MDY) (+ MDX (LSH MDY -1)))
                     (T (+ MDY (LSH MDX -1)))))
       (DRAW-CIRCLE-RASTER-COORDS-WITH-INTENSITY X0 Y0 R INTENSITY))

(DEFUN DRAW-CIRCLE-RASTER-COORDS-WITH-INTENSITY
         (DRAW-X0 DRAW-Y0 R &OPTIONAL (INTENSITY TV-ALL-PLANES-MASK)
                    &AUX DRAW-NEGATE-X DRAW-NEGATE-Y DRAW-SWAP-X-Y)
  (DRAW-SECTOR-RASTER-COORDS-WITH-INTENSITY R 0 INTENSITY)              ;Colorless but tasteful
  (SETQ DRAW-SWAP-X-Y T)
  (DRAW-SECTOR-RASTER-COORDS-WITH-INTENSITY R 1 INTENSITY)
  (SETQ DRAW-NEGATE-Y T)
  (DRAW-SECTOR-RASTER-COORDS-WITH-INTENSITY R 0 INTENSITY)
  (SETQ DRAW-SWAP-X-Y NIL)
  (DRAW-SECTOR-RASTER-COORDS-WITH-INTENSITY R 1 INTENSITY)
  (SETQ DRAW-NEGATE-X T)
  (DRAW-SECTOR-RASTER-COORDS-WITH-INTENSITY R 0 INTENSITY)
  (SETQ DRAW-SWAP-X-Y T)
  (DRAW-SECTOR-RASTER-COORDS-WITH-INTENSITY R 1 INTENSITY)
  (SETQ DRAW-NEGATE-Y NIL)
  (DRAW-SECTOR-RASTER-COORDS-WITH-INTENSITY R 0 INTENSITY)
  (SETQ DRAW-SWAP-X-Y NIL)
  (DRAW-SECTOR-RASTER-COORDS-WITH-INTENSITY R 1 INTENSITY))

(DEFUN DRAW-SECTOR-RASTER-COORDS-WITH-INTENSITY (R Y0 INTENSITY)
        ;Y0 ARG PREVENTS HITTING SAME POINT TWICE
  (DO ((X R)                    ;UGGH!  BLETCH!!
       (Y Y0 (1+ Y))
       (S (MINUS R)))
      ((>= Y X)
       (AND (> Y0 0)
            (DRAW-PLOT-RASTER-COORDS-WITH-INTENSITY X Y INTENSITY)))
    (DRAW-PLOT-RASTER-COORDS-WITH-INTENSITY X Y INTENSITY)
    (SETQ S (+ S (1+ (* 2 Y))))
    (AND (> S 0)
         (SETQ S (- S (* 2 (SETQ X (1- X))))))))
