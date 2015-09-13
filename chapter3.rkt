(include-book "j-bob-lang" :dir :teachpacks)

(include-book "j-bob" :dir :teachpacks)

;(include-book "little-prover" :dir :teachpacks)

(defun defun.pair ()
  (J-Bob/define (prelude)
                '(((defun pair (x y)
                     (cons x (cons y '())))
                   nil))))

(defun defun.first-of ()
  (J-Bob/define (defun.pair)
                '(((defun first-of (x)
                     (car x))
                   nil))))

(defun defun.second-of ()
  (J-Bob/define (defun.first-of)
                '(((defun second-of (x)
                     (car (cdr x)))
                   nil))))

(defun dethm.first-of-pair ()
  (J-Bob/define (defun.second-of)
                '(((dethm first-of-pair (a b)
                          (equal (first-of (pair a b)) a))
                   nil
                   ((1 1) (pair a b))
                   ((1) (first-of (cons a (cons b '()))))
                   ((1) (car/cons a (cons b '())))
                   (() (equal-same a))))))

(defun dethm.second-of-pair ()
  (J-Bob/define (dethm.first-of-pair)
                '(((dethm second-of-pair (a b)
                     (equal (second-of (pair a b)) b))
                   nil
                   ((1 1) (pair a b))
                   ((1) (second-of (cons a (cons b '()))))
                   ((1 1) (cdr/cons a (cons b '())))
                   ((1) (car/cons b '()))
                   (() (equal-same b))))))

(defun defun.in-pair? ()
  (J-Bob/define (dethm.second-of-pair)
                '(((defun in-pair? (xs)
                     (if (equal (first-of xs) '?)
                         't
                         (equal (second-of xs) '?)))
                   nil))))

(defun dethm.in-first-of-pair ()
  (J-Bob/define (defun.in-pair?)
                '(((dethm in-first-of-pair (b)
                          (equal (in-pair? (pair '? b)) 't))
                   nil
                   ((1) (in-pair? (pair '? b)))
                   ((1 Q 1) (first-of-pair '? b))
                   ((1 Q) (equal-same '?))
                   ((1) (if-true 't (equal (second-of (pair '? b)) '?)))
                   (() (equal-same 't))))))

(defun dethm.in-second-of-pair ()
  (J-Bob/define (dethm.in-first-of-pair)
                '(((dethm in-second-of-pair (a)
                          (equal (in-pair? (pair a '?)) 't))
                   nil
                   ((1 1) (pair a '?))
                   ((1) (in-pair? (cons a (cons '? '()))))
                   ((1 E 1) (second-of (cons a (cons '? '()))))
                   ((1 E 1 1) (cdr/cons a (cons '? '())))
                   ((1 E 1) (car/cons '? '()))
                   ((1 E) (equal-same '?))
                   ((1) (if-same (equal (first-of (cons a (cons '? '()))) '?) 't))
                   (() (equal-same 't))))))