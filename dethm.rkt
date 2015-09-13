(include-book "j-bob-lang" :dir :teachpacks)

(include-book "j-bob" :dir :teachpacks)

(include-book "little-prover" :dir :teachpacks)

(defun c1.1 ()
  (J-Bob/step (prelude)
              '(car (cons 'ham '(eggs)))
              '((() (car/cons 'ham '(eggs))))))

(defun c1.2 ()
  (J-Bob/step (prelude)
              '(atom '())
              '((() (atom '())))))

(defun c1.3 ()
  (J-Bob/step (prelude)
              '(atom (cons 'ham '(eggs)))
              '((() (atom/cons 'ham '(eggs))))))

(defun c1.4 ()
  (J-Bob/step (prelude)
              '(atom (cons a b))
              '((() (atom/cons a b)))))

(defun c1.5 ()
  (J-Bob/step (prelude)
              '(equal 'flapjack (atom (cons a b)))
              '(((2) (atom/cons a b))
                (() (equal 'flapjack 'nil)))))

(defun c1.6 ()
  (J-Bob/step (prelude)
              '(atom (cdr (cons (car (cons p q)) '())))
              '(((1) (cdr/cons (car (cons p q)) '()))
                (() (atom '())))))

(defun c1.7 ()
  (J-Bob/step (prelude)
              '(car (cons (equal (cons x y) (cons x y)) '(and crumpets)))
              '(((1 1) (equal-same (cons x y)))
                (() (car/cons 't '(and crumpets))))))

(defun c1.8 ()
  (J-Bob/step (prelude)
              '(equal (equal (cons x y) (cons 'bagels '(and lox)))
                      (equal (cons 'bagels '(and lox)) (cons x y)))
              '(((1) (equal-swap (cons x y) (cons 'bagels '(and lox))))
                (() (equal-same (equal (cons 'bagels '(and lox)) (cons x y)))))))

(defun c1.9 ()
  (J-Bob/step (prelude)
              '(cons y
                     (equal (car (cons (cdr x) (car y)))
                            (equal (atom x) 'nil)))
              ;'(((2 1) (car/cons (cdr x) (car y))))))
              '(((2 1) (car/cons (car (cons (çdr x) (car y))) '