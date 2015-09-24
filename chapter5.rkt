(include-book "j-bob-lang" :dir :teachpacks)

(include-book "j-bob" :dir :teachpacks)

;(include-book "little-prover" :dir :teachpacks)

(defun defun.memb? ()
  (J-Bob/define (prelude)
                '(((defun memb? (xs)
                     (if (atom xs)
                         'nil
                         (if (equal (car xs) '?)
                             't
                             (memb? (cdr xs)))))
                   (size xs)
                   ((Q) (natp/size xs))
                   (() (if-true (if (atom xs)
                                    't
                                    (if (equal (car xs) '?)
                                        't
                                        (< (size (cdr xs)) (size xs)))) 'nil))
                   
                   ((E E) (size/cdr xs))
                   ((E) (if-same (equal (car xs) '?) 't))
                   (() (if-same (atom xs) 't))))))

(defun defun.remb ()
  (J-Bob/define (defun.memb?)
                '(((defun remb (xs)
                     (if (atom xs)
                         '()
                         (if (equal (car xs) '?)
                             (remb (cdr xs))
                             (cons (car xs) (remb (cdr xs))))))
                   (size xs)
                   ((Q) (natp/size xs))
                   (() (if-true (if (atom xs)
                                    't
                                    (< (size (cdr xs)) (size xs)))
                                'nil))
                   ((E) (size/cdr xs))
                   (() (if-same (atom xs) 't))))))

(defun dethm.memb?/remb0 ()
  (J-Bob/define (defun.remb)
                '(((dethm memb?/remb0 ()
                          (equal (memb? (remb '())) 'nil))
                   nil
                   ((1 1) (remb '()))
                   ((1 1 Q) (atom '()))
                   ((1 1) (if-true '() (if (equal (car '()) '?)
                                           (remb (cdr '()))
                                           (cons (car '()) (remb (cdr '()))))))
                   ((1) (memb? '()))
                   ((1 Q) (atom '()))
                   ((1) (if-true 'nil (if (equal (car '()) '?)
                                          't
                                          (memb? (cdr '())))))
                   (() (equal 'nil 'nil))))))

(defun defun.memb?/remb1 ()
  (J-Bob/define (dethm.memb?/remb0)
                '(((dethm memb?/remb1 (x1)
                          (equal (memb? (remb (cons x1 '()))) 'nil))
                   nil
                   ((1 1) (remb (cons x1 '())))
                   ((1 1 Q) (atom/cons (cons x1 '())))
                   ((1 1) (if-false '() (if (equal (car (cons x1 '()) '?))
                                            (remb (cdr (cons x1 '())))
                                            (cons (car (cons x1 '())) (remb (cdr (cons x1 '())))))))
                   ((1 1 Q 1) (car/cons x1 '()))
                   ((1 1 A 1) (cdr/cons x1 '()))
                   ((1 1 E 1) (car/cons x1 '()))
                   ((1 1 E 2 1) (cdr/cons x1 '()))
                   ((1) (if-same (equal x1 '?) (memb? (if (equal x1 '?)
                                                          (remb '())
                                                          (cons x1 (remb '()))))))
                   ((A 1) (if-nest-A (equal x1 '?) (remb '()) (cons x1 (remb '()))))
                   ((E 1) (if-nest-E (equal x1 '?) (remb '()) (cons x1 (remb '()))))
                   ((A) (memb?/remb0))
                   