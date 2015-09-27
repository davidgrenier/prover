(include-book "j-bob-lang" :dir :teachpacks)

(include-book "j-bob" :dir :teachpacks)

;(include-book "little-prover" :dir :teachpacks)

(defun defun.ctx? ()
  (J-Bob/define (prelude)
                '(((defun ctx? (x)
                     (if (atom x)
                         (equal x '?)
                         (if (ctx? (car x))
                             't
                             (ctx? (cdr x)))))
                   (size x)
                   ((Q) (natp/size x))
                   (() (if-true (if (atom x)
                                 't
                                 (if (< (size (car x)) (size x))
                                     (if (ctx? (car x))
                                         't
                                         (< (size (cdr x)) (size x)))
                                     '()))
                             '()))
                   ((E Q) (size/car x))
                   
                   ((E A E) (size/cdr x))
                   ((E A) (if-same (ctx? (car x)) 't))
                   ((E) (if-true 't '()))
                   (() (if-same (atom x) 't))))))

