(include-book "j-bob-lang" :dir :teachpacks)

(include-book "j-bob" :dir :teachpacks)

(defun defun.member? ()
  (J-Bob/define (prelude)
                '(((defun member? (x ys)
                     (if (atom ys)
                         'nil
                         (if (equal x (car ys))
                             't
                             (member? x (cdr ys)))))
                   (size ys)
                   ((A E E) (size/cdr ys))
                   ((A E) (if-same (equal x (car ys)) 't))
                   ((A) (if-same (atom ys) 't))
                   ((Q) (natp/size ys))
                   (() (if-true 't 'nil))))))

(defun defun.set? ()
  (J-Bob/define (defun.member?)
                '(((defun set? (xs)
                     (if (atom xs)
                         't
                         (if (member? (car xs) (cdr xs))
                             'nil
                             (set? (cdr xs)))))
                   (size xs)
                   ((Q) (natp/size xs))
                   (() (if-true (if (atom xs)
                                    't
                                    (if (member? (car xs) (cdr xs))
                                        't
                                        (< (size (cdr xs)) (size xs))))
                                'nil))
                   ((E E) (size/cdr xs))
                   ((E) (if-same (member? (car xs) (cdr xs)) 't))
                   (() (if-same (atom xs) 't))))))

(defun defun.add-atoms ()
  (J-Bob/define (defun.set?)
                '(((defun add-atoms (x ys)
                     (if (atom x)
                         (if (member? x ys)
                             ys
                             (cons x ys))
                         (add-atoms (car x)
                                    (add-atoms (cdr x) ys))))
                   (size x)
                   ((Q) (natp/size x))
                   (() (if-true (if (atom x)
                                    't
                                    (if (< (size (car x)) (size x))
                                        (< (size (cdr x)) (size x))
                                        'nil))
                                'nil))
                   ((E A) (size/cdr x))
                   ((E Q) (size/car x))
                   ((E) (if-true 't 'nil))
                   (() (if-same (atom x) 't))))))

