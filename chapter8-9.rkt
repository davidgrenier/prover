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
                   ((A E E) (size/cdr xs))
                   ((A E) (if-same (member? (car xs) (cdr xs)) 't))
                   ((A) (if-same (atom xs) 't))
                   ((Q) (natp/size xs))
                   (() (if-true 't 'nil))))))

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
                   ((A E Q) (size/car x))
                   ((A E A) (size/cdr x))
                   ((A E) (if-true 't 'nil))
                   ((A) (if-same (atom x) 't))
                   ((Q) (natp/size x))
                   (() (if-true 't 'nil))))))

