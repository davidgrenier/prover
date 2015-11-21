#lang racket

(require (rename-in "j-bob-lang.rkt"
                    (s.car car)
                    (s.cdr cdr)
                    (s.< <)
                    (s.+ +)))
(require "j-bob-if.rkt")

(defun list0 () '())
(defun list0? (x) (equal x '()))

(defun list1 (x) (cons x (list0)))
(defun list1? (x)
  (if (atom x) 'nil (list0? (cdr x))))
(defun elem1 (xs) (car xs))

(defun list2 (x y) (cons x (list1 y)))
(defun list2? (x)
  (if (atom x) 'nil (list1? (cdr x))))
(defun elem2 (xs) (elem1 (cdr xs)))

(defun list3 (x y z) (cons x (list2 y z)))
(defun list3? (x)
  (if (atom x) 'nil (list2? (cdr x))))
(defun elem3 (xs) (elem2 (cdr xs)))

(defun tag (sym x) (cons sym x))
(defun tag? (sym x)
  (if (atom x)
    'nil
    (equal (car x) sym)))
(defun untag (x) (cdr x))

(defun member? (x ys)
  (if (atom ys)
    'nil
    (if (equal (car ys) x)
      't
      (member? x (cdr ys)))))

(defun quote-c (value)
  (tag 'quote (list1 value)))

(defun quote? (x)
  (if (tag? 'quote x)
    (list1? (untag x))
    'nil))

(defun quote.value (e)
  (elem1 (untag e)))

(defun if-c (Q A E) (tag 'if (list3 Q A E)))

(defun if? (x)
  (if (tag? 'if x)
    (list3? (untag x))
    'nil))

(defun if.Q (e) (elem1 (untag e)))
(defun if.A (e) (elem2 (untag e)))
(defun if.E (e) (elem3 (untag e)))

(defun app-c (name args) (cons name args))

(defun app? (x)
  (if (atom x)
    'nil
    (if (quote? x)
      'nil
      (if (if? x)
        'nil
        't))))

(defun app.name (e) (car e))
(defun app.args (e) (cdr e))

(defun var? (x)
  (if (equal x 't)
    'nil
    (if (equal x 'nil)
      'nil
      (if (natp x)
        'nil
        (atom x)))))

(defun defun-c (name formals body)
  (tag 'defun (list3 name formals body)))

(defun defun? (x)
  (if (tag? 'defun x)
    (list3? (untag x))
    'nil))

(defun.name (def) (elem1 (untag def)))
(defun.formals (def) (elem2 (untag def)))
(defun.body (def) (elem3 (untag def)))

(defun dethm-c (name formals body)
  (tag 'dethm (list3 name formals body)))

(defun dethm? (x)
  (if (tag? 'dethm x)
    (list3? (untag x))
    'nil))

(defun dethm.name (def) (elem1 (untag def)))
(defun dethm.formals (def) (elem2 (untag def)))
(defun dethm.body (def) (elem3 (untag def)))

(defun if-QAE (e)
  (list3 (if.Q e) (if.A e) (if.E e)))

(defun QAE-if (es)
  (if-c (elem1 es) (elem2 es) (elem3 es)))

(defun rator? (name)
  (member? name '(equal atom car cdr cons natp size + <)))

(defun rator.formals (rator)
  (if (member? rator '(atom car cdr natp size))
    '(x)
    (if (member? rator '(equal cons + <))
      '(x y)
      'nil)))

(defun def.name (def)
  (if (defun? def)
    (defun.name def)
    (if (dethm? def)
      (dethm.name def)
      def)))

(defun def.formals (def)
  (if (dethm? def)
    (dethm.formals def)
    (if (defun? def)
      (defun.formals def)
      '())))

(defun if-c-when-necessary (Q A E)
  (if (equal A E) A (if-c Q A E)))

(defun conjunction (es)
  (if (atom es)
    (quote-c 't)
    (if (atom (cdr es))
      (car es)
      (if-c (car es)
            (conjunction (cdr es))
            (quote-c 'nil)))))

(defun implication (es e)
  (if (atom es)
    e
    (if-c (car es)
          (implication (cdr es) e)
          (quote-c 't))))

(defun arity? (vars es)
  (if (atom vars)
    (atom es)
    (if (atom es)
      'nil
      (arity? (cdr vars) (cdr es)))))

(defun lookup (name defs)
  (if (atom defs)
    name
    (if (equal (def.name (car defs)) name)
      (car defs)
      (lookup name (cdr defs)))))

(defun undefined? (name defs)
  (if (var? name)
    (equal (lookup name defs) name)
    'nil))

(defun args-arity? (def args)
  (if (dethm? def)
    'nil
    (if (defun? def)
      (arity? (defun.formals def) args)
      (if (rator? def)
        (arity? (rator.formals def) args)
        'nil))))

(defun app-arity? (defs app)
  (args-arity? (lookup (app.name app) defs) (app.args app)))

(defun bound? (var vars)
  (if (equal vars 'any) 't (member? var vars)))

(defun all? (predicate xs)
  (if (atom xs)
    't
    (if (predicate (car xs))
      (all? predicate (cdr xs))
      'nil)))

(defun expr? (defs vars e)
  (if (var? e)
    (bound? e vars)
    (if (quote? e)
      't
      (if (if? e)
        (all? (lambda (e) (expr? defs vars e)) (if-QAE e))
        (if (app? e)
          (if (app-arity? defs e)
            (all? (lambda (e) (expr? defs vars e)) (app.args e))
            'nil)
          'nil)))))

(defun exprs? (defs vars es)
  (all? (lambda (e) (expr? defs vars e)) es))

(defun subset? (xs ys)
  (if (atom xs)
    't
    (if (member? (car xs) ys)
      (subset? (cdr xs) ys)
      'nil)))

(defun list-extend (xs x)
  (if (atom xs)
    (list1 x)
    (if (equal (car xs) x)
      xs
      (cons (car xs)
            (list-extend (cdr xs) x)))))

(defun list-union (xs ys)
  (if (atom ys)
    xs
    (list-union (list-extend xs (car ys))
                (cdr ys))))

(defun get-arg-from (n args from)
  (if (atom args)
    'nil
    (if (equal n from)
      (car args)
      (get-arg-from n (cdr args) (+ from '1)))))

(defun get-arg (n args)
  (get-arg-from n args '1))

(defun set-arg-from (n args y from)
  (if (atom args)
    '()
    (if (equal n from)
      (cons y (cdr args))
      (cons (car args)
            (set-arg-from n (cdr args) y (+ from '1))))))

(defun set-arg (n args y)
  (set-arg-from n args y '1))

(defun <=len-from (n args from)
  (if (atom args)
    'nil
    (if (equal n from)
      't
      (<=len-from n (cdr args) (+ from '1)))))

(defun <=len (n args)
  (if (< '0 n)
    (<=len-from n args '1)
    'nil))

(defun formals? (vars)
  (if (atom vars)
    't
    (if (var? (car vars))
      (if (member? (car vars) (cdr vars))
        'nil
        (formals? (cdr vars)))
      'nil)))

(defun direction? (dir)
  (if (natp dir)
    't
    (member? dir '(Q A E))))

(defun path? (path)
  (all? direction? path))

(defun quoted-exprs? (args)
  (all? quote? args))


