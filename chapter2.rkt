(include-book "j-bob-lang" :dir :teachpacks)

(include-book "j-bob" :dir :teachpacks)

(include-book "little-prover" :dir :teachpacks)

(defun c2.1 ()
  (J-Bob/step (prelude)
              '(if (car (cons a b)) c c)
              '((() (if-same (car (cons a b)) c)))))

(defun c2.1b ()
  (J-Bob/step (prelude)
              'c
              '((() (if-same 
                     (if (equal a 't)
                         (if (equal 'nil 'nil) a b)
                         (equal 'or (cons 'black '(coffee))))
                     c))
                ((Q E 2) (cons 'black '(coffee)))
                ((Q A Q) (equal-same 'nil))
                ((Q A) (if-true a b))
                ((Q A) (equal-if a 't)))))

(defun brillig (x) (x))
(defun slithy (x) (x))
(defun mimsy (x) (x))
(defun mome (x) (x))
(defun uffish (x) (x))
(defun frumious (x) (x))
(defun frabjous (x) (x))

(defun extended ()
  (cons
  '(dethm jabberwocky (x)
       (if (brillig x)
           (if (slithy x)
               (equal (mimsy x) 'borogove)
               (equal (mome x) 'rath))
           (if (uffish x)
               (equal (frumious x) 'bandersnatch)
               (equal (frabjous x) 'beamish))))
  (prelude)))

(defun c2.2 ()
  (J-Bob/step (extended)
              '(cons 'gyre
                     (if (uffish '(callooh callay))
                         (cons 'gimble
                               (if (brillig '(callooh callay))
                                   (cons 'borogrove '(outgrabe))
                                   (cons 'bandersnatch '(wave))))
                         (cons (frabjous '(callooh callay)) '(vorpal))))
              '(((2 A 2 E 1) (jabberwocky '(callooh callay))))))