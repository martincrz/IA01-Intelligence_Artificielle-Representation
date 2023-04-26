;; IA01 - TD02
;;;

;;
;; Ecriture d'une fonction deriv (ainsi que ses "sous-fonctions" permettant de dériver une expression
;; par rapport à l'une de ses variables.
;;

(defun deriv_term (terme var)
  "Dérivation d'un terme par rapport à une variable var."
  (if (equal var terme)
      1
    0
    )
  )

(defun deriv_add (expr var)
  "Dérivation d'une addition (ou soustraction)."
  (list 
   (car expr)
   (deriv (cadr expr) var)
   (deriv (caddr expr) var)
   )
  )

(defun deriv_mul (expr var)
  "Dérivation d'une multiplication."
  " *uv -> +(*u'v  *uv')"
  (list '+
        (list '*
              (deriv (cadr expr) var)
              (caddr expr)
              )
        (list '*
              (cadr expr)
              (deriv (caddr expr) var)
              )
        )
  )
(defun deriv_div (expr var)
  "Dérivation d'une division."
  (list '/
        (list '-
              (list '*
                    (deriv (cadr expr) var)
                    (caddr expr)
                    )
              (list '*
                    (cadr expr)
                    (deriv (caddr expr) var)
                    )
              )
        (list '* (caddr expr) (caddr expr))
        )
  )

(defun deriv_pui (expr var)
  "Dérivation d'une puissance."
  (list '* (caddr expr)
        (list '* (deriv (cadr expr) var)
              (list 'expt (cadr expr)
                    (list '- (caddr expr) 1)
                    )
              )
        )
  )

(defun deriv_exp (expr var)
  "Dérivation de la fonction exponentielle, exp."
  (list '* (deriv (cadr expr) var)
        (list 'exp (cadr expr))
        )
  )

(defun deriv_ln (expr var)
  "Dérivation de la fonction logarithme népérien, ln."
  (list '/ (deriv (cadr expr) var) (cadr expr))
  )
(defun deriv_cos (expr var)
  "Dérivation de la fonction cosinus."
  (list '* (deriv (cadr expr) var)
        (list '- 0
              (list 'sin (cadr expr))
              )
        )
  )

(defun deriv_sin (expr var)
  "Dérivation de la fonction sinus."
  (list '* (deriv (cadr expr) var)
        (list 'cos (cadr expr))
        )
  )

(defun deriv_tan (expr var)
  "Dérivation de la fonction tangente."
  (list '* (deriv (cadr expr) var)
        (list '+ 1 
              (list '*
                    (list 'tan (cadr expr))
                    (list 'tan (cadr expr))
                    )
              )
        )
  )
;;test de la fonction principale
(defun deriv (expr var)
  "Dérivation d'une expression expr par rapport à une variable var."
  (cond
   ((atom exp) (deriv_term expr var))
   ((or (equal (car expr) '+) (equal (car expr) '-)) (deriv_add expr var))
   ((equal (car expr) '*) (deriv_mul expr var))
   ((equal (car expr) '/) (deriv_div expr var))
   ((equal (car expr) '^) (deriv_pui expr var))
   ((equal (car expr) 'exp) (deriv_exp expr var))
   ((equal (car expr) 'ln) (deriv_ln expr var))
   ((equal (car expr) 'cos) (deriv_cos expr var))
   ((equal (car expr) 'sin) (deriv_sin expr var))
   ((equal (car expr) 'tan) (deriv_tan expr var))
   ))

(deriv '(+ x 3) 'x)
(deriv '(+ x x) 'x)
(deriv '(* x 3) 'x)
(deriv '(+ (* x 3) (- x 2)) 'x)
(deriv '(* (+ x 3) (- x 2)) 'x)
;; Passage en n-aire
;; -> Transformer en binaire
(defun transformBinaire (exp)
  ;; Verifies si liste car sinon pb avec length d'un atom
  (if (AND (listp exp) (> (length exp) 3))
      (list (car exp)
            (transformBinaire (cadr exp))
            (transformBinaire (append (list (car exp)) (cddr exp))))
    exp))

(transformBinaire '(* x 3 5 y))
;; Ecriture d'une fonction simpl (ainsi que ses "sous-fonctions") permettant de simplifier le résultat obtenu
;; précédement par la fonction deriv.

(defun simpl_num (expr)
  "Simplification d'un opération 'numérique', donc ne faisant intervenir que des nombres."
  (if (listp expr)
      (if (and (numberp (cadr expr)) (numberp (caddr expr)))
          (eval expr)
        expr
        )
    expr
    )
  )

;; (simpl_num '(+ 1 2))

(defun simpl_add (expr)
  "Simplification d'une addition."
  (cond
   ((equal (simpl (cadr expr)) 0) (simpl (caddr expr)))
   ((equal (simpl (caddr expr)) 0) (simpl (cadr expr)))
   (T (simpl_bin expr))
   )
  )

;;(simpl_add '(+ 1 0))
;; (simpl_add '(+ x 0))

(defun simpl_sou (expr)
  "Simplification d'une soustraction."
  (cond
   ((equal (simpl (caddr expr)) 0) (simpl (cadr expr)))
   (T (simpl_bin expr))
   )
  )

(defun simpl_mul (expr)
  "Simplification d'une multiplication."
  (cond
   ((equal (simpl (cadr expr)) 1) (simpl (caddr expr)))
   ((equal (simpl (caddr expr)) 1) (simpl (cadr expr)))
   ((equal (simpl (cadr expr)) 0) 0)
   ((equal (simpl (caddr expr)) 0) 0)
   (T (simpl_bin expr))
   )
  )

(defun simpl_div (expr)
  "Simplification d'une division."
  (cond
   ((equal (simpl (cadr expr)) 0) 0)
   ((equal (simpl (caddr expr)) 1) (simpl (cadr expr)))
   ((equal (simpl (caddr expr)) 0) NIL)
   (T (simpl_bin expr))
   )
  )
(defun simpl_pui (expr)
  "Simplification d'une puissance."
  (cond
   ((equal (simpl (caddr expr)) 0) 1)
   ((equal (simpl (caddr expr)) 1) (simpl (cadr expr)))
   (T (simpl_bin expr))
   )
  )

(defun simpl_exp_cos (expr)
  "Simplification des fonctions exponentielle et cosinus."
  (cond
   ((equal (simpl (cadr expr)) 0) 1)
   (T (simpl_una expr))
   )
  )

(defun simpl_ln (expr)
  "Simplification de la fonction logarithme népérien."
  (cond
   ((equal (simpl (cadr expr)) 1) 0)
   (T (simpl_una expr))
   )
  )

(defun simpl_sin_tan (expr)
  "Simplification des fonctions sinus et tangente."
  (cond
   ((equal (simpl (cadr expr)) 0) 0)
   (T (simpl_una expr))
   )
  )

(defun simpl_una (expr)
  "Simplification d'une fonction unaire."
  (list (car expr) (simpl (cadr expr)))
  )

(defun simpl_bin (expr)
  "Simplification d'une fonction binaire (pour redécomposer et appeler récursivement)."
  (list (car expr) (simpl (cadr expr)) (simpl (caddr expr)))
  )


(defun simpl (expr)
  "Simplification d'une expression expr obtenue par la fonction deriv."
  (if (listp expr)
      (simpl_num
       (cond
        ((equal (car expr) '+) (simpl_add expr))
        ((equal (car expr) '-) (simpl_sou expr))
        ((equal (car expr) '*) (simpl_mul expr))
        ((equal (car expr) '/) (simpl_div expr))
        ((equal (car expr) '^) (simpl_pui expr))
        ((equal (car expr) 'exp) (simpl_exp_cos expr))
        ((equal (car expr) 'ln) (simpl_ln expr))
        ((equal (car expr) 'cos) (simpl_exp_cos expr))
        ((equal (car expr) 'sin) (simpl_sin_tan expr))
        ((equal (car expr) 'tan) (simpl_sin_tan expr))
        )
       )
    expr
    )
  )
;; Ecriture d'une fonction deriv_simpl permettant de dériver formellement une expression par rapport à une
;; variable, puis de simplifier le résultat de cette dérivation pour obtenir une nouvelle expression lisible.
;;

(defun deriv_simpl (expr var)
  "Dérivation d'une expression expr par rapport à une variable var retournée sous la forme la plus simple."
  (simpl (deriv expr var))
  )

(simpl (deriv '(* (+ x 3) (- x 2)) 'x))
