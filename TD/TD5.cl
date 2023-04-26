
(setq *baseDeRegles* '( (F (B D E) R1)
		      (A (D G) R2)
		      (A (C F) R3)
		      (D (C) R4)
		      (E (D) R5)
		      (H (A) R6)
		      (X (B) R7)
		      (A (X C) R8))
      )

(setq *baseDefaits* '(b c))

;; Boucle à l'infini (mais non traité car méthode bourrin...)
;; (setq *baseDeRegles* '((A (B) R1)
;;                       (B (A) R2)
;;                       (H (A C) R3)))
;; (setq *baseDefaits* '(c))

(defun cclRegle (regle) (car regle))
(defun premisseRegle (regle) (cadr regle))
(defun numRegle (regle) (caddr regle))

(defun regles_candidates (but bdR)
  (let ((candidates nil))
    (dolist (regle bdR candidates)
      (if (EQUAL (cclRegle regle) but)
	  (push regle candidates))
	)
      )
  )

(regles_candidates 'A *baseDeRegles*)

;; version récursive
(defun regles_candidates (but bdR)
  (if bdR
      (if (EQUAL (cclRegle (car bdR)) but)
          (cons (car bdR) (regles_candidates but (cdr bdR)))
        (regles_candidates but (cdr bdR)))))

(regles_candidates 'A *baseDeRegles*)

;;;--------------------------------------------------------
;;;
;;;---------------- CHAINAGE ARRIERE ----------------------
;;;

;;;--------------------------------------------------------
;; Algo Verifier-ou (on regarde si une des regles est verifiee)
;; Si BUT est dans la base de fait
;;    Le but est prouvé 
;;    RENVOYER TRUE
;; Sinon
;;     INIT (Regles = toutes les regles candidates pour un but donne ; ok = nil)
;;     Pour chaque regle de Regles et TANT qu'aucune regle n'est verifiee (ok = nil)
;;          On verifie la premisse est prouvee (ok = (verifier-et (pop regles) ...))
;;     Renvoyer ok< (avec TRUE ou nil)

(defun verifier_ou (but bdF bdR &optional (i 0))
  (if (member but bdF) 
      (progn 
        (format t "~V@t But : ~A proof ~%" i but)
        T)
    ;;(let ((regles (regles_candidates but bdR)) (ok nil))
    (let ((regles (reverse (regles_candidates but bdR))) (ok nil))

     (while (and regles (not ok))
       (format t "~% ~V@t VERIFIE_OU ~A Regles ~s :  ~A ~%" i but (numRegle (car regles)) (car regles))
       (setq ok (verifier_et (pop regles) bdF bdR i)))
     ok)
    )) 

;; Algo Verifier-et (on regarde que tous les elements de la premisse de la regle passe en param sont vrais)
;; INIT (ok = true ; premisse = premisse de la regle)
;; Pour chaque élément de la premisse et TANT que les elements sont verifies (ok = true)
;;      On verifie que l'element est prouve (ok = (verifier-ou (pop premisse)...))
;; Renvoyer ok< (avec TRUE ou nil)

(defun verifier_et (regle bdF bdR i)
  (let ((ok t) (premisses (premisseRegle regle)))
    (while (and premisses ok)
      (format t "~V@t  ~t VERIFIE_ET ~s premisse ~A~%" (+ 1 i) (numRegle regle) (car premisses))
      (setq ok (verifier_ou (pop premisses) bdF bdR (+ 6 i))))
    ok))

(verifier_ou 'h *baseDefaits* *baseDeRegles*)
(verifier_ou 'g *baseDefaits* *baseDeRegles*)

(setq *baseDefaits* '(b))
(verifier_ou 'h *baseDefaits* *baseDeRegles*)

;;; Avec retour des règles qui mènent à la solution
;;;--------------------------------------------------------
;; Algo Verifier-ou (on regarde si une des regles est verifiee)
;; Si BUT est dans la base de fait
;;    Le but est prouvé 
;;    RENVOYER TRUE
;; Sinon
;;     INIT (Regles = toutes les regles candidates pour un but donne ; sol = nil)
;;     Pour chaque regle de Regles et TANT qu'aucune regle n'est verifiee (sol = nil)
;;          On verifie la premisse est prouvee (sol = (verifier-et (pop regles) ...))
;;          Si sol != nil
;;              Ajouter la regle ou numRegle à sol
;;     Renvoyer ok

(defun verifier_ou (but bdF bdR &optional (i 0))
  (if (member but bdF) 
      (progn 
        (format t "~V@t But : ~A proof ~%" i but)
        T)
    ;;(let ((regles (regles_candidates but bdR)) (sol nil))
    (let ((regles (reverse (regles_candidates but bdR))) (sol nil))

     (while (and regles (not sol))
       (format t "~% ~V@t VERIFIE_OU ~A Regles ~s :  ~A ~%" i but (numRegle (car regles)) (car regles))
       (setq sol (verifier_et (car regles) bdF bdR i))
       (if sol 
           (push (numRegle (car regles)) sol))
       (pop regles))
     sol)
    ))

;; Algo Verifier-et (on regarde que tous les elements de la premisse de la regle passe en param sont vrais)
;; INIT (ok = true ; premisse = premisse de la regle)
;; Pour chaque élément de la premisse et TANT que les elements sont verifies (ok = true)
;;      On verifie que l'element est prouve (ok = (verifier-ou (pop premisse)...))
;; Renvoyer ok< (avec TRUE ou nil)

(defun verifier_et (regle bdF bdR i)
  (let ((ok t) (premisses (premisseRegle regle)))
    (while (and premisses ok)
      (format t "~V@t  ~t VERIFIE_ET ~s premisse ~A~%" (+ 1 i) (numRegle regle) (car premisses))
      (setq ok (verifier_ou (pop premisses) bdF bdR (+ 6 i))))
    ok))

(verifier_ou 'h *baseDefaits* *baseDeRegles*)
(verifier_ou 'g *baseDefaits* *baseDeRegles*)

(setq *baseDefaits* '(b))
(verifier_ou 'h *baseDefaits* *baseDeRegles*)



;;;--------------------------------------------------------
;;;
;;;---------------- SOME EVERY ----------------------
;;;
;;;--------------------------------------------------------

;;; Verifier_ou => utiliser la mÃ©thode some, qui renvoie vrai si un Ã©lÃ©ment de la liste est vrais
 
(defun verifier_ou2 (but bdF bdR)
  (if (member but bdF) T
    (some #'(lambda (x)
	      (format t "~%VERIFIE_OU ~A ~%" but)
	      (verifier_et2 (premisseRegle x) bdF bdR)) (regles_candidates but bdR))
    )
  )

;;; Verifier_et => utiliser la mÃ©thode every, qui renvoie vrai si tous les Ã©lÃ©ments de la liste sont vrais

(defun verifier_et2 (regle bdF bdR)
  (format t "~tVERIFIE_ET ~A ~%" regle)
  (every #'(lambda(x) (verifier_ou2 x bdF bdR)) regle))

(verifier_ou2 'h *baseDefaits* *baseDeRegles*)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; une seule fonction ;;;

;; Algo 
;; Si BUT est dans la base de fait
;;    Le but est prouvé 
;;    RENVOYER TRUE
;; Sinon
;;     INIT (Regles = toutes les regles candidates pour un but donne ; ok = nil)
;;     Pour chaque regle de Regles et TANT qu'aucune regle n'est verifiee (ok = nil)
;;          INIT (premisse = premisse de la regle)
;;          ok = true 
;;          Pour chaque élément de la premisse et TANT que les elements sont verifies (ok = true)
;;               On verifie que l'element est prouve (ok = (verifier-ou (pop premisse)...))
;;     Renvoyer ok< (avec TRUE ou nil)

(defun chainage-arriere (but bdF bdR &optional (i 0))
  (if (member but bdF) 
      (progn 
        (format t "~V@t   But : ~A proof ~%" i but)
        T)
    (progn
      ;; (let ((regles (regles_candidates but bdR)) (ok nil))
      (let* ((regles (reverse (regles_candidates but bdR))) (ok nil))
        (while (and regles (not ok))
          (format t "~%~V@t VERIFIE_OU ~A Regles ~s :  ~A ~%" i but (numRegle (car regles)) (car regles))
          (let ((premisses (premisseRegle (pop regles))))
            (setq ok t)
            ;;(format t "premisses ~s~%" premisses)
            (while (and premisses ok)
              (format t "~V@t  ~t VERIFIE_ET premisse ~A~%" (+ 1 i) (car premisses))
              (setq ok (chainage-arriere (pop premisses) bdF bdR (+ 9 i))))))
     ok))
    ))


(chainage-arriere 'h *baseDefaits* *baseDeRegles*)
(chainage-arriere 'g *baseDefaits* *baseDeRegles*)

(setq *baseDefaits* '(b))
(chainage-arriere 'h *baseDefaits* *baseDeRegles*)

;; Meme chose mais renvoie les regles appliquees
;; Si BUT est dans la base de fait
;;    Le but est prouvé 
;;    RENVOYER TRUE
;; Sinon
;;     INIT (Regles = toutes les regles candidates pour un but donne ; sol = nil)
;;     Pour chaque regle de Regles et TANT qu'aucune regle n'est verifiee (sol = nil)
;;          INIT (premisse = premisse de la regle)
;;          sol = true 
;;          Pour chaque élément de la premisse et TANT que les elements sont verifies (ok = true)
;;               On verifie que l'element est prouve (ok = (verifier-ou (pop premisse)...))
;;          Si la premisse est prouvee
;;               Ajouter le numero de la regle à sol
;;     Renvoyer sol (avec TRUE ou nil)
(defun chainage-arriere (but bdF bdR &optional (i 0))
  (if (member but bdF) 
      (progn 
        (format t "~V@t   But : ~A proof ~%" i but)
        T)
    (progn
      ;;(let ((regles (reverse (regles_candidates but bdR))) (sol nil))
      (let ((regles (regles_candidates but bdR)) (sol nil))
      
        (while (and regles (not sol))
          (format t "~%~V@t VERIFIE_OU ~A Regles ~s :  ~A ~%" i but (numRegle (car regles)) (car regles))
          (let* ((premisses (premisseRegle (car regles))))
            (setq sol T)
            ;;(format t "premisses ~s~%" premisses)
            (while (and premisses sol)
              (format t "~V@t  ~t VERIFIE_ET premisse ~A~%" (+ 1 i) (car premisses))
              (setq sol (chainage-arriere (pop premisses) bdF bdR (+ 9 i))))
            (if sol 
                  (push (numRegle (car regles)) sol)
                  ))
            (pop regles))
     sol))
    ))

(setq reglesSolution (chainage-arriere 'h *baseDefaits* *baseDeRegles*))
reglesSolution

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; une seule fonction ;;;
;; mais Representation differente
;;    Ce qui change ce sont les fonctions de service 
;;       et surtout le regle candidates est plus simple avec un assoc

(setq *baseDeRegles* '( (F ((B D E) R1))
                       (A ((D G) R2)
                          ((C F) R3)
                          ((X C) R8))
		      (D ((C) R4))
		      (E ((D) R5))
		      (H ((A) R6))
                       (X ((B) R7))))



(setq *baseDefaits* '(b c))

(defun cclRegle (regle) (car regle))
(defun premisseRegle (pregle) (car pregle)) ;; ici on n'envoie pas du tout la conclusion pregle = petite regle
(defun numRegle (pregle) (cadr pregle))

(defun regles_candidates (but bdR)
  (cdr (assoc but bdR)))

(regles_candidates 'A *baseDeRegles*)

(defun chainage-arriere (but bdF bdR &optional (i 0))
  (if (member but bdF) 
      (progn 
        (format t "~V@t   But : ~A proof ~%" i but)
        T)
    (progn
      ;;(let ((regles (reverse (regles_candidates but bdR))) (sol nil))
      (let ((regles (regles_candidates but bdR)) (sol nil))
      
        (while (and regles (not sol))
          (format t "~%~V@t VERIFIE_OU ~A Regles ~s :  ~A ~%" i but (numRegle (car regles)) (car regles))
          (let ((premisses (premisseRegle (car regles))))
            (setq sol T)
            ;;(format t "premisses ~s~%" premisses)
            (while (and premisses sol)
              (format t "~V@t  ~t VERIFIE_ET premisse ~A~%" (+ 1 i) (car premisses))
              (setq sol (chainage-arriere (pop premisses) bdF bdR (+ 9 i))))
            (if sol 
                  (push (numRegle (car regles)) sol)
                  ))
            (pop regles))
     sol))
    ))

(setq reglesSolution (chainage-arriere 'h *baseDefaits* *baseDeRegles*))
reglesSolution

;;-------------------Ordre 0+ ------------------
;; on modifie la base de regle
(setq *basederegles*
      '((R1 ((>= d 5)) (eq moyen voiture))
        (R2 ((>= d 1)(< temperature 15)) (eq moyen voiture))
        (R3 ((>= d 1)(>= temperature 15)) (eq moyen a-pied))
        (R4 ((eq moyen voiture)(eq cinema ville)) (eq action taxi))
        (R5 ((eq moyen voiture)(neq cinema ville)) 
	    (eq action voiture-personnelle))
        (R6 ((eq moyen a-pied)(eq temps mauvais)) 
	    (eq action a-pied-avec-impermeable))
        (R7 ((eq moyen a-pied)(eq temps beau)) (eq action promenade))))

;; il faut modifier les fonctions de service (ordre modifie)
(defun cclRegle (regle) (caddr regle))
(defun premisseRegle (regle) (cadr regle))
(defun numRegle (regle) (car regle))


;; (setq *basedefaits* '((temps mauvais) (d 6) (cinema inconnu)))

;; On doit non seulement regarder si but est dans la base de fait mais savoir si le but est verifie
(defun appartient (but bdf)  ;; (> d 5)
  ;; but est de la forme: (comparateur attribut valeur)
  (let ((valeur (cadr (assoc (cadr but) bdf)))) ;; valeur = 6 (dans bdF)
    (if valeur
        (funcall (car but) valeur (caddr but))))) ;; le eval ne fonctionne pas avec eq : (eval (list (car but) valeur (caddr but))))))
;;        (eval (list (car but) valeur (caddr but)))))) ;; le eval ne fonctionne pas avec eq : (eval (list (car but) valeur (caddr but))))))

(setq *basedefaits* '((temps beau) (temperature 5) (D 6)))
(appartient '(eq temps mauvais) *basedefaits*) 
s
(setq *basedefaits* '((temps mauvais) (temperature 5) (D 6)))
(appartient '(eq temps mauvais) *basedefaits*) 

(appartient '(> d 4) *basedefaits*) 
(appartient '(> d 10) *basedefaits*) 
(appartient '(<= temperature 4) *basedefaits*) 
(appartient '(>= d 6) *basedefaits*) 
(appartient '(<= temperature 15) *basedefaits*) 

(appartient '(eq cinema ville) *basedefaits*) 


;;; Idem. on doit modifier les regles-candidates
(defun regles_candidates (but bdr) ;; (eq moyen voiture) // (> age 18) 
  ;;(print bdr)
  (if bdr
      (let* ((conclusion (cclRegle (car bdr))) ;; R1 (eq moyen voiture) // Rx (> age 20)
             (attribut (cadr conclusion)) ;; moyen  // age
             (valeur (caddr conclusion))) ;; voiture // 20

        (if 
          (and 
           ;; il faut verifier l identité des attributs
           (eq attribut (cadr but))  ;; eq moyen moyen // eq age age
           ;; et que la regle concluera sur une valeur correcte 
           (funcall (car but) valeur (caddr but)))  ;; eq voiture voiture // > 20 18 // ou (eval list (car but) valeur (caddr but))))
          (cons (car bdr) (regles_candidates but (cdr bdr))) ;; alors on push la règle R1 // RX
          (regles_candidates but (cdr bdr))))))

;;; Pour prendre en compte (+ d 1) ou (- d 1)
;;      Dans le else du if alors vérifier ;
;;           si > dans but alors chercher un + dans conclusion
;;           si < dans but alors chercher un - dans conclusion
;;      Et appliquer la règle plusieurs fois si néccessaire

(regles_candidates '(eq moyen voiture) basederegles)


(setq *basedefaits* '((temps mauvais) (d 6) (t 20)))

(defun verifier_ou (but bdF bdR &optional (i 0))
  (if (appartient but bdF) ;; ici on remplacer le member par appartient
      (progn 
        (format t "~V@t But : ~A proof ~%" i but)
        T)
    ;;(let ((regles (regles_candidates but bdR)) (ok nil))
    (let ((regles (reverse (regles_candidates but bdR))) (ok nil))

     (while (and regles (not ok))
       (format t "~% ~V@t VERIFIE_OU ~A Regles ~s :  ~A ~%" i but (numRegle (car regles)) (car regles))
       (setq ok (verifier_et (pop regles) bdF bdR i)))
     ok)
    ))


(defun verifier_et (regle bdF bdR i)
  (let ((ok t) (premisses (premisseRegle regle)))
    (while (and premisses ok)
      (format t "~V@t  ~t VERIFIE_ET ~s premisse ~A~%" (+ 1 i) (numRegle regle) (car premisses))
      (setq ok (verifier_ou (pop premisses) bdF bdR (+ 6 i))))
    ok))

(verifier_ou '(eq action a-pied-avec-impermeable) *baseDefaits* *baseDeRegles*)
(verifier_ou '(eq action promenade) *baseDefaits* *baseDeRegles*)


;; --------------------------------------
;; Avec question quand impasse 

(defun question-utilisateur (but bdf)
  (unless
      (eql :inconnu (cdr (assoc but bdf)))
      (format t "Connaissez vous la valeur de ~s ? si vous tapez la valeur, sinon tapez inconnu~%" but)
      (push (list but (read)) bdf))
  bdf)

(setq *basedefaits* '((d 6)))

(setq *basedefaits* (question-utilisateur 'cinema *basedefaits*))
*basedefaits*
(setq *basedefaits* (question-utilisateur 't *basedefaits*))
*basedefaits*

(setq *basedefaits* '((d 6)))

;; Modification Ajout de la question
(defun verifier_ou (but bdF bdR &optional (i 0))
  (let ((ok nil))
  (OR 
   (appartient but bdF) ;; ici on remplacer le member par appartient
   (let ((regles (reverse (regles_candidates but bdR))))
     ;;(format t "~% ~V@t Toto règles : ~s ~%" i regles)
     (while (and regles (not ok))
       (format t "~% ~V@t VERIFIE_OU ~A Regles ~s :  ~A ~%" i but (numRegle (car regles)) (car regles))
       (setq ok (verifier_et (pop regles) bdF bdR i)))
     ;;(format t "~% ~V@t Toto ok : ~s ~%" i ok)
  
     ok)
   (if (not ok)
       (progn
         (setq bdF (question-utilisateur (cadr but) bdF))
         (format t "~% ~V@t Toto bdf : ~s ~%" i bdF)

         (verifier_ou but bdF bdR i))
     ))))

;; Pas de modif
(defun verifier_et (regle bdF bdR i)
  (let ((ok t) (premisses (premisseRegle regle)))
    (while (and premisses ok)
      (format t "~V@t  ~t VERIFIE_ET ~s premisse ~A~%" (+ 1 i) (numRegle regle) (car premisses))
      (setq ok (verifier_ou (pop premisses) bdF bdR (+ 6 i))))
    ok))

(setq *basedefaits* '((d 6)))

(setq *basedefaits* (verifier_ou '(eq action taxi) *baseDefaits* *baseDeRegles*))
(setq *basedefaits* (verifier_ou '(eq action a-pied-avec-impermeable) *baseDefaits* *baseDeRegles*))



