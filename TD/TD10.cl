(setq Chiffres '((0 (1 1 1 1 1 1 1 0) 0) ;; nb / x0...x7 / C res attendu
                 (1 (1 0 1 1 0 0 0 0) 1)
                 (2 (1 1 1 0 1 1 0 1) 0)
                 (3 (1 1 1 1 1 0 0 1) 1)
                 (4 (1 0 1 1 0 0 1 1) 0)
                 (5 (1 1 0 1 1 0 1 1) 1)
                 (6 (1 0 0 1 1 1 1 1) 0)
                 (7 (1 1 1 1 0 0 0 0) 1)
                 (8 (1 1 1 1 1 1 1 1) 0)
                 (9 (1 1 1 1 0 0 1 1) 1)))

(setq Poids '(1 1 1 1 1 1 1 1))
 
(defun Affichage (W X Os) ; W = poid, X entrée, Os = sortie calcule (Affichage proposé par Joseph)
  (format t "----- ~%Nombre : ~s ~% représentation ~s ~% poids : ~s ~% sortie attendue : ~s ~% sortie calculee : ~s ~%" (car x) (cadr x) w (caddr x) Os)
  (if (equal (cadr Os) (caddr x))
      (format t "Resultat ok ~%")
      (format t "Resultat contradictoire~%")))
(Verification Chiffres '(0 1 0 1 0 -2 -1 0) )

;; Ca marche aussi avec ces poids
(Verification Chiffres '(0 1 0 1 0 -1 -1 0) )
(Verification Chiffres '(1 2 -1 3 2 -6 -3 0))
 
 (defun Calcul_Os (X W)
  (let ((Os 0))
  (dolist (xi (cadr X))
        (setq Os (+ Os (* xi (pop W)))))

      (if (> Os 0) 
          (setq Os (list Os 1))
        (setq Os (list Os 0)))
    Os))

(defun Modification_Poids (X Os W)
  (let (Wt1)
      ;; Modification des poids
      (dolist (xi (cadr X) (reverse Wt1))
        ;; wi(t+1)=wi(t)+(C - 0)*xi
        (push (+ (pop W) (* (- (caddr X) (cadr Os)) xi)) Wt1))))
 
;; Arret quand tout le jeu a été vu et que tout l'échantillon a été traité avec des poids qui ne bougent plus (verification intégrée)
;; Algo le plus adapté
(defun Perceptron_CorrErr_Verif (S Wt)
 (let (b Wt1 Os c (tours 0))
  
  (loop
    ;; Tant qu'il y a des exemples dans l'échantillon et qu'il existe des modifications
    (setq b t)
    (setq tours (+ tours 1))
    (format t "~%~%======= Nouvelle boucle (~s) =======~%" tours)
    
    (dolist (X S)
    
    
      ;; Calcul de Os pour l'exemple
      (setq Os (Calcul_Os X Wt))
      ;
      ; Affichage 
      (Affichage Wt X Os) 
      
      (setq Wt1 (Modification_Poids X Os Wt))
      
      (format t "Nouveau Poids : ~s ~%" Wt1)
      
      (if (not (equal Wt Wt1)) 
          (setq b nil)
        )
      (setq Wt Wt1)
      )
    (if b ;; si converge (tout l'échantillon a été vu et les poids n'ont pas été modifié)
        (return-from Perceptron_CorrErr_Verif Wt)
      )
    (progn ;; Sinon demander si on veut continuer (sinon risque de boucler à l'infini)
      (format t "Tout l'echantillon n a pas ete verifie, voulez-vous continuer o/n ? ")
      (read c)
      (format t "~s ~%" c)
      (if (equal c "n")
          (return-from Perceptron_CorrErr_Verif Wt)
          )))))

(setq Poids '(1 1 1 1 1 1 1 1))
    
(setq Poids (Perceptron_CorrErr_Verif Chiffres Poids))
(defun Verification (S W)
  (let ((Os 0) (b t))
  (dolist (X S b)
    (setq Os (Calcul_Os X W))
    (Affichage W X Os)
    (if (not (equal (cadr Os) (caddr x))) 
          (setq b nil))))) 
