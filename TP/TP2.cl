;; TP2 IA01


(setq actions '((16 3 2 1) (15 3 2 1) (14 3 2 1) (13 3 2 1) (12 3 2 1) (11 3 2 1) (10 3 2 1) (9 3 2 1) (8 3 2 1) (7 3 2 1) (6 3 2 1) (5 3 2 1) (4 3 2 1) (3 3 2 1) (2 2 1) (1 1)))

;; Resolution 1

;; fonction qui donne tous les successeurs d'un etat :
(defun successeurs (allumettes actions)
  (cdr (assoc allumettes actions)))

;; EXPLORE 
(defun explore (allumettes actions joueur i)
  (cond 
   ((and (eq joueur 'humain) (eq allumettes 0)) nil) 
   ((and (eq joueur 'IA) (eq allumettes 0)) t)    
    (t (progn                             
         (let ((sol nil) (coups (successeurs allumettes actions))) 
           (while (and coups (not sol)) 
             (progn
               (format t "~%~V@tJoueur ~s joue ~s allumettes - il reste ~s allumette(s) " i joueur (car coups) (- allumettes (car coups)))    
                (setq sol (explore (- allumettes (car coups)) actions (if (eq joueur 'IA) 'humain 'IA) (+ i 3)))  
                (if sol           
                    (setq sol (car coups))) 
               (format t "~%~V@t sol = ~s~%" i sol)
               (pop coups)
               )  
             )
           sol))))) 

(defvar nbCoupsAJouer nil)
(setq nbCoupsAJouer (explore 16 actions 'IA 0))
(setq nbCoupsAJouer (explore 8 actions 'IA 0))
(setq nbCoupsAJouer (explore 3 actions 'IA 0))


;; Resolution 2

(defun Randomsuccesseurs (actions) 
  (let ((r (random (length actions))))
    ;;(format t "~&~2t Res du random ~s~&" r)
    (nth r actions)))

(Randomsuccesseurs (successeurs 10 actions))

(defun JeuJoueur (allumettes actions)
  (let ( (coup 0) (possibilites (successeurs allumettes actions)))
    (loop do (progn
              (format t "~%Saisir nombre d allumettes a tirer : ")
               (setq coup (read))
               )
          while (not (member coup possibilites)))
   coup)
  )

(JeuJoueur 2 actions)

;; sans renforcement 
(defun explore-renf (allumettes actions)
  ;; on initialise une variable joueur, vaut IA au début
  ;; une variable i pour permettre un affichage listé 
  ;; et une variable correspond au nombre d’allumettes retirés par un joueur
  (let ((joueur 'IA) (i 1) (retire 0))
      ;; tant qu’il y a des allumettes …
    (while allumettes
        ;; … on affiche le nombre d’allumettes
        (format t "~%~s.   Il y a ~s allumettes," i allumettes)
        (setq i (+ i 1))
        ;; si c’est a l’IA de joueur :
        (if (eq joueur 'IA)
            ;; on récupère aléatoirement le nombre d’allumettes retirés, on l’affiche. Nombre qu’on retire du nombre total d’allumettes 
          (progn
            (setq retire (Randomsuccesseurs (successeurs allumettes actions)))
            (format t "l'IA tire ~s allumettes" retire)
            (setq allumettes (- allumettes retire))
            ;; si l’IA a pris la derniere, c est perdu, sinon c’est a l humain de jouer
            (if (eq allumettes 0) (return nil))
            (setq joueur 'humain))

      ;;sinon, c' est à l'humain de jouer
        (progn
          (format t "~%")
            ;; il retire le nombre d’allumettes désirés, nombre qu’on enleve du total
          (setq retire (JeuJoueur allumettes actions))
          (format t  "l'humain tire ~s allumettes ~%" retire)
          (setq allumettes (- allumettes retire))
            ;; s’il a pris la derniere, on retourne les actions
          (if (eq allumettes 0) (return actions))
            ;; sinon c’est à l’IA de jouer
          (setq joueur 'IA)))
      )))

(explore-renf 16 actions)

;; avec renforcement
(defun explore-renf (allumettes actions)
  ;; on initialise une variable joueur, vaut IA au début
  ;; une variable i pour permettre un affichage listé 
  ;; et deux variables correspondants au nombre d’allumettes retirés par un joueur ou par l'IA
  (let ((joueur 'IA) (i 1) (retire_IA 0)(retire_h 0))
    ;; tant qu’il y a des allumettes …
    (while allumettes
      ;; … on affiche le nombre d’allumettes
      (format t "~%~s.   Il y a ~s allumettes," i allumettes)
      (setq i (+ i 1))
      ;; si c’est a l’IA de joueur :
      (if (eq joueur 'IA)
          ;; on récupère aléatoirement le nombre d’allumettes retirés, on l’affiche. Nombre qu’on retire du nombre total d’allumettes 
          (progn
            (setq retire_IA (Randomsuccesseurs (successeurs allumettes actions)))
            (format t "l'IA tire ~s allumettes" retire_IA)
            (setq allumettes (- allumettes retire_IA))
            ;; si l’IA a pris la derniere, c est perdu, sinon c’est a l humain de jouer
            (if (eq allumettes 0) (return nil))
            (setq joueur 'humain))
        
        ;;sinon, c' est à l'humain de jouer
        (progn
          (format t "~%")
          ;; il retire le nombre d’allumettes désirés, nombre qu’on enleve du total
          (setq retire_h (JeuJoueur allumettes actions))
          (format t "l'humain tire ~s allumettes ~%" retire_h)
          (setq allumettes (- allumettes retire_h))
          ;; s’il a pris la derniere, on retourne les actions
          (if (eq allumettes 0) 
              (progn
                  ;; ajoute l action réalisé au dernier coup de l' IA dans la liste d'actions pour (retire_IA + retire_h) allumettes soit le nombre d'allumettes avant que l'IA ne joue
                  (nconc (assoc (+ retire_IA retire_h) actions) (list retire_IA))            
                  (return actions)
               )
          ;; sinon c’est à l’IA de jouer
          (setq joueur 'IA))))
      )))


(explore-renf 16 actions)

(defun renforcement (nb_allumettes coup_gagnant actions)
  (nconc (assoc nb_allumettes actions) (list coup_gagnant))actions)

(renforcement 16 3 actions)

(defun explore-renf-rec (allumettes actions joueur i)
  
  (cond  
   ;; si l'IA a pris la derniere allumettes > perdu
   ((and (eq joueur 'humain) (eq allumettes 0)) nil) 
   ;; si l'humain a pris la derniere allumettes > gagné
   ((and (eq joueur 'IA) (eq allumettes 0)) t) 
   ;; sinon : il reste des allumettes
   (t (progn
        ;; on initialise deux variables correspondants au nombre d’allumettes retirés par un joueur ou par l'IA (une seule aurait pu suffir)
        ;; et une variable solution 
        (let ((retire_IA 0)(retire_h 0) (sol nil))
          ;; si c'est a l'IA de jouer
          (if (eq joueur 'IA) (progn
                      ;; on récupère aléatoirement le nombre d’allumettes retirés, on l’affiche        
                      (setq retire_IA (Randomsuccesseurs (successeurs allumettes actions)))
                      (format t "~%~V@tIl y a ~s allumettes, l'IA tire ~s allumettes" i allumettes retire_IA)
                      ;; on rappelle la fonction avec le nb d'allumettes restants pour l'humain         
                      (setq sol (explore-renf-rec (- allumettes retire_IA) actions 'humain (+ i 3)))
                              ;; si l'humain a pris la derniere allumette, on renforce chaque action réalisé par l'IA
                              (if sol (progn
                                        (format t "~%~V@t On ajoute l action ~s quand il reste ~s allumettes" i retire_IA allumettes)
                                        (renforcement allumettes retire_IA actions))))
            
          ;; sinon c'est a l'humain de jouer
          (progn
            (format t "~%~V@tIl y a ~s allumettes~%" i allumettes)
            ;; il retire le nombre d’allumettes désirés
            (setq retire_h (JeuJoueur allumettes actions))
            (format t "~%~V@t l'humain tire ~s allumettes" i retire_h)
            ;; on rappelle la fonction avec le nb d'allumettes restants pour l'IA 
            (setq sol (explore-renf-rec (- allumettes retire_h) actions 'IA (+ i 3))))))))))
          
(explore-renf-rec 16 actions 'IA 0)
