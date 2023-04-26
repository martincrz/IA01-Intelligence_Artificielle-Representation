;; TD7 - IA01

;; Labyrinthes :

(setq *laby1* '((A1 A2 B1)
                (A2 A3 A1)         
                (A3 A4 A2)
                (A4 B4 A3)
                (B1 C1 A1)
                (B2)
                (B3)
                (B4 C4 A4)
                (C1 C2 D1 B1)
                (C2 C3 D2 C1)
                (C3 C4 C2)
                (C4 D4 C3 B4)
                (D1 D2 C1)
                (D2 D1 C2)
                (D3)
                (D4 C4)))

(setq *laby2* '((A1 A2 B1)
                (A2 A3 A1)
                (A3 A4 A2)
                (A4 B4 A3)
                (B1 C1 A1)
                (B2)
                (B3)
                (B4 C4 A4)
                (C1 C2 B1)
                (C2 C1)
                (C3)
                (C4 D4 B4)
                (D1)
                (D2)
                (D3)
                (D4 C4)))

;; successeursValides :

(defun successeurs (etat laby)
  (cdr (assoc etat laby)))

(successeurs 'A2 *laby1*)
(successeurs 'A5 *laby1*)
(successeurs 'C4 *laby1*)

(defun successeursValides (etat laby chem)
  (let ((succ (successeurs etat laby))(suivants nil) (c chem))
    (dolist (x succ suivants)
      (if (not (member x chem))
          (push x suivants)))))

(successeursValides 'C4  *laby1* '(C3))

;; pour la distance de Manhattan
(defun x(case)
  (- (char-int (char (symbol-name case) 0)) 64))


(defun y(case)
  (- (char-int (char (symbol-name case) 1)) 48))


(defun distanceManhattan (startingPoint endPoint)
  (+ (abs (- (x startingPoint) (x endPoint)))
     (abs (- (y startingPoint) (y endPoint)))))

(distanceManhattan 'A1 'B1)
(distanceManhattan 'A1 'B2)
(distanceManhattan 'A1 'D4)

(defun sortBy (lst)
  (stable-sort lst #'< :key #'cadr))

(setq lst '((1 3) (2 2) (3 2) (2 3) (2 2) (3 1) (1 3) (4 2) (3 5)))

(sortby lst)


;; Algorithme glouton 


;; version itératif, avec sauvegarde d'états
;;
;; Glouton (depart arrivee labyrinthe)
;;    init : file = ( (depart distance_man) )
;;           chemin parcouru = NIL
;;    Tant que l'arrivee n'est pas dans la liste file :
;;           Ajouter 1er élément de file à chemin parcouru
;;           Trouver les successeurs du 1er élément de file, qu'on enlève de file
;;           Pour chaque successeur : 
;;                   Calculer heuristique 
;;                   Ajout à file 
;;           Tri de file
;;    Retour Chemin parcouru



(defun glouton1 (startingPoint endPoint Laby)
  (let ( (file (list (list startingPoint (distanceManhattan startingPoint endPoint))))
        (chemin_parcouru NIL))
    
    (while (not (assoc endPoint file))
      
      (setq chemin_parcouru (append chemin_parcouru (list (car(car file)))))
      (format t "~%L = [ ~s ] - visite = [ ~s ]" file chemin_parcouru)
      
      
      (dolist (x (successeursValides (car(pop file)) Laby chemin_parcouru) chemin_parcouru)
          (push (list x (distanceManhattan x endPoint)) file)
          )
      
      (setq file (sortby file))
      )
    (setq chemin_parcouru (append chemin_parcouru (list (car(car file)))))
    chemin_parcouru
   )
 )
        
(glouton1 'B1 'A4 *laby1*)
(glouton1 'B1 'C4 *laby2*)
;; pour ce chemin, il y a un "saut" entre C2 et A1
;; voici la version récursif qui résout ce problème


;; version récursif, avec sauvegarde d'états

(defun glouton2 (file etat endPoint Laby chemin_parcouru)
  ;; on met l'etat dans la file
  (push (list etat (distanceManhattan etat endPoint)) file)
  ;; on actualise le chemin parcouru
  (setq chemin_parcouru (append chemin_parcouru (list (car(car file)))))
  ;; si l'arrivée est dans la file
  
  (if (assoc endPoint file) 
        ;; on retourne l'etat comme une liste
        (list etat)  
        ;;sinon
        (let ((sol nil) (suivants (successeursValides (car(pop file)) Laby chemin_parcouru)))
          ;;(format t "~%L = [ ~s ] - visite = [ ~s ]" file chemin_parcouru)
          ;;(car(pop file)) ;; a laisser si on veut afficher, et mettre (car(car file)) dans le let
          
          ;; pour chacun des successeurs du premier element de la liste : 
          (while (and suivants (not sol))                                          
            (progn
                ;; on l'ajoute a la file, qu'on trie et on rappelle la fonction sur le 1er elem de la file
                (push (list (car suivants) (distanceManhattan (pop suivants) endPoint)) file)
                (setq file (sortby file))
                (setq sol (glouton2 file (car (car file)) endPoint Laby chemin_parcouru))      
                (if sol                                                             
                   (push etat sol)                                                       
                   )  
             ))
          sol)))   
  
(glouton2 NIL 'B1 'A4 *laby1* NIL)
(glouton2 NIL 'B1 'C4 *laby2* NIL)



;; Algorithme A*

(defun AlgoA (startingPoint endPoint Laby)
  (let ( (file (list (list startingPoint (distanceManhattan startingPoint endPoint))))
        (chemin_parcouru NIL))
    
    (while (not (assoc endPoint file))
      (setq chemin_parcouru (append chemin_parcouru (list (car(car file)))))
      ;;(format t "~%L = [ ~s ] - visite = [ ~s ]" file chemin_parcouru)
      
      (dolist (x (successeursValides (car(pop file)) Laby chemin_parcouru) chemin_parcouru)
          (push (list x (+ (distanceManhattan startingPoint x) (distanceManhattan x endPoint))) file)
          )
      
      (setq file (sortby file))
      )
    (setq chemin_parcouru (append chemin_parcouru (list (car(car file)))))
    chemin_parcouru
   )
 )

(AlgoA 'B1 'A4 *laby1*)
(AlgoA 'B1 'C4 *laby2*)
;; de meme, pour ce chemin, il y a un "saut" entre C2 et A1
;; voici la version récursif qui résout ce problème


(defun AlgoA-rec (file etat startingPoint endPoint Laby chemin_parcouru)
  ;; on met l'etat dans la file
  (push (list etat (distanceManhattan etat endPoint)) file)
  ;; on actualise le chemin parcouru
  (setq chemin_parcouru (append chemin_parcouru (list (car(car file)))))
  ;; si l'arrivée est dans la file
  
  (if (assoc endPoint file) 
        ;; on retourne l'etat comme une liste
        (list etat)  
        ;;sinon
        (let ((sol nil) (suivants (successeursValides (car(pop file)) Laby chemin_parcouru)))
          ;;(format t "~%L = [ ~s ] - visite = [ ~s ]" file chemin_parcouru)
          ;;(car(pop file)) ;; a laisser si on veut afficher, et (car(car file)) dans le let
          
          ;; pour chacun des successeurs du premier element de la liste : 
          (while (and suivants (not sol))                                          
            (progn
                ;; on l'ajoute a la file, qu'on trie et on rappelle la fonction sur le 1er elem de la file
                (push (list (car suivants) (+ (distanceManhattan startingPoint (car suivants))(distanceManhattan (pop suivants) endPoint))) file)
                (setq file (sortby file))
                (setq sol (AlgoA-rec file (car (car file)) startingPoint endPoint Laby chemin_parcouru))       
                (if sol                                                             
                   (push etat sol)                                                       
                   )  
             ))
          sol)))  

(AlgoA-rec NIL 'B1 'B1 'A4 *laby1* NIL)
(AlgoA-rec NIL 'B1 'B1 'C4 *laby2* NIL)
