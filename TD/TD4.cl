
(setq *laby* '((e 1)(1 e 2)(2 1 7)(7 2 6 8)(6 3 7)(3 6)(8 7 9)(9 8 10)(10 9 11 15)(11 10 12 14)
             (14 11)(12 5 11)(5 4 12)(4 5)(15 10 16)(16 15 17)(17 16 18)(18 17 19)
             (19 18 20)(20 19 13 s)(13 20)(s 20)))

(setq *laby* '((e 1)(1 e 2)(2 1 7)(7 2 6 8)(6 3 7 s)(3 6)(8 7 9)(9 8 10)(10 9 11 15)(11 10 12 14)
             (14 11)(12 5 11)(5 4 12)(4 5)(15 10 16)(16 15 17)(17 16 18)(18 17 19)
             (19 18 20)(20 19 13)(13 20)(s 3)))

(setq *laby* '((e 1)(1 e 2)(2 1 7)(7 2 6 8)(6 3 7 s)(3 6)(8 7 9)(9 8 10)(10 9 11 15)(11 10 12 14)
             (14 11 s)(12 5 11)(5 4 12)(4 5)(15 10 16)(16 15 17)(17 16 18)(18 17 19)
             (19 18 20)(20 19 13)(13 20)(s 6 14)))

(setq *laby* '((e 1)(1 e 2)(2 1 7)(7 2 6 8)(6 3 5 7)(3 4 6)(8 7 9)(9 8 10)(10 9 11 15)(11 10 12 14)
             (14 11)(12 5 11)(5 4 6)(4 3 5)(15 10 16)(16 15 17)(17 16 18)(18 17 19)
             (19 18 20)(20 19 13 s)(13 20)(s 20)))



;; fonction qui donne tous les successeurs d'un etat :

(defun successeurs (etat laby)
  (cdr (assoc etat laby)))


;; fonction qui renvoie les successeurs valides :

(defun successeursValides (etat laby chem)
  (let ((succ (successeurs etat laby))(suivants nil))
    (dolist (x succ suivants)
      (if (not (member x chem))
          (push x suivants)))))

(defun successeursValides (etat laby chem)
  (let ((succ (successeurs etat laby)))
    (dolist (x succ succ)
      (if (member x chem)
      (setq succ (remove x succ))))))

(successeursValides '7 *laby* '(2 8))


;; Algo prof avec un dolist
;; mettre l’état dans cheminParcouru
;; Si (etat = sortie) retourner le cheminParcouru
;; Sinon pour tous les successeursValide -> explore


(defun explore (etat sortie laby chem)
  (push etat chem)                                                                 
  (if (equal etat sortie) 
      (print (reverse chem))
      (dolist (x (successeursValides etat laby chem))
        (explore x sortie laby chem))))


(setq l (explore 'E 'S *laby* nil)) 

;; pb de cet algo :
;;    - ne s'arrête pas quand trouve une solution = parcours tout le laby et 
;;              imprime tous les bons chemins
;;    - renvoie le dernier chemin même si a trouvé plusieurs bon chemin 
;;              (donc si le dernier mène à une impasse, alors renvoie nil !


;; solution d'un étudiant (pas tout à fait le même algo que vu en TD)
(defun explore_profondeur (laby noeud chemin)
  (let ((noeuds_a_visiter (succ_valides laby noeud chemin)) (solution))
    (if (member 'S noeuds_a_visiter)               ; condition d'échappement : S fait partie des noeuds à visiter
        (return-from explore_profondeur (cons 'S NIL)))
    (if noeuds_a_visiter
      (dolist (n noeuds_a_visiter NIL)
        (setq solution (explore_profondeur laby n (cons n chemin)))
        (if solution
            (return-from explore_profondeur (cons n solution))))
      NIL)))  ; aucun noeuds_a_visiter
;; on sort du dolist quand on trouve la solution
;; c'est discutable mais pour moi ce n'est pas propre de sortir ainsi du dolist


;;; Algo prof
;; Créer les variables (suivants = sucesseursValides) (sol = nil)
;; Mettre l'état dans CheminParcouru
;;    Si etat = sortie retourner etat
;;    sinon 
;;        Tant qu'il existe des suivants et que pas trouvé (sol = nil)
;;            "Je vais de" état "à" (car suivants) 
;;            sol = explore avec le (car suivants)
;;            si sol
;;                push etat dans sol
;;            sinon
;;                 "de" (pop suivants) "je vais en" état
;;        retourner sol vide ou avec la solution





;; EXPLORE PROFONDEUR (faire plutot avec chemin parcouru en retour voir algo suivant)
;; ---  renvoie le chemin direct qui mène à la sortie
;; On met l'état courant dans cheminPacouru
;; Si A la fin on a trouvé la sortie, on la renvoie
;; Sinon
           ;; Déclaration sol = nil, succ = successeurs valides
           ;; Tant qu'il y a des successeurs valides et que sol mène à nil
                 ;; Affichage du successeur valide où on va 
                 ;; sol = Explore avec ce successeur (car suivants)
                 ;; Si ce chemin a mené à une sol différente de nil
                         ;; alors on push l'état dans la sol
                 ;; sinon on affiche qu'on revient au précédent (en fait on passe juste au successeur suivant)
           ;; On retourne la sol (avec le bon chemin ou nil)
(defun explore-prof (etat sortie lab cheminParcouru)
  (push etat cheminParcouru)            ;; On met l'état courant dans cheminPacouru
    (if (eq etat sortie) 
        (list etat)                     ;; A la fin on a trouvé la sortie, on la renvoie
                                  
        (let ((sol nil) (suivants (successeursValides etat lab cheminParcouru)))    ;; Sinon 
          (while (and suivants (not sol)) ;; Tant qu'il y a des successeurs valides et que sol mène à nil                                         
              (progn
                (format t "~%De ~s je vais en ~s" etat (car suivants))               ;; Affichage du successeur valide où on va 
                (setq sol (explore-prof (car suivants) sortie lab cheminParcouru))        ;; sol = Explore avec ce successeur (car suivants)
                (if sol                                                              ;; Si ce chemin a mené à une sol différente de nil
                   (push etat sol)                                                        ;; alors on push l'état dans la sol
                  (format t "~%De ~s je retourne en ~s" (pop suivants) etat)) ;; sinon on affiche qu'on revient au précédent (en fait on passe juste au successeur suivant)
                )  
             )
          sol)))   ;; On retourne la sol (avec le bon chemin ou nil si plus de successeurs)

(setq chemin (explore-prof 'e 's *laby* nil))


;; EXPLORE PROFONDEUR
;; -- renvoie le chemin parcouru qui mène directement à la sortie (sans construction en dépilant)
;; On met l'état courant dans cheminPacouru
;; Si A la fin on a trouvé la sortie, on la renvoie
;; Sinon
;;         ;; Déclarer sol = nil et suivants = successeurs valides
           ;; Tant qu'il y a des successeurs valides et que sol mène à nil
                 ;; Affichage du successeur valide où on va 
                 ;; sol = Explore avec ce successeur (car suivants)
                 ;; Si ce chemin ne mène pas à la sortie 
                         ;; on affiche qu'on revient au précédent (en fait on passe juste au successeur suivant)
           ;; On retourne la sol (avec le bon chemin ou nil)
(defun explore-prof2 (etat sortie lab cheminParcouru)
  (push etat cheminParcouru)            ;; On met l'état courant dans cheminPacouru
   (if (eq etat sortie) 
        cheminParcouru                            ;; A la fin on a trouvé la sortie, on renvoie tout le chemin parcouru
     
     (let ((sol nil) (suivants (successeursValides etat lab cheminParcouru))) ;; Sinon 

          (while (and suivants (not sol)) ;; Tant qu'il y a des successeurs valides et que sol mène à nil                                         
              (progn
                (format t "~%De ~s je vais en ~s" etat (car suivants))               ;; Affichage du successeur valide où on va 
                (setq sol (explore-prof2 (car suivants) sortie lab cheminParcouru))        ;; sol = Explore avec ce successeur (car suivants)
                (if (not sol)                                                              ;; Si ce chemin a mené à une sol différente de nil
                  (format t "~%De ~s je retourne en ~s" (pop suivants) etat)) ;; sinon on affiche qu'on revient au précédent (en fait on passe juste au successeur suivant)
                )  
             )
          sol)))   ;; On retourne la sol (avec le bon chemin ou nil si plus de successeurs)

(setq chemin (explore-prof2 'e 's *laby* nil))

;; Algo fonction backtrack
;; -- on va recherche dans (chemin = (cdr cheminParcouru) le premier état qui a des successeurs valides
;; S'il reste des éléments dans chemin 
;;    On récupère les successeurs valides du premier élément de ce chemin
;;    Affichage je reviens en à ce premier élément
;;    S'il y a des successeurs valide pour ce premier élément
;;          Affichage je vais dans le 1er successeur valide de ce premier élément
;;          j'explore avec ce 1er successeur
;;    sinon
;;          je backtrack sur le (cdr chemin)
(defun backtrack (chemin sortie lab cheminParcouru)
    (if chemin
       (let ((suivants (successeursValides (car chemin) lab cheminParcouru)))
         (format t "~%--- backtrack je reviens en ~s" (car chemin)) 
         (if suivants
             (progn
               (format t "~%--- je vais en ~s" (car suivants)) 
               (explore-prof-bckt (car suivants) sortie lab cheminParcouru))
            (backtrack (cdr chemin) sortie lab cheminParcouru)))
      nil))
;; Algo explore avec backtrack
;; -- renvoie le chemin parcouru qui mène directement à la sortie s
;; On récupère la liste des successeurs valides
;; On met l'état courant dans cheminPacouru
;; Si A la fin on a trouvé la sortie, on la renvoie
;; Sinon si l y a des successeurs valides 
;;           Affichage du premier successeur valide où on va 
;;           sol = Explore avec ce successeur (car suivants)
;; Sinon on backtrack à partir du cheminParcouru

(defun explore-prof-bckt (etat sortie lab cheminParcouru)
  (let ((sol nil) (suivants (successeursValides etat lab cheminParcouru)))  
  (push etat cheminParcouru)            ;; On met l'état courant dans cheminPacouru
  (cond
     ((eq etat sortie) cheminParcouru) ;; A la fin on a trouvé la sortie, on renvoie tout le chemin parcouru
     (suivants 
         (progn            
                (format t "~%De ~s je vais en ~s" etat (car suivants))                   ;; Affichage du successeur valide où on va 
                (setq sol (explore-prof-bckt (car suivants) sortie lab cheminParcouru))));; sol = Explore avec ce successeur (car suivants)
   (t (backtrack (cdr cheminParcouru) sortie lab cheminParcouru)))))                     ;; Si ce chemin a mené à une sol différente de nil
                  
(setq chemin (explore-prof-bckt 'e 's *laby* nil))


(setq *laby* '((e 1)(1 e 2)(2 1 7)(3 6 s)(4 5)(5 4 12)(6 3 7)(7 2 6 8)(8 7 9)(9 8 10)(10 9 11 15)(11 10 12 14)
             (12 5 11)(13 20)(14 11)(15 10 16)(16 15 17)(17 16 18)(18 17 19)
             (19 18 20)(20 19 13)(s 3)))


(setq *laby* '((e 1)(1 e 2)(2 1 7)(7 2 6 8)(6 3 7)(3 6)(8 7 9)(9 8 10 s)(10 9 11 15)(11 10 12 14)
             (14 11)(12 5 11)(5 4 12)(4 5)(15 10 16)(16 15 17)(17 16 18)(18 17 19)
             (19 18 20)(20 19 13)(13 20)(s 9 11)))


;; pas de sol
(setq *laby* '((e 1)(1 e 2)(2 1 7)(7 2 6 8)(6 3 7)(3 6)(8 7 9)(9 8 10)(10 9 11 15)(11 10 12 14)
             (14 11)(12 5 11)(5 4 12)(4 5)(15 10 16)(16 15 17)(17 16 18)(18 17 19)
             (19 18 20)(20 19 13)(13 20)(s)))


;; Algo largeur (liste etats = niveau exploré)
;; Cond
;;     sortie est dans la liste etats - renvoyer chemin parcouru
;;     etats = nil - renvoyer nil
;;     sinon
;;         Ajouter etats à cheminParcouru
;;         Pour tous les éléments de la liste etats du niveau
;;             chercher les successeurs valides et les ajouter à la liste suivants
;;         S'il existe des éléments dans suivants
;;              sol = explore avec les suivants dans etats
;;         Renvoyer sol
(defun explore-larg (etats sortie laby cheminParcouru)
  (let ((suivants nil) (sol nil))
    (format t "Explore etats ~s, cheminP ~s~%" etats cheminParcouru)
    (cond
     ((member sortie etats) cheminParcouru)
     ((not etats) nil)
     (t (progn
          (setq cheminParcouru (append etats cheminParcouru )) 
          (format t "~tAjout etats ~s dans cheminP ~s~%" etats cheminParcouru) 
          (dolist (x etats suivants)
            (setq suivants (append suivants (successeursValides x laby cheminParcouru))))
          (format t "suivants ~s~%" suivants)
          (if suivants     
            ;;(progn
              (setq sol (explore-larg suivants sortie laby cheminParcouru))        ;; sol = Explore avec tous les suivants 
              ;;(if sol  ;; Si ce chemin a mené à une sol différente de nil
                  ;;(setq sol (append etats sol))))        ;; alors on push l'état dans la s
            ;;(format t "Etats ~s suivants ~s~%" etats suivants))
            sol)
          )))))  ;; On retourne la sol (avec le bon chemin ou nil si plus de successeurs)


(setq bonChemin (explore-larg '(e) 's *laby* nil))
bonChemin
