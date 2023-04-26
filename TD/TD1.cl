;; Fonction de base 
;;     Verification qu'on a bien une liste
(defun prefixe (l) 
  (if (listp l) 
          (list (cadr l) (prefixe (car l)) (prefixe (caddr l))) 
        l)   
  )
;; Verifie que la liste est bien formee
;;      Ajout d'une verification supplementaire : s'assurer que la liste est de taille 3. 
;;      Permet d'eviter les appels sans fin si l'arbre n'est pas un arbre binaire.

(defun prefixe (l) 
  (if (listp l) 
      (if (= (list-length l) 3)            
          (list (cadr l) (prefixe (car l)) (prefixe (caddr l))) 
        (format t "La liste est mal formee, il faut 3 elements et il y a ~s" (list-length l)))
        l)   
  )
