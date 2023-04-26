;; Avec Variables lexicales
(defun def_node (node type nom nodes)
    (push
         (list 
          node
          (cons 'type type)
          (cons 'nom nom)
          (list 'arcs_in)
          (list 'arcs_out)
          (list 'marks)
         ) nodes)
    nodes
  )


(defparameter BdN nil)
(defparameter BdA nil)

(setq BdN (def_node 'DeGuiche0 'individu 'DeGuiche BdN)) ;
(setq BdN (def_node 'Roxane0 'individu 'Roxane BdN)) 
(setq BdN (def_node 'Cyrano0 'individu 'Cyrano BdN)) 
(setq BdN (def_node 'Christian0 'individu 'Christian BdN)) 

(setq BdN (def_node 'Conte 'concept 'Comte BdN)) 
(setq BdN (def_node 'Cadet 'concept 'CadetDeGascogne BdN)) 
(setq BdN (def_node 'Mondaine 'concept 'Mondaine BdN)) 
(setq BdN (def_node 'Noble 'concept 'Noble BdN)) 
(defun def_arc (arc Na type Nb arcs nodes)
  (push (list arc
              (cons 'type type)
              (cons 'from_node Na)
              (cons 'to_node Nb)
              ) arcs)
        (push arc (cdr (assoc 'arcs_in (cdr (assoc Nb nodes)))))
        (push arc (cdr (assoc 'arcs_out (cdr (assoc Na nodes)))))
    arcs)    


(setq BdA (def_arc 'A0 'DeGuiche0 'is_a 'Conte BdA BdN))
(setq BdA (def_arc 'A1 'Roxane0 'is_a 'Mondaine BdA BdN))
(setq BdA (def_arc 'A2 'Cyrano0 'is_a 'Cadet BdA BdN))
(setq BdA (def_arc 'A3 'Christian0 'is_a 'Cadet BdA BdN))
(setq BdA (def_arc 'A4 'Conte 'is_a 'Noble BdA BdN))
(setq BdA (def_arc 'A5 'Cadet 'is_a 'Noble BdA BdN))

(setq BdA (def_arc 'A6 'Cyrano0 'aime 'Roxane0 BdA BdN))
(setq BdA (def_arc 'A7 'DeGuiche0 'aime 'Roxane0 BdA BdN))
(setq BdA (def_arc 'A8 'Christian0 'aime 'Roxane0 BdA BdN))
(setq BdA (def_arc 'A9 'Roxane0 'aime 'Christian0 BdA BdN))

;; Sans Variables lexicales
(defun def_node (node type nom nodes)
    (push
         (list 
          node
          (cons 'type type)
          (cons 'nom nom)
          (list 'arcs_in)
          (list 'arcs_out)
          (list 'marks)
         ) nodes)
    nodes
  )


(setq BdN nil)
(setq BdA nil)

(setq BdN (def_node 'DeGuiche0 'individu 'DeGuiche BdN)) ;
(setq BdN (def_node 'Roxane0 'individu 'Roxane BdN)) 
(setq BdN (def_node 'Cyrano0 'individu 'Cyrano BdN)) 
(setq BdN (def_node 'Christian0 'individu 'Christian BdN)) 

(setq BdN (def_node 'Conte 'concept 'Comte BdN)) 
(setq BdN (def_node 'Cadet 'concept 'CadetDeGascogne BdN)) 
(setq BdN (def_node 'Mondaine 'concept 'Mondaine BdN)) 
(setq BdN (def_node 'Noble 'concept 'Noble BdN)) 

;; Sans Variables globales
(defun def_arc (arc Na type Nb arcs nodes)
  (push (list arc
              (cons 'type type)
              (cons 'from_node Na)
              (cons 'to_node Nb)
              ) arcs)
		(pushnew arc (cdr (assoc 'arcs_in (cdr (assoc Nb nodes)))))
		(pushnew arc (cdr (assoc 'arcs_out (cdr (assoc Na nodes)))))
	arcs)	


(setq BdA (def_arc 'A0 'DeGuiche0 'is_a 'Conte BdA BdN))
(setq BdA (def_arc 'A1 'Roxane0 'is_a 'Mondaine BdA BdN))
(setq BdA (def_arc 'A2 'Cyrano0 'is_a 'Cadet BdA BdN))
(setq BdA (def_arc 'A3 'Christian0 'is_a 'Cadet BdA BdN))
(setq BdA (def_arc 'A4 'Conte 'is_a 'Noble BdA BdN))
(setq BdA (def_arc 'A5 'Cadet 'is_a 'Noble BdA BdN))

(setq BdA (def_arc 'A6 'Cyrano0 'aime 'Roxane0 BdA BdN))
(setq BdA (def_arc 'A7 'DeGuiche0 'aime 'Roxane0 BdA BdN))
(setq BdA (def_arc 'A8 'Christian0 'aime 'Roxane0 BdA BdN))
(setq BdA (def_arc 'A9 'Roxane0 'aime 'Christian0 BdA BdN))

(defun mark_node (node mark nodes)
	(pushnew mark (cdr (assoc 'marks (cdr (assoc node nodes)))))
  )

(defun is_marked (node mark nodes)
  (member mark (cdr (assoc 'marks (cdr (assoc node nodes))))))
  
(defun is_typed (arc type arcs)
  (equal (cdr (assoc 'type (cdr (assoc arc arcs)))) type)
  )

(defun is_fromNode (node arc arcs)
  (equal node (cdr (assoc 'from_node (cdr (assoc arc arcs)))))
  )


(defun is_toNode (node arc arcs)
  (equal node (cdr (assoc 'to_node (cdr (assoc arc arcs)))))
  )


(defun typed (arc arcs)
	(cdr (assoc 'type (cdr (assoc arc arcs))))
  )

(defun fromNode (arc arcs)
	(cdr (assoc 'from_node (cdr (assoc arc arcs))))
  )


(defun toNode (arc arcs)
	(cdr (assoc 'to_node (cdr (assoc arc arcs))))
)
(defun successeurs (node type arcs nodes)
	(let ((succ nil))
		(dolist (arc (cdr (assoc 'arcs_out (cdr (assoc node nodes)))) succ)
			(if (equal (is_typed arc type arcs) t)
				(if (equal (is_fromNode node arc arcs) t) ;; pas necssaire
					(push (toNode arc arcs) succ)
				)
			)
		)
	)
  )

(successeurs 'Cadet 'is_a BdA BdN)

(defun predecesseurs (node type arcs nodes)
  (let ((pred nil))
    (dolist (arc (cdr (assoc 'arcs_in (cdr (assoc node nodes)))) pred)
      (if (equal (is_typed arc type arcs) t)
          ;; (if (equal (is_toNode node arc arcs) t) ;; pas necessaire
              (push (fromNode arc arcs) pred)
            
        )
      )
    )
  )
(predecesseurs 'Noble 'is_a BdA BdN)


;; defun wave (node mark type arcs sens nodes)
;; Initialisations ....
;; Si node pas déjà marqué
;; -------- Marquer le noeud node avec la marque mark
;; -------- Ici on va récupérer les noeuds de l'autre côté des arcs du bon type
;; -------- Si (sens = 'direct)
;; ---------------------- suivants = (successeurs node type arcs nodes)
;; --------- Sinon
;; ---------------------- suivants = (predecesseurs node type arcs nodes)
;; --------- Pour tous les noeuds s de suivants
;; ---------------------- (wave s mark type arcs sens nodes)

(defun wave (node mark type arcs sens nodes)
	(let ((suivants nil))
		(if (not (is_marked node mark nodes))
                  (progn     
				(mark_node node mark nodes)
				(if (equal sens 'direct)
					(setq suivants (successeurs node type arcs nodes))
					(setq suivants (predecesseurs node type arcs nodes))
      )

                    (dolist (s suivants)
                   
                      (wave s mark type arcs sens nodes))

				)
			)
		)
	)


(wave 'Noble 'M1 'is_a BdA 'indirect BdN)
(wave 'Mondaine 'M2 'is_a BdA 'indirect BdN)
(wave 'Christian0 'M3 'is_a BdA 'direct BdN)

;; get_results (markA type markB arcs nodes)
;; Pour tous les arcs arc de la liste arcs
;; ----- Si AND (arc is-typed avec type) 
;; -----        (fromNode arc is-marked avec markB)
;; -----        (toNode arc is-marked avec markA)
;; ------------ Ajouter toNode à Resultat

(defun get_results (markA type markB arcs nodes)
	(let ((resultat nil))
   (dolist (arc arcs resultat)
     (if (AND (is_typed (car arc) type arcs)
              (is_marked (fromNode (car arc) arcs) markB nodes)
              (is_marked (toNode (car arc) arcs) markA nodes))
              (pushnew (cdr (assoc 'nom (cdr (assoc (toNode (car arc) arcs) nodes)))) resultat)
       )
     )
  )
)

(get_results 'M2 'aime 'M1 BdA BdN) ;; ;; Quelles sont les mondaines aimées par un noble 
(get_results 'M1 'aime 'M2 BdA BdN) ;; Quels sont les nobles aimés par une mondaine













































































