(setq *frames* nil)
(setq ETRE
      '(ETRE
        (TYPE (VALUE CONCEPT))
        (PERE (DEFAULT "ROOT"))
        (NOTHING (IF-ADDED myToto))
        ))

(defun myToto (slot value)
  (print "toto ne sert à rien"))
;;(myToto 't 85)

(pushnew 'ETRE *frames*)
;; Remarque j'ai fait le choix de mettre les id dans la liste de frames et pas tout le frames. Cela nécessitera de faire des eval de l'identifiant

(setq ELEPHANT
      '(ELEPHANT
        (TYPE (VALUE CONCEPT))
        (IS-A (VALUE ETRE))
        (COLOR (DEFAULT "grey"))
        (AGE (IF-NEEDED ask-user)
             (IF-ADDED check-age))
        (POIDS (IF-NEEDED computer-weight-from-age))
        (AFFICHAGE (IF-ADDED draw-elephant)
                   (IF-REMOVED erase-elephant))
        ))
(pushnew 'ELEPHANT *frames*)

(defun check-age (slot value)
  (if (and (> value 0) (<= value 140))
      value
  (progn
    (format t "~%error age~%")
    (format t "saisir a nouveau age : ")
    (check-age slot (read)))))
;;(check-age 'AGE 189)



(defun getSlotsAllowed (concept)
   (let ((slots NIL))
    (dolist (slot (cdr (eval concept)) slots) ;; eval ou symbol-name
      (if (equal (car slot) 'is-a)
            (setq slots (append (getSlotsAllowed (cadr (assoc 'value (cdr slot)))) slots))
        (pushnew (car slot) slots)))))
(getSlotsAllowed 'ELEPHANT)


(defun getDemon (concept typeDemon slot)
  (let ((fn nil) herit)        
    ;;Cherche si démon existe dans ce concept
    (setq fn (cadr (assoc typeDemon 
                          (cdr (assoc slot (cdr (eval concept)))))))
    ;; Sinon appel récursif avec le pere
    (if (not fn) 
        (progn
          (setq herit (assoc 'is-a (cdr (eval concept))))
          (if herit
              (setq fn (getDemon (cadr (assoc 'value (cdr herit))) typeDemon slot))
            )))
    ;; Retour du nom de fonction ou nil
          fn))

(getDemon 'Elephant 'IF-ADDED 'AGE)  
(getDemon 'Elephant 'IF-ADDED 'TOTO)
(getDemon 'Elephant 'IF-ADDED 'NOTHING)

(defun verif-add-slot (value concept slot)
  (let (demon-if-added)
       ;; check for IF-ADDED demon
        ;;(setq fn (cadr (assoc 'IF-ADDED 
        ;;                      (cdr (assoc slot (cdr (symbol-value concept)))))))
        (setq demon-if-added (getDemon concept 'IF-ADDED slot))

        ;; if there, we call it, presumably it returns a normalized value
        (if demon-if-added 
            (setq value (funcall demon-if-added slot value))
           valu''e)))


;; Algo make-individu (concept caract lframes)
;; Si concept existe et si c'est un concept
;; -------- créer le frame + Construit le début du frame (individu + is-a
;; -------- Récuperer les slots autorisés
;; -------  Tant quil y a des caractéristiques
;; ---------------- slot = pop caract
;; ---------------- value  = pop caract
;; ---------------- Si slot autorisé (et n'existe pas déjà)
;; ---------------------- Si demon = getDemon (verifie if-added)
;; --------------------------- value = execute demon
;; ---------------------- Ajouter (slot ('value value) au frame
;; --------- Ajouter frame à lframes
;; renvoyer lframes

(defun make-individu (name concept caract lframes)
 
  (if (not (member concept lframes)) ;; il faudrait vérifier que le concept est bien un concept
    (error "concept ~S does not exist" concept)
  
  ;; otherwise create id, and record list of concept slots
  (let ((allowed-slots (getSlotsAllowed concept)) 
        (frame (gentemp (symbol-name concept))) ;; possible eval à la place de symbole name
        ;; (gentemp (concatenate 'string (symbol-name concept) "_"))
        slot value fn)
    ;; create piece of frame
    (set frame (list name
                     (list 'type '(value individu))
                    (list 'is-a (list 'value concept))
                      ))
       
    ;; process slots
    (loop
      ;; when no more get out of the loop
      (unless caract (return nil))
      
      ;; pop up list
      (setq slot (pop caract))
      (setq value (pop caract))
      ;; verification demon IF-ADDED
      (setq value (verif-add-slot value concept slot lframes))
 
      ;; when slot is allowed and not exist in the frame we process it
      (if (and (member slot allowed-slots) (not (assoc slot (cdr (symbol-value frame)))))
        (if value (set frame (append (symbol-value frame)
                                    (list (list slot (list 'VALUE value))))))) 
    
    ;; continue with next caract
      )
    ;; add id to frame-list
    (push frame *frames*)
    ;; return frame
    frame)))

(make-individu 'CLYDE 'ELEPHANT '(COLOR "pink" AGE 5) *frames*)
(make-individu 'Toto 'ELEPHANT '(COLOR "pink" COLOR "blue" AGE 5) *frames*)
(make-individu 'Babar 'ELEPHANT '(AGE 189) *frames*) ;; pas possible erreur
(make-individu 'Delirium 'ELEPHANT '(COLOR "pink" AGE 85) *frames*)
(make-individu 'Celeste 'ELEPHANT '(AGE 20) *frames*)
(make-individu 'Cornelius 'ELEPHANT '(Toto 85) *frames*)
(make-individu 'Clyde 'ELEPHANT nil *frames*)
(make-individu 'Clyde 'ELEPHANT nil *frames*)
(make-individu 'Clyde 'ELEPHANT nil *frames*)


(defun computer-weight-from-age (frameoriginal slot lframe)
  500)

(defun computer-weight-from-age (frameoriginal slot lframe)
  (* 100 (get-slot-value frameoriginal 'AGE lframe frameoriginal)))

(defun ask-user (frameoriginal slot lframe)
  (let (result)
    (format t "~%entrer ~s de ~s : " slot (car (symbol-value frameoriginal)))
    ;; idéalement il faudrait modifier le frame et ajouter la valeur
    (setq result (parse-integer (read-line)))))

(ask-user 'ELEPHANT4 'AGE *frames*)

(defun ask-user (frameoriginal slot lframes)
  (let* (result (frame (symbol-value frameoriginal)) (concept (cadr (assoc 'VALUE (cdr (assoc 'IS-A (cdr frame)))))))
    (format t "~%entrer ~s de ~s : " slot (car frame))
    (setq result (parse-integer (read-line)))
    
    (setq result (verif-add-slot result concept slot))
    ;; Idealement il faudrait ajouter le result à lframes mais ici c'est une copie - à améliorer...
    (if result
        (progn
          (if (assoc slot (cdr frame))
              (delete (assoc slot (cdr frame)) frame))
          (format t "~& frame : ~s" frame)
          (push (list slot (list 'VALUE result)) (cdr frame))))
    result
    ))

;;(ask-user 'ELEPHANT2 'AGE *frames*)

(defun get-slot-value (frame slot lframes frameoriginal)
  (format t "~&get-slot-value (frame : ~S - SLOT : ~s frameoriginal : ~s)~& " frame slot frameoriginal)
  
  ;; check for frame existence
  (unless (member frame lframes)
    (return-from get-slot-value nil))
  ;; check for slot presence
  (let ((slot-list (assoc slot (cdr (symbol-value frame))))
        demon value parent)
    ;; try to get a value locally
    (setq value
          (cond
           ;; if no slot, then nil
           ((null slot-list) nil)
           ;; slot there, try value value
           ((cadr (assoc 'VALUE (cdr slot-list))))
           ;; default value
           ((cadr (assoc 'DEFAULT (cdr slot-list))))
           ;; does not work try demon
           ((setq demon (getDemon frame 'IF-NEEDED slot))

            (funcall demon frameoriginal slot lframes))))
    ;; when value is non nil we return it, otherwise we try to inherit it from 
    ;; prototypes
    (if value 
        (return-from get-slot-value value)
      ;; otherwise get IS-A slot (attention appliquer sur frameoriginal)
      (progn
        (setq parent (cadadr (assoc 'IS-A (cdr (symbol-value frame)))))
        ;; if there, call a new function
        (when parent 
          (get-slot-value parent slot lframes frameoriginal))))))

(get-slot-value 'Elephant1 'AGE *frames* 'Elephant1)
(get-slot-value 'Elephant2 'COLOR *frames* 'Elephant2)

(get-slot-value 'Elephant3 'COLOR *frames* 'Elephant3)


(get-slot-value 'Elephant2 'AGE *frames* 'Elephant2)
(get-slot-value 'Elephant2 'Yeux *frames* 'Elephant2)

(get-slot-value 'Elephant5 'POIDS *frames* 'Elephant5)
(get-slot-value 'Elephant6 'POIDS *frames* 'Elephant6)


(get-slot-value 'Elephant4 'AGE *frames* 'Elephant4)
(get-slot-value 'Elephant4 'COLOR *frames* 'Elephant4)
(get-slot-value 'Elephant8 'COLOR *frames* 'Elephant8)


(get-slot-value 'Elephant25 'AGE *frames* 'Elephant25)
(get-slot-value 'Elephant25 'Yeux *frames* 'Elephant25)

(get-slot-value 'Elephant25 'POIDS *frames* 'Elephant25)
(get-slot-value 'Elephant29 'POIDS *frames* 'Elephant29)
































