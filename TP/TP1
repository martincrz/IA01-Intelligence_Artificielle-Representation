TP 1 - Montée en compétences Lisp


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
EXERCICE 1 - Mise en condition 


1. Déterminer le type des objets lisp

35 : atome (nombre)
(35) : liste (profondeur 0)
(((3) 5) 6) : liste (profondeur 2)
-34RRRR : atome (symbole -> une variable)
T : atome (symbole à valeur prédéfinie et non modifiable)
NIL : atome (symbole à valeur prédéfinie et non modifiable)
() : liste vide




2. Traduire une liste en arbre

voir rapport




3. Que font les appels de fonctions

> (CADR (CDR (CDR (CDR '(DO RE MI FA SOL LA SI)))))
> (CADR (CDR (CDR '(RE MI FA SOL LA SI))))
> (CADR (CDR  '(MI FA SOL LA SI)))
> (CADR '(FA SOL LA SI))
SOL

> (CONS (CADR '((A B)(C D))) (CDDR '(A (B (C)))))
> (CONS (C D) NIL)
((C D))

> (CONS (CONS 'HELLO NIL) '(HOW ARE YOU))
> (CONS  '(HELLO) '(HOW ARE YOU))
((HELLO) HOW ARE YOU)

> (CONS 'JE (CONS 'JE (CONS 'JE (CONS 'BALBUTIE NIL))))
> (CONS 'JE (CONS 'JE (CONS 'JE '(BALBUTIE))))
> (CONS 'JE (CONS 'JE '(JE BALBUTIE)))
> (CONS 'JE '(JE JE BALBUTIE))
(JE JE JE BALBUTIE)

> (CADR (CONS 'TIENS (CONS '(C EST SIMPLE) ())))
> (CADR (CONS 'TIENS '((C EST SIMPLE))))
> (CADR '(TIENS (C EST SIMPLE)))
(C EST SIMPLE)




4. Ecrire des fonctions

Ecrire nombres3 ( L) tel que (nombres3 '( 1 2 3 R S 4)) -> BRAVO

(defun nombres3 (L)
  (if (listp L)
      (if (and (integerp (car L))(integerp (cadr L))(integerp(caddr L)))
          'BRAVO
        'PERDU)
       (format t "Ceci n est pas une liste")
    )
 )

> (nombres3 '( 1 2 3 R S 4))
BRAVO
> (nombres3 'A)
Ceci n est pas une liste
NIL
> (nombres3 '(2 3))
PERDU


Ecrire grouper (L1 L2) tel que (grouper ‘(1 2 3) ‘(4 5 6)) -> ((1 4)(2 5)(3 6))

(defun grouper(L1 L2)
  (if (and (listp L1) (listp L2))
      (mapcar #'(lambda (x y) (list x y)) L1 L2)
    (format t "Veuillez passer des listes en argument")
  )
)

> (grouper '(1 2 3) '(4 5 6))
((1 4) (2 5) (3 6))
> (grouper 1 'A)
Veuillez passer des listes en argument
NIL
     

Ecrire monReverse(L) tel que (monReverse ‘(1 2 3 4 5)) -> (5 4 3 2 1)

(defun monReverse(l)
  (if (listp l)
      (if (null l) '() (append (monReverse (cdr l)) (list (car l))))
      (format t "Veuillez passer une liste en argument")
  )
)

exemples :
> (monReverse '(3 2))
(2 3)
> (monReverse '(A 2 Y 9 N))
(N 9 Y 2 A)
> (monReverse '3)
Veuillez passer une liste en argument
NIL


(defun monReverse(liste)
  (let ((l '()))
  (dotimes (x (length liste))
    (setq l (append (list (pop liste)) l ))
    )l)
 )

> (monReverse '(1 2 3))
(3 2 1)


Ecrire palindrome(L) tel que (palindrome ‘(x a m a x) ) -> T

(defun palindrome(L)
  (if (listp L)
      (if (equal L (monReverse L)) T NIL)
    (format t "Veuillez passer une liste en argument")
  ))

exemples : 
> (palindrome '(x a m a x))
T
> (palindrome '(x i m a x))
NIL
> (palindrome '1)
Veuillez passer une liste en argument
NIL




* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
EXERCICE 2 - Objets fonctionnels

(defun list_triple_couple(l)
  (if (listp l)
      (mapcar #'(lambda (x)
                  (if (integerp x)
                  (list x (* x 3)) (format t "Non entier"))
                  ) l)
           (format t "Veuillez passer une liste en argument")
  )
)



exemples : 
> (list_triple_couple '(3 5 7))
((3 9) (5 15) (7 21))
> (list_triple_couple 5)
Veuillez passer une liste en argument
NIL

> (list_triple_couple '(A 5 7))
Non entier
(NIL (5 15) (7 21))



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
EXERCICE 3 - a-list

(defun my-assoc(cle liste)
  (if (listp liste)
    (dolist (y liste)
           (if (equal (car y) cle) (return y) 'NIL)
      )
      (format t "Veuillez passer une liste en argument")
  ))


Exemples :
> (my-assoc 'Pierre '((Yolande 25) (Pierre 22) (Julie 45)))
(Pierre 22)
> (my-assoc 'Pierre '2)
Veuillez passer une liste en argument
NIL


Cas plusieurs occurences :

(defun my-assoc(cle liste)
  (if (listp liste)
  (let (
    (l '()))
    (dolist (y liste)
        (if (equal (car y) cle) (setq l (append l (list y))) 'NIL)
      )
     l
   )(format t "Veuillez passer une liste en argument") )
)


exemples : 
> (my-assoc 'Pierre '((Yolande 25) (Pierre 22) (Julie 45) (Pierre 91)))
((PIERRE 22) (PIERRE 91))
> (my-assoc 'Yves '((Yolande 25) (Pierre 22) (Julie 45)))
NIL



(defun cles(liste)
  (if (listp liste)
      (mapcar #'(lambda(x) (car x)) liste)
      (format t "Veuillez passer une liste en argument"))
)


exemples : 
> (cles '((Yolande 25) (Pierre 22) (Julie 45)))
(YOLANDE PIERRE JULIE)
> (cles '((Yolande 25) (Pierre 22) (Yolande 28) (Julie 45)))
(YOLANDE PIERRE YOLANDE JULIE)



creation (listeCles listeValeurs)

(defun creation(listeCles listeValeurs)
  (if (and (listp listeCles) (listp listeValeurs))
      (mapcar #'(lambda (x y)
              (list x y)) listeCles listeValeurs)
      (format t "Veuillez passer une liste en argument")
  )
)


exemples : 
> (creation '(Yolande Pierre Julie) '(25 22 45))
((YOLANDE 25) (PIERRE 22) (JULIE 45))
> (creation '2 '3)
Veuillez passer une liste en argument
NIL



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
EXERCICE 4 - Gestion d’une base de connaissances en Lisp



(setq BaseTest
 '(

     ("Guerre de Burgondie" 523 533 (("Royaume franc")("Royaume des Burgondes")) ("Vézeronce" "Arles"))

     ("Conquête de la Thuringe" 531 531 (("Royaume franc")("Thuringes")) ("Thuringe"))

     ("Guerre des Goths" 535 553 (("Royaume ostrogoth" "Alamans" "Royaume franc" "Royaume wisigoth" "Burgondes")("Empire byzantin")) ("Péninsule italienne"))

     ("Conquête de l'Alémanie" 536 536 (("Royaume franc")("Alamans")) ("Alémanie"))

     ("Conquête de la Bavière" 555 555 (("Royaume franc")("Bavarii")) ("Bavière"))

     ("Campagnes de Bretagne" 560 578 (("Royaume franc")("Royaume du Vannetais")) ("Vannetais"))

     ("Guerre de succession mérovingienne" 584 585 (("Royaume franc")("Royaume d'Aquitaine")) ("Comminges "))

     ("Guerre franco-frisonne" 600 793 (("Royaume franc")("Royaume de Frise")) ("Pays-Bas" "Allemagne"))

     ("Guerre civile des Francs" 715 719 (("Neustrie")("Austrasie")) ("Royaume franc"))

     ("Invasion omeyyade en France" 719 759 (("Royaume franc")("Califat omeyyade")) ("Royaume d'Aquitaine" "Septimanie"))

     ("Guerre des Lombards" 755 758 (("Royaume franc")("Lombards")) ("Lombardie"))

     ("Guerre d'Aquitaine" 761 768 (("Royaume franc")("Aquitains")) ("Vasconie Aquitaine"))

     ("Guerre des Saxons" 772 804 (("Royaume franc")("Saxons")) ("Germanie"))

     ("Guerre des Lombards" 773 774 (("Royaume franc")("Lombards")) ("Lombardie"))

     ("Guerre des Avars" 791 805 (("Royaume de France")("Avars")) ("Pannonie"))

     ("Invasions sarrasines en Provence" 798 990 (("Royaume de France" "Comté de Provence")("Sarrasins")) ("Provence"))

     ("Guerre civile entre les fils de Louis le Pieux" 830 842 (("Francie occidentale" "Francie orientale")("Francie médiane")) ("Fontenoy"))

     ("Guerre franco-bretonne" 843 851 (("Royaume de France")("Royaume de Bretagne" "Vikings")) ("Royaume de Bretagne"))

     ("Luttes inter-dynastiques carolingiennes" 876 946 (("Francie occidentale" "Francie orientale")("Royaume de Bourgogne" "Francie orientale")) ("Ardennes" "Saône-et-Loire" "Rhénanie-Palatinat" "Aisne"))

     ("Invasions vikings en France" 799 1014 (("Royaume de France")("Vikings")) ("Normandie" "Bretagne"))

     ("Première croisade" 1096 1099 (("Comté de Blois" "Comté de Toulouse" "Comté de Boulogne" "Marquisat de Provence" "Comté de Flandre" "Duché de Normandie" "Diocèse du Puy-en-Velay" "Comté de Vermandois" "République de Gênes" "Duché de Basse-Lotharingie" "Principauté de Tarente" "Empire byzantin" "Royaume de Petite-Arménie" "Croisés" "Royaume de France")("Sultanat de Roum" "Danichmendides" "Califat fatimide")) ("Terre sainte"))
 )
)



(defun nomConflit (conflit)
  (car conflit))

(defun dateDebut (conflit)
  (cadr conflit))

(defun allies(conflit)
  (car (cadddr conflit)))

(defun ennemis(conflit)
  (cadr (cadddr conflit)))

(defun lieu (conflit)
  (car(last conflit)))


Tests:
> (nomConflit '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))) 
" Guerre de Burgondie "
> (dateDebut '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles")))
523
> (allies '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))) 
("Royaume Franc") 
> (ennemis '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))) 
("Royaume des Burgondes") 
> (lieu '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))) 
("Vezeronce" "Arles")



FB1 : affiche tous les conflits

(defun FB1 (dataBase)
  (dolist (conflit dataBase 'finBDD)
    (print (nomConflit conflit))))



FB2 : affiche les conflits du "Royaume Franc"

(defun membre (item liste)
  (dolist (x liste)
    (if (equal x item) (return 'T) 'F)))


(defun FB2 (dataBase)
  (dolist (conflit dataBase 'finBDD)
    (if (membre '"Royaume franc" (allies conflit)) (print (nomConflit conflit)) 'non)
    )
 )


Exemples :
> (FB2 BaseTest)
"Guerre de Burgondie"
"Conquête de la Thuringe"
"Guerre des Goths"
"Conquête de l'Alémanie"
"Conquête de la Bavière"
"Campagnes de Bretagne"
"Guerre de succession mérovingienne"
"Guerre franco-frisonne"
"Invasion omeyyade en France"
"Guerre des Lombards"
"Guerre d'Aquitaine"
"Guerre des Saxons"
"Guerre des Lombards"
FINBDD


FB3 : retourne la liste des conflits dont un allié est précisé en argument

(defun FB3-beta (dataBase allie)
  (dolist (conflit dataBase 'finBDD)
    (if (membre allie (allies conflit)) (print conflit))))


(defun FB3 (dataBase allie)
  (let ((liste '()))
    (dolist (conflit dataBase 'finBDD)
        (if (membre allie (allies conflit)) (setq liste (append liste (list(nomConflit conflit)))))
     )liste
 ))


Exemple :
> (FB3 BaseTest "Royaume franc")
("Guerre de Burgondie" "Conquête de la Thuringe" "Guerre des Goths"
 "Conquête de l'Alémanie" "Conquête de la Bavière" "Campagnes de Bretagne"
 "Guerre de succession mérovingienne" "Guerre franco-frisonne"
 "Invasion omeyyade en France" "Guerre des Lombards" ...)
> (FB3 BaseTest "Alamans")
("Guerre des Goths")



FB4 : retourne le conflit dont la date de début est 523

(defun FB4 (dataBase)
  (dolist (conflit dataBase 'finBDD)
    (if (equal 523 (dateDebut conflit)) (return (nomConflit conflit)))     ))

Exemple :
> (FB4 BaseTest)
"Guerre de Burgondie"

Cas plusieurs conflits ont meme date de depart:

(defun FB4 (dataBase)
  (let ((liste '()))  
  (dolist (conflit dataBase 'finBDD)
    (if (equal 523 (dateDebut conflit)) (setq liste (append liste (list(nomConflit conflit)))))
   )
    liste))

Exemple:
> (FB4 BaseTest)
("Guerre de Burgondie")



FB5 : retourne la liste des conflits dont la date de début est entre 523 et 715

(defun FB5 (dataBase)
  (let ((liste '()))  
  (dolist (conflit dataBase 'finBDD)
    (if (and (>= (dateDebut conflit) 523) (<= (dateDebut conflit) 715))
        (setq liste (append liste (list(nomConflit conflit)))))
   )
   liste))


> (FB5 BaseTest)
("Guerre de Burgondie" "Conquête de la Thuringe" "Guerre des Goths"
 "Conquête de l'Alémanie" "Conquête de la Bavière" "Campagnes de Bretagne" "Guerre de succession mérovingienne" "Guerre franco-frisonne"
 "Guerre civile des Francs")



FB6 : calcule et retourne le nombre de conflits ayant pour ennemis les "Lombards”

(defun FB6 (dataBase)
  (let ((nbre_conflits '0))
    (dolist (conflit dataBase 'finBDD)
      (if (membre '"Lombards" (ennemis conflit)) (setq nbre_conflits(+ 1 nbre_conflits)))
    )
    nbre_conflits
  )
)

> (FB6 BaseTest)
2
