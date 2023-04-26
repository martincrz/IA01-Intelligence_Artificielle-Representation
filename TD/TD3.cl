;Exercice 1

(defun f1 (L)
    (dolist (x L 'Fin)
      (print x)))

(defun f2 (L)
    (mapcar 'print L))

(defun f3 (L)
    (if L
       (progn
          (print (car L))
          (f3 (cdr L)))))

(defun f4 (L)
    (dotimes (i (length L) NIL)
      (print (pop L))))

(defun f4 (L)
    (dotimes (i (length L) NIL)
      (print (car L))
      (setq l (cdr l))))


(defun f5 (L)
  (if L
      (progn
        (print (pop L))
        (f5 L))))

(defun f6(L) 
  (loop for idx from 0 to (- (list-length L) 1) do 
        (print (nth idx L)) ) )

(defun f7 (L) 
     (loop 
        (print (pop L)) 
       (if (not L) (return-from nil))))

(defun f8 (L)
  (loop for x in L
        do (print x)))

(defvar l '(AA BB CC DD 4))
(f1 l)
(f2 l)
(f3 l)
(f4 l)
(f5 l)
(f7 l)
(f8 l)
 
;Exercice 2

;1) Représentation : 
(defvar *html* '(html 
               (header 
                  (title "Ma page")) 
               (body 
                  (h1 "Un titre") 
                  (p "Soror et aemula Romae"))))

;; (format t "Bonjour ~s" nom)
;;   ~s affichage string
;;   ~a affichage string sans guillements
;;   ~% ou ~& retour à la ligne
;;   ~t tabulation
;;   ~5@t 5 tabulations
;;   ~V@t V tabulations (V = un nombre) = (format t "V@t Bonjour ~s" 3 nom)

;; Algo
;; Si la liste est une liste alors
;;     Afficher la balise ouvrante
;;     Pour le reste de la liste, pour chaque élément
;;          Appeler make-html avec cet élément
;;     Afficher la balise fermante
;; Sinon
;;     Afficher le texte

(defun make-html (code) 
  (if (listp code)
      (progn (format t "<~s>~&" (car code))
        (dolist (x (cdr code))
          (make-html x))
        (format t "</~s>~&"(car code))
        )
    (format t "~s ~&" code)
    )
  )

(make-html *html*)    ;Test de la fonction make-html
 
;On utilise (format T "texte ~s texte" x), T pour la console (qu'on peut remplacer par le nom de fichier), 
;~s pour afficher une chaîne de caractères, ~t une tabulation,
;~a pour enlever les "", ~% ou ~& pour un retour à la ligne et x sera le texte à remplacer

;on appelera la fonction make-html avec i=0, qui servira d'indice pour les indentations
(defun make-html (L i)
  (if (listp L)
      (progn 
        (format t "~V@t<~s>~&" i (car L))
        (dolist (x (cdr L))
          (make-html x (+ i 3)))
        (format t "~V@t</~s>~&" i (car L))
        )
    (format t "~V@t~s ~&" i L)
    )
  )

(make-html *html* 0)    ;Test de la fonction make-html

(defun make-html-fichier (code fichier) 
  (if (listp code)
      (progn (format fichier "<~s>~&" (car code))
        (dolist (x (cdr code))
          (make-html-fichier x fichier))
        (format fichier "</~s>~&"(car code))
        )
    (format fichier "~s ~&" code)
    )
  )

(defun fichier-html (liste nom)
  (with-open-file (file nom 
                        :if-does-not-exist :create
                        :if-exists :overwrite
                        :direction :output)
    (make-html-fichier liste file)
    )
  "fichier généré"
  )

(fichier-html *html* "test.htm")
