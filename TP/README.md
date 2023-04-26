Organisation : 

📁 TP1 - Montée en compétence Lisp

Ce TP a pour objectif d’avoir un premier contact avec le langage LISP avec le logiciel ACL (Allegro Common LISP). Cette prise en main nous permettra de comprendre davantage le fonctionnement du langage ainsi que quelques-unes de ses subtilités.

- comprendre les différents types d'objets présents
- comprendre comment fonctionne la profondeur d'une liste
- écrire des fonctions simples
- traiter des objets fonctionnels

📁 TP2 - Jeu de Nim

Le but de ce TP est de trouver différentes manières de faire gagner une IA au jeu de Nim. 
Pour rappel, le jeu de Nim est un jeu de pure stratégie. Il se joue avec un tas d’allumettes, chaque joueur peut retirer au maximum 3 allumettes par tour. Le joueur qui prend la dernière allumette a perdu. Le jeu de Nim est ainsi équivalent à se déplacer d’un sommet à un autre dans un arbre : les sommets représentent les diverses positions du jeu et les arêtes les transitions d’une position à une autre. Il montrera qu’il existe une stratégie optimale. 

Ce TP est divisé en 2 parties, chacune ayant un objectif différent. 

Le but de la première partie est d’étudier la fonction proposée et de déterminer quel type de recherche elle effectue pour la résolution du jeu de Nim. Nous allons également déterminer quelle recherche (en profondeur ou en largeur) semble la plus adaptée pour ce jeu et voir s’il est possible d’améliorer la fonction proposée. 

Pour la deuxième partie, le but est de programmer une IA capable d’améliorer sa stratégie au fur et à mesure de jeux successifs. Pour ceci, dès qu’un coup mènera à la victoire de l’IA, il sera ajouté à une base de données contenant les coups que l’IA peut jouer à partir d’un certain état. Ce système est propagé à tous les coups qui ont mené à la victoire. Ainsi, au prochain lancement du jeu, les probabilités que l’IA tombe sur un coup qui mène à la victoire seront augmentées. Ceci permettra de faire émerger des stratégies gagnantes.

📁 TP3 - Système expert

Dans le cadre du dernier TP de IA01, il nous est demandé de développer un système expert d’ordre 0+ qui puisse traiter une problématique du domaine de notre choix.
Le choix d'un sport peut être déterminant pour l'adoption d'une activité physique régulière et pour l'amélioration de la santé physique et mentale. Cependant, il n'est pas toujours facile de trouver le sport qui convient le mieux à ses goûts et à sa condition physique. En effet, avec la grande variété de sports disponibles, il peut être difficile de savoir par où commencer ou de s'y retrouver. C'est pourquoi nous avons décidé de développer un système expert de l'ordre 0+ pour aider les gens à trouver le sport qui leur convient le mieux.
Notre système expert s'intéresse à la problématique suivante : "quel sport est fait pour moi ?" et vise à recommander un sport en fonction du tempérament, des préférences et de la condition physique. Pour ce faire, nous avons cherché à enrichir notre domaine d’expertise sur le sujet pour pouvoir développer l’ensemble des règles utilisées par le moteur d’inférence pour analyser les données de l'utilisateur et lui associer les sports les plus adaptés.
