mydata<-read.csv2("C:/Users/HP/OneDrive/Bureau/Cours 2A/S4/préparation  exams/lasse9 TS docs/exam/mon essai/data exam 2022-2023 copie.csv")
View(mydata)
str(mydata)#afficher la structure de la série car il se peut qu'elle soit en format texte et dans ce cas tout ce qui vient ne marche pas donc il faut s'assurer que la série est numérique
th=nrow(mydata)
v=rep(1,th)#creer un vecteur remplie des 1 , il nous aidera dans les calculs


#mape, modele : exponentiel modifié
mapeB<-function(x) (t(abs(mydata[1:th,4]-x[1]*x[2]^mydata[1:th,1]-x[3])*100/mydata[1:th,4])%*%v)/th #mape = moyenne(abs(série temporelle - série expliquée))

#vecteur initial 
x0<-c(150,1,-100)#pour utiliser la fonction numérique il faut utiliser un vecteur initial trouvé par la méthode des 3 points ou aléatoire (on peut vérifier alpha beta et gamma du tableau excel)
mapeB(x0)#mape avec la solution initiale 

#Calcul du minimum de mapeB
solution<-optim(x0,mapeB)#la fonction optim admet deux parametres le vecteur initial et la fonction pour qu'elle génere la suite de solutions 
solution #ca donne une nouvelle solution 
x<-solution$par
mapeB(x)# mape avec la solution optimale 
