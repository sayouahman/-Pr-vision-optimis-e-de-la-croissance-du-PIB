mydata<-read.csv2("C:/Users/HP/Downloads/gnp_annuel.csv")
View(mydata)
str(mydata)#afficher la structure de la série car il se peut qu'elle soit en format texte et dans ce cas tout ce qui vient ne marche pas donc il faut s'assurer que la série est numérique
th=nrow(mydata)-5
v=rep(1,th)

#mape, modele : exponentiel modifié
mapeB<-function(x) (t(abs(mydata[1:th,3]-x[1]*x[2]^mydata[1:th,1]-x[3])*100/mydata[1:th,3])%*%v)/th #série temporelle - série expliquée

#vecteur initial 
x0<-c(29.3721198,1.019228402,-12.43400673)#pour utiliser la fonction numérique il faut utilisé un vecteur initial trouvé par la méthode des 3 points (on peut vérifier alpha beta et gamma du tableau excel)
mapeB(x0)

#Calcul du minimum de mapeB
solution<-optim(x0,mapeB)#la fonction optim admet deux parametres le vecteur initial et la fonction pour qu'elle génere la suite de solutions 
solution #ca donne une nouvelle solution 
x<-solution$par
mapeB(x)
 
#mape, mod?le : Gomp
mapeB<-function(x) (t(abs(mydata[1:th,3]-exp(x[1]*x[2]^mydata[1:th,1]+x[3]))*100/mydata[1:th,3])%*%v)/th

#vecteur initial
x0<-c(-5.8499094,0.99498853,8.70691616)
mapeB(x0)

#Calcul du minimum de mapeB
solution<-optim(x0,mapeB)
solution
x<-solution$par
mapeB(x)

#mape, mod?le : Logistique
mapeB<-function(x) (t(abs(mydata[1:th,3]-1/(x[1]*x[2]^mydata[1:th,1]+x[3]))*100/mydata[1:th,3])%*%v)/th

#vecteur initial
x0<-c(0.05315774,0.9710169,0.00340247)
mapeB(x0)

#Calcul du minimum de mapeB
solution<-optim(x0,mapeB)
solution
x<-solution$par
mapeB(x)

#mse mod?le : exponentiel modifi?
mseB<-function(x) (t(mydata[1:th,3]-x[1]*x[2]^mydata[1:th,1]-x[3])%*%(mydata[1:th,3]-x[1]*x[2]^mydata[1:th,1]-x[3])/th)

#vecteur initial
x0<-c(29.3721198,1.019228402,-12.43400673)
mseB(x0)

#Calcul du minimum de mapeB
solution<-optim(x0,mseB)
solution
x<-solution$par
mseB(x)

#mse mod?le : Gomp
mseB<-function(x) (t(mydata[1:th,3]-exp(x[1]*x[2]^mydata[1:th,1]+x[3]))%*%(mydata[1:th,3]-exp(x[1]*x[2]^mydata[1:th,1]+x[3]))/th)

#vecteur initial
x0<-c(-5.8499094,0.99498853,8.70691616)
mseB(x0)

#Calcul du minimum de mapeB
solution<-optim(x0,mseB)
solution
x<-solution$par
mseB(x)

#mse mod?le : Logistique
mseB<-function(x) (t(mydata[1:th,3]-1/(x[1]*x[2]^mydata[1:th,1]+x[3]))%*%(mydata[1:th,3]-1/(x[1]*x[2]^mydata[1:th,1]+x[3]))/th)

#vecteur initial
x0<-c(0.05315774,0.9710169,0.00340247)
mseB(x0)

#Calcul du minimum de mapeB
solution<-optim(x0,mseB)
solution
x<-solution$par
mseB(x)

