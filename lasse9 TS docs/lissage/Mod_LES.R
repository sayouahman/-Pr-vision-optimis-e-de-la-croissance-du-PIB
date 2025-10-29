Don <- read.csv2("C:/Users/HP/Downloads/Indices_Petroliers.csv",header = TRUE, sep = ";", quote = '"', stringsAsFactors = FALSE)
View(Don)
str(Don)
h=5
sy=Don$y;


th=nrow(Don)-h
yajust<-vector("numeric",th)#declaration du vecteur avec so type et sa taille 

LES<-function(x)   { #la fonction critere de comparaison
yajust[2]=x[1];
fa=x[2]
mape=0
for (i in 3:th) {
          yajust[i]=fa*sy[i]+(1-fa)*yajust[i-1]
          mape=mape+abs(sy[i]-yajust[i-1])*100/sy[i] 
                }
mape=mape/(th-2)
return(mape)
                   }
x0<-c((sy[1]+sy[2]+sy[3])/3,0.3)# choix des parametres du lissage F2= moyenne des 3 premiers et alpha =0.3
LES(x0)
ob<-optim(x0,LES, lower=c(-Inf,0), upper=c(Inf,1), method = "L-BFGS-B")#contraintes importantes a ne pas enlever 
x<-ob$par
LES(x)
ob

 

#Calcul du mod?le avec les param?tres optimales
 yajust[2]=x[1];fa=x[2];
for (i in 3:th) {
          yajust[i]=fa*sy[i]+(1-fa)*yajust[i-1]
                 }
yajust[th]
prev<-vector("numeric",h)

mape=0
for (i in 1:h)  {
     prev[i]=yajust[th]
     mape=mape+abs(sy[th+i]-prev[i])*100/sy[th+i]
                 }
prev
mape/h
