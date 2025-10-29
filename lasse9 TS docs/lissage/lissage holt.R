data<-read.csv2("C:/Users/HP/Downloads/LED.csv")
View(data)
str(data)
h=5
sy=data$y;

th=nrow(data)-h
#------------------
SH<-vector("numeric",th)
TH<-vector("numeric",th)
holt<-function(x)   { #la fonction critere de comparaison
  SH[2]=x[1];
  TH[2]=x[2];
  lambda1=x[3];
  mu1=x[4]
  
  mape=0
  for (i in 3:th) {
    SH[i]=lambda1*sy[i]+(1-lambda1)*(SH[i-1]-TH[i-1])
    T[i]=mu1*(SH[i]-SH[i-1])+(1-mu1)*TH[i-1]
    mape=mape+abs(sy[i]-SH[i-1]-TH[i-1])*100/sy[i] 
  }
  mape=mape/(th-2)
  return(mape)
}

x0<-c((sy[1]+sy[2]+sy[3])/3,(sy[3]-sy[1])/2,0.4,0.3)# choix des parametres du lissage s2= moyenne des 3 premiers et alpha =0.5
holt(x0)
ob<-optim(x0,holt, lower=c(-Inf,0), upper=c(Inf,1), method = "L-BFGS-B")#contraintes importantes a ne pas enlever 
x<-ob$par
holt(x)
ob  
#----------
SH[2]=x[1];
TH[2]=x[2];
lambda1=x[3];
mu1=x[4]

for (i in 3:th) {
  SH[i]=lambda1*sy[i]+(1-lambda1)*(SH[i-1]-TH[i-1])
  TH[i]=mu1*(SH[i]-SH[i-1])+(1-mu1)*T[i-1]
}
SH[th]
prev<-vector("numeric",h)

mape=0
for (i in 1:h)  {
  prev[i]=SH[th]+i*TH[th]
  mape=mape+abs(sy[th+i]-prev[i])*100/sy[th+i]
}
prev
mape/h
x0  
#vaut mieux tracer le graphique des prévisions avec la valeur réelle pour vérifier la proximité 

