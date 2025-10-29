data<-read.csv2("C:/Users/HP/Downloads/LED.csv")
View(data)
str(data)
h=5
sy=data$y;

th=nrow(data)-h
S<-vector("numeric",th)
T<-vector("numeric",th)
LED<-function(x)   { #la fonction critere de comparaison
  S[2]=x[1];
  T[2]=x[2];
  alpha=x[3];
  lambda=2*alpha-alpha*alpha;
  mu=alpha/(2-alpha)
  mape=0
  for (i in 3:th) {
    S[i]=lambda*sy[i]+(1-lambda)*(S[i-1]-T[i-1])
    T[i]=mu*(S[i]-S[i-1])+(1-mu)*T[i-1]
    mape=mape+abs(sy[i]-S[i-1]-T[i-1])*100/sy[i] 
  }
  mape=mape/(th-2)
  return(mape)
}
  
x0<-c((sy[1]+sy[2]+sy[3])/3,(sy[3]-sy[1])/2,0.5)# choix des parametres du lissage s2= moyenne des 3 premiers et alpha =0.5
  LED(x0)
  ob<-optim(x0,LED, lower=c(-Inf,0), upper=c(Inf,1), method = "L-BFGS-B")#contraintes importantes a ne pas enlever 
  x<-ob$par
  LED(x)
  ob  
#----------
  S[2]=x[1];
  T[2]=x[2];
  alpha=x[3];
  lambda=2*alpha-alpha*alpha;
  mu=alpha/(2-alpha)
  
  for (i in 3:th) {
    S[i]=lambda*sy[i]+(1-lambda)*(S[i-1]-T[i-1])
    T[i]=mu*(S[i]-S[i-1])+(1-mu)*T[i-1]
  }
  S[th]
  prev<-vector("numeric",h)
  
  mape=0
  for (i in 1:h)  {
    prev[i]=S[th]+i*T[th]
    mape=mape+abs(sy[th+i]-prev[i])*100/sy[th+i]
  }
  prev
  mape/h
x0  
#vaut mieux tracer le graphique des prévisions avec la valeur réelle pour vérifier la proximité 

