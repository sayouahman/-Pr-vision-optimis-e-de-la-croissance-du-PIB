data<-read.csv2("C:/Users/HP/OneDrive/Bureau/Cours 2A/S4/time series/tp4 Gardner-WissalBrika Ikhlas Azmaoui/data R tp4.csv")
View(data)
str(data)
h=12
Y=data$serie
th=nrow(data)-h
S<- vector("numeric",th)
T<- vector("numeric",th)

s=12
# fonction a minimiser 
DAN<- function(x){
  S[2]=x[1];
  T[2]=x[2];
  a=x[3];
  b=x[4];
  c=x[5]
  
  mape=0 ;
  for (i in 3:th) {
    S[i] = a* Y[i]+ (1-a)*(S[i-1]+c*T[i-1])
    T[i] = b*(S[i]-S[i-1])+(1-b)*c * T[i-1]
    mape= mape+abs(Y[i]-(S[i-1]+c*T[i-1]))*100/Y[i]
  }
  mape=mape/(th-2)
  return (mape)
}
#initialisation des parametres 

S1=(Y[1]+Y[2]+Y[3])/3
T1=(Y[3]-Y[1])/2
x0<-c(S1,T1,0.5,0.5,0.5)

DAN(x0)
ob<-optim(x0,DAN,lower=c(-Inf,-Inf,0,0,0), upper=c(Inf,Inf,1,1,1), method = "L-BFGS-B") 
x<-ob$par;
#les parametres du modele 
x
#le nouveau mape avec les nouveaux parametres (mape de l'ajustement)
DAN(x)
#calcul de la série ajustée 

S[2] = x[1];
T[2] = x[2] ;
a = x[3] ;
b = x[4];
c = x[5]
y_ajust<- vector("numeric",th-3)
for (i in  3 : th) {
  S[i] = a*Y[i]+ (1-a)*(S[i-1]+c*T[i-1])
  T[i] = b*(S[i]-S[i-1])+(1-b)*c*T[i-1]
  y_ajust[i-1]= S[i-1]+c*T[i-1]
}

# Calcul de RMSE pour l'ajustement
rmse_ajust <- sqrt(mean((Y[3:th] - y_ajust[2:(th-1)])^2))
rmse_ajust

# Calcul de MAE pour l'ajustement
mae_ajust <- mean(abs(Y[3:th] - y_ajust[2:(th-1)]))
mae_ajust

S[th]
T[th]

#calcul des prévisions

prev<-vector("numeric",h)
mape_prev=0
for (i in 1:h)  {
  somme = 0
  for (j in 1:i){
    somme = somme + (c^j * T[th])
  }
  prev[i] = S[th] + somme
  if (!is.na(Y[th+i])) {
    mape_prev = mape_prev + (abs(Y[th+i] - prev[i])) * 100/Y[th+i]
  }
}
prev

#Calcul de MAPE pour la prévision 

mape_prev/h

#Calcul de RMSE pour la prévision 
rmse_prev <- sqrt(mean((Y[(th + 1):(th + h)] - prev)^2))
rmse_prev

#Calcul de MAE pour la prévision
mae_prev <- mean(abs(Y[(th + 1):(th + h)] - prev))
mae_prev

#tracé graphique 

windows()
plot(Y, type="l", col="blue", ylim=c(min(Y, prev), max(Y, prev)), xlab="Temps", ylab="Valeurs", main="DAN")
lines(c(rep(NA, 2), y_ajust), col="red", lwd=2, lty=2)
lines((th+1):(th+h), prev, col="green", lwd=2)
legend("bottomright", legend=c("Série Observée", "Valeurs Ajustées", "Prévisions"), col=c("blue", "red", "green"), lty=c(1, 2, 1), lwd=c(1, 2, 2))

