data<-read.csv2("C:/Users/HP/OneDrive/Bureau/Cours 2A/S4/time series/champagne mensuelle.csv")
View(data)
str(data)
h=12
Y=data$Ventes
th=nrow(data)-h
S<- vector("numeric",th)
T<- vector("numeric",th)
I<- vector("numeric",th)
s=12
DAM<- function(x){
  S[2]=x[1];
  T[2]=x[2];
  I[1:12]=x[3:14];
  a=x[15];
  b=x[16];
  c=x[17];
  d=x[18]
  
  for (i in 3:12){
    S[i]= a*(Y[i]/I[i])+(1-a)*(S[i-1]+b*T[i-1])
    T[i]= c*(S[i]-S[i-1])+(1-c)*b*T[i-1]
  }
  mape=0
  for (i in 13:th){
    S[i]= a*(Y[i]/I[i-s])+(1-a)*(S[i-1]+b*T[i-1])
    T[i]= c*(S[i]-S[i-1])+(1-c)*b*T[i-1]
    I[i]= d*(Y[i]/S[i])+(1-d)*I[i-s]
    mape= mape + abs(Y[i]-(S[i-1]+b*T[i-1])*I[i-s])*100/Y[i]
  }
  mape= mape/(th-12)
  return (mape)
}
S1= ((Y[3]/0.831521553)+(Y[2]/0.68070087)+(Y[1]/0.701207313))/3
T1= ((Y[3]/0.831521553)-(Y[1]/0.701207313))/2
I1<-vector("numeric",12)
I1[1:12]=c(0.701207313,0.68070087,0.831521553,0.887409571,1.011699562,0.845411418,0.658253407,0.641694165,0.846854245,1.234279155,1.62736748,2.039846565)

x0<-c(S1,T1,I1[1],I1[2],I1[3],I1[4],I1[5],I1[6],I1[7],I1[8],I1[9],I1[10],I1[11],I1[12],0.5,0.5,0.5,0.5)
DAM(x0)
ob<-optim(x0,DAM, lower = c(rep(-Inf, 18), rep(0, 4)), upper = c(rep(Inf, 18), rep(1, 4)), method = "L-BFGS-B")#contraintes importantes a ne pas enlever 
x<-ob$par
DAM(x)
#-------------
S[2]=x[1];
T[2]=x[2];
I[1:12]=x[3:14];
a=x[15];
b=x[16];
c=x[17];
d=x[18]
y_ajust<- vector("numeric",th-3)

for (i in 3:12){
  S[i]= a*(Y[i]/I[i])+(1-a)*(S[i-1]+b*T[i-1])
  T[i]= c*(S[i]-S[i-1])+(1-c)*b*T[i-1]
}

for (i in 13:th){
  S[i]= a*(Y[i]/I[i-s])+(1-a)*(S[i-1]+b*T[i-1])
  T[i]= c*(S[i]-S[i-1])+(1-c)*b*T[i-1]
  I[i]= d*(Y[i]/S[i])+(1-d)*I[i-s]
  y_ajust[i-1]= S[i-1]*I[i-s]
  
}
y_ajust
prev<-vector("numeric",h)
mape_prev=0
for (i in 1:h)  {
  T2=0
  for (j in 1:h){
    T2=T2+(b^j)*T[i]
  }
  prev[i]=(S[th]+T2)*I[th+i-s]
  mape_prev=mape_prev+abs(Y[th+i]-prev[i])*100/Y[th+i]
}
prev
mape_prev/h

windows()
plot(Y, type="l", col="blue", ylim=c(min(Y, prev), max(Y, prev)), xlab="Temps", ylab="Valeurs", main="DAM multiplicatif")
lines(c(rep(NA, 2), y_ajust), col="red", lwd=2, lty=2)
lines((th+1):(th+h), prev, col="green", lwd=2)
legend("bottomright", legend=c("Série Observée", "Valeurs Ajustées", "Prévisions"), col=c("blue", "red", "green"), lty=c(1, 2, 1), lwd=c(1, 2, 2))


