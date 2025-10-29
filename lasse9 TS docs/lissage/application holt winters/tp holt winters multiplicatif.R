data1<-read.csv2("C:/Users/HP/OneDrive/Bureau/Cours 2A/S4/time series/champagne mensuelle.csv")
View(data1)
str(data1)
h=12
Y=data1$Ventes
th=105-h
SM<-vector("numeric",th)
TM<-vector("numeric",th)
IM<-vector("numeric",th)
s=12;
HW_mult<- function(x){
  SM[2]=x[1];
  TM[2]=x[2];
  IM[1:12]=x[3:14];
  alpha1=x[15];
  mu1=x[16];
  delta=x[17]
  for (i in 3:12){
    SM[i]=alpha1*(Y[i]/IM[i])+(1-alpha1)*(SM[i-1]+TM[i-1])
    TM[i]=mu1*(SM[i]-SM[i-1])+(1-mu1)*TM[i-1]
  }
  mape=0
  for (i in 13:th){
    SM[i]= alpha1*(Y[i]/IM[i-s])+(1-alpha1)*(SM[i-1]+TM[i-1])
    TM[i]= mu1*(SM[i]-SM[i-1])+(1-mu1)*TM[i-1]
    IM[i]= delta*(Y[i]/SM[i])+(1-delta)*IM[i-s]
    mape= mape + abs(Y[i]-(SM[i-1]+TM[i-1])*IM[i-s])*100/Y[i]
  }
  mape = mape/(th-12)
  return(mape)
}
SM2= ((Y[3]/0.834204152)+(Y[2]/0.690034416)+(Y[1]/0.734250013))/3
TM2= ((Y[3]/0.834204152)-(Y[1]/0.734250013))/2
II<-vector("numeric",12)
II[1:12]=c(0.734250013,0.690034416,0.834204152,0.83618742,0.931687861,0.879451364,0.74618689,0.343741139,0.949493578,1.239983582,1.703606134,2.11117345)

x0<-c(SM2,TM2,II[1],II[2],II[3],II[4],II[5],II[6],II[7],II[8],II[9],II[10],II[11],II[12],0.5,0.5,0.5)
HW_mult(x0)
ob<-optim(x0,HW_mult, lower = c(rep(-Inf, 14), rep(0, 3)), upper = c(rep(Inf, 14), rep(1, 3)), method = "L-BFGS-B")#contraintes importantes a ne pas enlever 
x<-ob$par
HW_mult(x)
ob 
#----------------------
SM[2]=x[1];
TM[2]=x[2];
IM[1:12]=x[3:14];
alpha1=x[15];
mu1=x[16];
delta=x[17]

for (i in 3:12){
  SM[i]=alpha1*(Y[i]/IM[i])+(1-alpha1)*(SM[i-1]+TM[i-1])
  TM[i]=mu1*(SM[i]-SM[i-1])+(1-mu1)*TM[i-1]
}

for (i in 13:th){
  SM[i]= alpha1*(Y[i]/IM[i-s])+(1-alpha1)*(SM[i-1]+TM[i-1])
  TM[i]= mu1*(SM[i]-SM[i-1])+(1-mu1)*TM[i-1]
  IM[i]= delta*(Y[i]/SM[i])+(1-delta)*IM[i-s]
}
#-------------------
SM[th]
TM[th]
prev<-vector("numeric",h)
mape=0
for (i in 1:h)  {
  prev[i]=(SM[th]+i*TM[th])*IM[th+i-s]
  mape=mape+abs(Y[th+i]-prev[i])*100/Y[th+i]
}
prev
mape/h


adjusted_series_M <- vector("numeric",th-3)
for (i in 3:12) {
  adjusted_series_M[i] <- (SM[i] + TM[i]) * IM[i]
}
for (i in 13:th) {
  adjusted_series_M[i] <- (SM[i] + TM[i]) * IM[i-s]
}
windows(width = 10, height = 7)
par(mar = c(5, 4, 4, 2))
plot(3:th, Y[3:th], type = "l", col = "red", lwd = 2, 
     xlab = "Temps", ylab = "Valeurs", 
     main = "Série initiale et série ajustée")
lines(3:th, adjusted_series_M, col = "blue", lwd = 2)
legend("topright", legend = c("Série initiale (Y)", "Série ajustée "), 
       col = c("red", "blue"), lwd = 2)
