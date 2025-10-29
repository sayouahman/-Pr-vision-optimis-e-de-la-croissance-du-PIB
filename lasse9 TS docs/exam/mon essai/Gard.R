
data<-read.csv2("C:/Users/HP/OneDrive/Bureau/Cours 2A/S4/préparation  exams/lasse9 TS docs/exam/mon essai/data for exam 2022-2023(1).csv")
View(data)
str(data)

h=12
Y=data$Sweetwhite
th=nrow(data)-h

S<- vector("numeric",th)
T<- vector("numeric",th)
I<- vector("numeric",th)

s=12

DAM<- function(x){
  S[2]=x[1];
  T[2]=x[2];
  I[1:12]=x[3:14];
  alpha=x[15];
  gamma=x[16];
  phi=x[17];
  delta=x[18]
  
  for (i in 3:12){
    S[i]= alpha*(Y[i]/I[i])+(1-alpha)*(S[i-1]+phi*T[i-1])
    T[i]= gamma*(S[i]-S[i-1])+(1-gamma)*phi*T[i-1]
  }
  mape=0
  for (i in 13:th){
    S[i]= alpha*(Y[i]/I[i-s])+(1-alpha)*(S[i-1]+phi*T[i-1])
    T[i]= gamma*(S[i]-S[i-1])+(1-gamma)*phi*T[i-1]
    I[i]= delta*(Y[i]/S[i])+(1-delta)*I[i-s]
    mape= mape + abs(Y[i]-(S[i-1]+phi*T[i-1])*I[i-s])*100/Y[i]
  }
  mape= mape/(th-12)
  return (mape)
}

#initialisation 
I0=data$I0
S2= ((Y[3]/I0[3])+(Y[2]/I0[2])+(Y[1]/I0[1]))/3
T2= ((Y[3]/I0[3])-(Y[1]/I0[1]))/2


#I2<-vector("numeric",12)
#I1[1:12]=c(0.701207313,0.68070087,0.831521553,0.887409571,1.011699562,0.845411418,0.658253407,0.641694165,0.846854245,1.234279155,1.62736748,2.039846565)

#x0<-c(S2,T2,I2[1],I2[2],I2[3],I2[4],I2[5],I2[6],I2[7],I2[8],I2[9],I2[10],I2[11],I2[12],0.5,0.5,0.5,0.5)

x0<-c(S2,T2,I0[1:12],0.5,0.5,0.5,0.5)

DAM(x0)
ob<-optim(x0,DAM, lower = c(rep(-Inf, 18), rep(0, 4)), upper = c(rep(Inf, 18), rep(1, 4)), method = "L-BFGS-B")#contraintes importantes a ne pas enlever 
x<-ob$par
DAM(x)

#-------------


S[2]=x[1];
T[2]=x[2];
I[1:12]=x[3:14];
alpha=x[15];
gamma=x[16];
phi=x[17];
delta=x[18]

y_ajust<- vector("numeric",th)

for (i in 3:12){
  S[i]= alpha*(Y[i]/I[i])+(1-alpha)*(S[i-1]+phi*T[i-1])
  T[i]= gamma*(S[i]-S[i-1])+(1-gamma)*phi*T[i-1]
}

for (i in 13:th){
  S[i]= alpha*(Y[i]/I[i-s])+(1-alpha)*(S[i-1]+phi*T[i-1])
  T[i]= gamma*(S[i]-S[i-1])+(1-gamma)*phi*T[i-1]
  I[i]= delta*(Y[i]/S[i])+(1-delta)*I[i-s]
  y_ajust[i] <-(S[i-1] + phi * T[i-1])*I[i-s]
  
}
y_ajust

#previsions

prev <- vector("numeric", h)
mape <- 0
prev[1] <- (S[th] + phi * T[th])*I[th+h-s]
mape <- mape + abs(Y[th + 1] - prev[1]) * 100 / Y[th + 1]
for (i in 2:h) {
  prev[i] <- prev[i - 1] + (phi ^ i) * T[th] * I[th+h-s]
  mape <- mape + abs(Y[th + i] - prev[i]) * 100 / Y[th + i]
}
prev
mape / h
