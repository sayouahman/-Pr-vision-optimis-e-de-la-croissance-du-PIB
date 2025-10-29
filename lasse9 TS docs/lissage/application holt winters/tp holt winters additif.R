data<-read.csv2("C:/Users/HP/OneDrive/Bureau/Cours 2A/S4/time series/H-W add0.csv")
View(data)
str(data)
h=4
sy=data$y;
th=56-h
S<-vector("numeric",th)
T<-vector("numeric",th)
I<-vector("numeric",th)
HW_add<-function(x){
  S[2]=x[1];
  T[2]=x[2];
  I[1]=x[3];
  I[2]=x[4];
  I[3]=x[5];
  I[4]=x[6];
  alpha=x[7];
  mu=x[8];
  d=x[9]
  
  for (i in 3:4){
    S[i]=alpha*(sy[i]-I[i])+(1-alpha)*(S[i-1]+T[i-1])
    T[i]=mu*(S[i]-S[i-1])+(1-mu)*T[i-1]
  }
  mape=0;
  s=4
  for (i in 5:th){
    S[i]= alpha*(sy[i]-I[i-s])+(1-alpha)*(S[i-1]+T[i-1])
    T[i]= mu*(S[i]-S[i-1])+(1-mu)*T[i-1]
    I[i]= d*(sy[i]-S[i])+(1-d)*I[i-s]
    mape= mape + abs(sy[i]-S[i-1]-T[i-1]-I[i-s])*100/sy[i]
  }
  mape = mape/(th-4)
  return(mape)
}
S2= ((sy[3]-2.5625)+(sy[2]-1.5375)+(sy[1]+2.4625))/3
T2= ((sy[3]-2.5625)-(sy[1]+2.4625))/2
I1= -2.4625
I2= 1.5375
I3= 2.5625
I4= -1.6875

x0<-c(S2,T2,I1,I2,I3,I4,0.5,0.5,0.5)
HW_add(x0)
ob<-optim(x0,HW_add, lower = c(rep(-Inf, 6), rep(0, 3)), upper = c(rep(Inf, 6), rep(1, 3)), method = "L-BFGS-B")#contraintes importantes a ne pas enlever 
x<-ob$par
HW_add(x)
ob 

#-------------------------
S[2]=x[1];
T[2]=x[2];
I[1]=x[3];
I[2]=x[4];
I[3]=x[5];
I[4]=x[6];
alpha=x[7];
mu=x[8];
d=x[9];
s=4
for (i in 3:4){
  S[i]=alpha*(sy[i]-I[i])+(1-alpha)*(S[i-1]+T[i-1])
  T[i]=mu*(S[i]-S[i-1])+(1-mu)*T[i-1]

}
for (i in 5:th) {
  S[i]= alpha*(sy[i]-I[i-s])+(1-alpha)*(S[i-1]+T[i-1])
  T[i]= mu*(S[i]-S[i-1])+(1-mu)*T[i-1]
  I[i]= d*(sy[i]-S[i])+(1-d)*I[i-s]
}
S[th]
T[th]
prev<-vector("numeric",h)
mape=0
for (i in 1:h)  {
  prev[i]=S[th]+i*T[th]+I[th+i-s]
  mape=mape+abs(sy[th+i]-prev[i])*100/sy[th+i]
}
prev
mape/h
  