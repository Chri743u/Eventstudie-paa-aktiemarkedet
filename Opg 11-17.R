CPRURL <- "https://github.com/Chri743u/Eventstudie-paa-aktiemarkedet/blob/main/CRSPCompustatdata.Rda?raw=true"

df = datacrspcompustat_final

id_CAR_tau=data.frame(cbind(seq(1:897),vCAR_tau,vVAR_car))

colnames(id_CAR_tau) <- c("event_id","car_tau","var_car")
merged_data = merge(x=df,y=id_CAR_tau,by.x="event_id",all.x=TRUE)
n=724
Car_hat_724=merged_data$car_tau
Car_bar_724=sum(Car_hat_724)/n
Var_car_724=merged_data$var_car
std_car_724=sum(Var_car_724)/n^2
round(std_car_724,digits=6)

mean(id_CAR_tau$car_tau)
x=pt(Car_bar_724,n)
x

(VCL95pct=cbind(Car_bar_724-qt(0.975,100-1)*std_car_724,Car_bar_724-qt(0.025,100-1)*std_car_724))

Car_bar_724
?pt
#Opg. 8
N=724
valphahat=matrix(0,N,1)
vbetahat=matrix(0,N,1)
vepsilon__hat_STJ=matrix(0,N,81)
vsigma2hat_STJ=matrix(0,N,1)
for (i in 1:N) {
  FinalData_i=subset(CPRURL,event_id==i)
  VY=FinalData_i$Ri #L1x1 vektor
  VX=FinalData_i$Rm #L1x1 vektor
  Xi=cbind(rep(1, 100), VX[1:100])
  thetahat <- solve(t(Xi) %*% Xi) %*% (t(Xi) %*% VY[1:100])
  epsilon_hat=VY[1:100]-Xi%*%thetahat
  sigma2hat <- (1/(100 - 2)) * (t(epsilon_hat)%*%epsilon_hat)
  valphahat[i,1]=thetahat[1,1]
  vbetahat[i,1]=thetahat[2,1]
  
  Xi_STJ=cbind(rep(1, 81), VX[101:181])
  thetahat_STJ <- solve(t(Xi_STJ) %*% Xi_STJ) %*% (t(Xi_STJ) %*% VY[101:181])
  epsilon_hat_STJ=VY[101:181]-Xi_STJ%*%thetahat_STJ
  sigma2hat_STJ <- (1/(100 - 2)) * (t(epsilon_hat_STJ)%*%epsilon_hat_STJ)
  vepsilon__hat_STJ[i,]=epsilon_hat_STJ
  vsigma2hat_STJ[i,]=sigma2hat_STJ
}
valphahat
vbetahat
mu_a=mean(valphahat)
mu_b=mean(vbetahat)
sigma_a=sqrt((sum((valphahat-mu_a)^2))/N)
sigma_b=sqrt((sum((vbetahat-mu_b)^2))/N)