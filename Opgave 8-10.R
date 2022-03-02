#rm(list = ls()) # Clear environment
cat("\014")  # Clear console # ctrl+L

load(FinalData.rda)

#Opg. 8
N=897
valphahat=matrix(0,N,1)
vbetahat=matrix(0,N,1)
vepsilon__hat_STJ=matrix(0,N,81)
vsigma2hat_STJ=matrix(0,N,1)
for (i in 1:N) {
  FinalData_i=subset(FinalData,event_id==i)
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

hist(valphahat, breaks=25)
hist(vbetahat, xlim=c(-1,3), breaks=25)

#opg. 9
# CAR_HAT=vepsilon_hat_STJ
CAR_BAR=matrix(0,N,1)
vCAR_mat=matrix(0,N,81)
for (i in 1:N){
  CAR_mat=cumsum(vepsilon__hat_STJ[i,])
  vCAR_mat[i,]=CAR_mat
} 
vCAR_bar=matrix(0,1,81)
for (i in 1:81){
  vCAR_bar[,i]=mean(vCAR_mat[,i])
}
x_seq=seq(-50,30)
plot(x_seq, vCAR_bar)


#opg. 10
L1=100 
sigma2hat_bar_STJ=sum(vsigma2hat_STJ)/N^2

vCAR_tau=matrix(0,N,1)
for (i in 1:N){
  vCAR_tau[i,]=vepsilon__hat_STJ[i,50]+vepsilon__hat_STJ[i,51]+vepsilon__hat_STJ[i,52]
}
vCAR_tau
CAR_bar_tau=mean(vCAR_tau)
CAR_bar_tau

(J1=sum(CAR_bar_tau)/sqrt(sigma2hat_bar_STJ))
(pval_J1=2*(1-pt(J1,df=L1-1)))

scar_hat=vCAR_tau/sqrt(sigma2hat_bar_STJ)
scar_bar=sum(scar_hat)/N
(J2=sqrt((L1-4)/(L1-2))*scar_bar)
(pval_J2=2*(1-pt(J2,df=L1-1)))

