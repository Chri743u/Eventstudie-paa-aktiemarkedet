rm(list = ls()) # Clear environment
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

mu_a=mean(valphahat)
mu_b=mean(vbetahat)
sigma_a=sqrt((sum((valphahat-mu_a)^2))/N)
sigma_b=sqrt((sum((vbetahat-mu_b)^2))/N)

hist(valphahat, breaks=25)
hist(vbetahat, xlim=c(-1,3), breaks=25)

#opg. 9
# CAR_HAT=vepsilon_hat_STJ
CAR_BAR=matrix(0,N,1)
for (i in 1:N){
  CAR_BAR[i]=sum(vepsilon__hat_STJ[i,1:80])/N
}
plot(CAR_BAR)

#opg. 10
L1=100
sigma2hat_bar_STJ=sum(vsigma2hat_STJ)/N^2
J1=CAR_BAR/sqrt(sigma2hat_bar_STJ)
J1
scar_hat=vepsilon__hat_STJ/sqrt(sigma2hat_bar_STJ)
scar_bar=sum(scar_hat)/N
J2=sqrt((L1-4)/(L1-2))*scar_bar


#opg. 7

gamma_start=rep(0,78)
gamma=append(gamma_start, c(1,1,1),49) #for at lave en vector med 1 fra tau_1 til tau_2
eps=c(epsilon_STJ) #vektorisere espilson_STJ
CAR=t(gamma)*eps #trække epsilon værdierne ud fra tau_1 til tau_2
CAR_BAR=sum(CAR) #summer over epsilon værdierne 
#vi har sigma2hat fra opgave før, vi har at sigma2hat_subset(epsilon_i)=sigma2hat_subset(i)

sig=c(sigma2hat) # vektorisering af sigma2hat
SCAR=CAR/sqrt(sig) #scar_hat udregnes
SCAR_BAR=sum(SCAR) #der summes over scar_hat, som giver scar_bar

J_1=sum(CAR)/sqrt(sum(sigma2hat)) #teststørrelsen J1 er givet
print(J_1)
J_2=sqrt((L1-4)/(L1-2))*SCAR_BAR #teststørrelsen J2 er givet
print(J_2)



