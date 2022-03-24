rm(list = ls()) # Clear environment
cat("\014")  # Clear console # ctrl+L 

#importer datasæt fra github
BHLURL <- "https://github.com/Chri743u/Eventstudie-paa-aktiemarkedet/blob/main/BerkHathLubrizol.Rda?raw=true"
FullURL <- "https://github.com/Chri743u/Eventstudie-paa-aktiemarkedet/blob/main/EventdataFull.Rda?raw=true"
CRPURL <- "https://github.com/Chri743u/Eventstudie-paa-aktiemarkedet/blob/main/CRSPCompustatdata.Rda?raw=true"
load(url(BHLURL))
load(url(FullURL))
load(url(CRPURL))

#Den korte udgave af opg.1:

VRi=BerkHathLubrizol$Ri #180x1 vektor
VRm=BerkHathLubrizol$Rm #180x1 vektor
(ValueL=10*(10^6)*prod((1+VRi[105:163]))) #værdi af Lubrizol
(ValueSP=10*(10^6)*prod((1+VRm[105:163]))) #Værdi af S&P
(Diff=ValueL-ValueSP) #værdiforskel på de to investeringer

#Den korte udgave af opg.2:

VRi1=BerkHathLubrizol$Ri #180x1 vektor
VRm1=BerkHathLubrizol$Rm #180x1 vektor
(ValueL1=10*(10^6)*prod((1+VRi1[152:181]))) #værdi af Lubriuzol
(ValueSP1=10*(10^6)*prod((1+VRm1[152:181]))) #Værdi af S&P
(Diff1=ValueL1-ValueSP1) #værdiforskel på de to investeringer

#Opg.3

#spg. 3 estimationsperioden fra t=-151 til t=-51, estimer afkast i den periode og lav en hypotesetest
VYi=VRi[1:100] #L1x1 vektor
L1=100
(muhat=mean(VYi))
(sigma2_zetahat=sum((VYi-muhat)^2)/(L1-1)) #skal bruge mindst 3 observationer for at beregne varians
VYi-muhat
#hypotesetest: H_0 : mu=0
#t-test
stderror=sqrt(sigma2_zetahat)
teststat=abs(muhat-0)/stderror #minus 0 fordi vi har sat mu=0 i nulhypotesen
(pval=2*(1-pt(teststat,df=L1-1))) #2x fordi det er både store og små værdier der er kritiske (den er dobbeltsidet).
#p-værdi måler sandsynlighed for at vi oplever der er noget som en endnu mere usandsynligt
#pval=0.938, da p>0.05 fail to reject (Ronald A. Fisher udviklede P-værdi) (p-værdi er modelkontrollen)

(VCL95pct=cbind(muhat-qt(0.975,L1-1)*stderror,muhat-qt(0.025,L1-1)*stderror))
#alle værdier indenfor intervallet afvises ikke


#modelkontrol
qqnorm(VYi)
qqline(VYi)

#Opg. 4
(1-pnorm(0.27, muhat,stderror))
#chancen er infinitesimal lille, da chancen for at en enkelt aktie stiger med 27% eller
#over på en dag over en normalfordeling aldrig ville ske, sammenligneligt med at blive 
#ramt af 5000 lyn på en dag (eller lign.)

#Opg. 5
VYi=VRi[1:100] #L1x1 vektor
VXm=VRm[1:100] #L1x1 vektor
(linmod = lm(VYi ~ VXm, data = BerkHathLubrizol)) #for at sammenligne vores svar med

Xim <- cbind(rep(1, 100), VXm) #L2X2 vector med 1 taller på første række
thetahat <- solve(t(Xim) %*% Xim) %*% (t(Xim) %*% VYi) #er det ikke theta_hat? indeholder alpha på index [1] og beta på index [2]
epsilon_hat=VYi-Xim%*%thetahat #epsilon hat i estimations perioden
sigma2hat <- (1/(100 - 2)) * (t(epsilon_hat)%*%epsilon_hat) #sigmahat^2 i estimationsperioden

#modelkontrol
qqnorm(VXm)
qqline(VXm)

alpha = thetahat[1] #alpha-værdien der findes på førstekoordinatet på theta, tjekket på lm fct
beta = thetahat[2] #beta-værdien der findes på andetkoordinatet på theta, tjekket på lm fct

confint(linmod) #95% confidence interval for linmod funktionen

#Opg. 6
VYi_STJ = VRi[101:181] #afkast for lubrizol i event-perioden
VXm_STJ = VRm[101:181] #afkast for markedet i event-perioden
Xim_STJ <- cbind(rep(1, 81), VXm_STJ) #L2X2 vector med 1 taller på første række
epsilon_STJ = VYi_STJ - Xim_STJ%*%thetahat
epsilon_S = VYi_STJ - (alpha+beta*VXm_STJ)
#to af de samme måder at lave epsilon_stjerne på i event-perioden, den ene er givet
#i opgaven og den anden er mackinley formlen som der også bliver brugt i opgaven før

#modelkontrol
qqnorm(epsilon_S)
qqline(epsilon_S)

Event_vindue = seq(-50,30) # en sekvens der bliver brugt til at bestemme x-aksen i vores plot
cepsilon = cumsum(epsilon_S) #funktion der tager den komulative sum af epsilon_STJ
plot(Event_vindue,cepsilon) #plot af den comulative sum af epsilon af eventperioden

#opg. 7
gamma_start=rep(0,78)
gamma=append(gamma_start, c(1,1,1),49) #for at lave en vector med 1 fra tau_1 til tau_2

V_i=diag(81)*c(sigma2hat)+c(Xim_STJ%*%solve(t(Xim)%*%Xim)%*%t(Xim_STJ))*c(sigma2hat)
Var_carbar=t(gamma)%*%V_i%*%gamma

CAR7=sum(t(gamma)*epsilon_S) #trække epsilon værdierne ud fra tau_1 til tau_2
CAR_BAR7=CAR7 #summer over epsilon værdierne 
#vi har sigma2hat fra opgave før, vi har at sigma2hat_subset(epsilon_i)=sigma2hat_subset(i)

SCAR7=CAR7/sqrt(Var_carbar) #scar_hat udregnes
SCAR_BAR7=SCAR7 #der summes over scar_hat, som giver scar_bar

(J_1=CAR7/sqrt(Var_carbar)) #teststørrelsen J_1 er givet
(J_2=sqrt((L1-4)/(L1-2))*SCAR_BAR7) #teststørrelsen J_2 er givet
(pval_J_1=2*(1-pt(J_1,df=L1-1))) #p-værdien givet J_1
(pval_J_2=2*(1-pt(J_2,df=L1-1))) #p-værdien givet J_2

#Opg. 8
N=897
valphahat=matrix(0,N,1) #en tom matrice der bruges til at opbevare alpha'er der fås i for loopet
vbetahat=matrix(0,N,1) #en tom matrice der bruges til at opbevare beta'er der fås i for loopet
vepsilon__hat_STJ=matrix(0,N,81) #en tom matrice der bruges til at opbevare epsilon'er der fås i for loopet
vsigma2hat_STJ=matrix(0,N,1) #en tom matrice der bruges til at opbevare sigma^2'er der fås i for loopet
vVAR_car=matrix(0,N,1)
for (i in 1:N) {
  FinalData_i=subset(FinalData,event_id==i) #får loopet til at fokusere på en eventperiode af gangen
  VY=FinalData_i$Ri #L1x1 vektor
  VX=FinalData_i$Rm #L1x1 vektor
  Xi=cbind(rep(1, 100), VX[1:100]) #L1x2 vektor med 1 taller på første række
  thetahat <- solve(t(Xi) %*% Xi) %*% (t(Xi) %*% VY[1:100]) #theta med alpha på [1] og beta på [2]
  epsilon_hat=VY[1:100]-Xi%*%thetahat #de abnormale afkast af vores forskellige events i estimationsperioden
  sigma2hat <- (1/(100 - 2)) * (t(epsilon_hat)%*%epsilon_hat) #udregning af sigma2hat
  valphahat[i,1]=thetahat[1,1] #der lagres alpha'er ind i matrice "valphahat"
  vbetahat[i,1]=thetahat[2,1] #der lagres beta'er ind i matrice "vbetahat"
  
  #den næste del af for loopet bliver benyttet i en længere hen opgave (9)
  Xi_STJ=cbind(rep(1, 81), VX[101:181]) #L2x2 vektor med 1 taller på første række
  epsilon_hat_STJ=VY[101:181]-Xi_STJ%*%thetahat #de abnormale afkast af vores forskellige events i eventperioden
  sigma2hat_STJ <- (1/(100 - 2)) * (t(epsilon_hat_STJ)%*%epsilon_hat_STJ) #udregning af sigma2hat
  vepsilon__hat_STJ[i,]=epsilon_hat_STJ #der lagres epsilon'er in i matricen "vepsilon_hat_STJ"
  vsigma2hat_STJ[i,]=sigma2hat_STJ #der lagres sigma2hat'er in i matricen "vsigma2hat_STJ"
  V_i=diag(81)*c(sigma2hat)+Xi_STJ%*%solve(t(Xi)%*%Xi)%*%t(Xi_STJ)*c(sigma2hat)
  vVAR_car[i,]=t(gamma)%*%V_i%*%gamma
  
}



(mu_a=mean(valphahat)) #mean af datasættet alpha_hat
(mu_b=mean(vbetahat)) #mean af datasættet beta_hat
(sigma_a=sqrt((sum((valphahat-mu_a)^2))/N)) #sigma2 af datasættet alpha_hat
(sigma_b=sqrt((sum((vbetahat-mu_b)^2))/N)) ##mean af datasættet beta_hat

hist(valphahat, breaks=25) #histogram af datasættet alpha_hat
hist(vbetahat, xlim=c(-1,3), breaks=25) #histogram af datasættet beta_hat

#opg. 9
vCAR_mat=matrix(0,N,81) #tom matrice der benyttes til at lægge de cumulative summer af epsilon i
for (i in 1:N){
  CAR_mat=cumsum(vepsilon__hat_STJ[i,])
  vCAR_mat[i,]=CAR_mat
} #for loop der cumulativt summer vores epsilon vektorer og ligger den i matricen "vCAR_mat
vCAR_bar=matrix(0,1,81) #tom matrice der benyttes til at lægge gennemsnittet af CAR_hat ind
for (i in 1:81){
  vCAR_bar[,i]=mean(vCAR_mat[,i])
} #for loop der tage gennemsnittet af kolonnerne og ligger dem in i den tomme matrice "v_CAR_bar"
x_seq=seq(-50,30) #sekvens der benyttes til at plotte x-aksen
plot(x_seq, vCAR_bar) #plot af CAR_bar(tau(1), tau(2))


#opg. 10
L1=100 #længde af estimationsvindue
Varians=vVAR_car

vCAR_tau=matrix(0,N,1) #tom matrice der cumulativt summer over epsilon værdierne fra tau(1)-tau(2)
#(koden her er suboptimal for cumsum af et bestemt interval af indexer i en vektor)
for (i in 1:N){
  vCAR_tau[i,]=sum(vepsilon__hat_STJ[i,][50:52])
}
vCAR_bar_tau=sum(vCAR_tau)/N
(J1=vCAR_bar_tau/sqrt(sum(Varians)/N^2)) #teststørrelse, formel i kinley pdf
(pval_J1=2*(1-pt(J1,df=L1-1))) #p-værdi med teststørrelse J1
sigma2hat

scar_hat=vCAR_tau/sqrt(Varians) #scar formel i kinley pdf (1:897 vektor)
scar_bar=sum(scar_hat)/N #scar bar formel i kinley pdf, som skalar
(J2=sqrt(N*(L1-4)/(L1-2))*scar_bar) #teststørrelse J2 formel i kinley pdf, som skalar
(pval_J2=2*(1-pt(J2,df=L1-1))) #p-værdi med teststørrelse J2

#Del 2

df = data.frame(datacrspcompustat_final)

#opgave 11
id_CAR_tau=data.frame(cbind(seq(1:897),vCAR_tau,vVAR_car))

colnames(id_CAR_tau) <- c("event_id","car_tau","var_car")
merged_data = merge(x=df,y=id_CAR_tau,by.x="event_id",all.x=TRUE)
n=724
Car_hat_724=merged_data$car_tau
(Car_bar_724=sum(Car_hat_724)/n)
Var_car_724=merged_data$var_car
(std_car_724=sqrt(sum(Var_car_724)/n^2))
round(std_car_724,digits=6)

mean(id_CAR_tau$car_tau)
x=pt(Car_bar_724,n)

qqplot(Car_hat_724, vCAR_tau)

(mean_diff=abs(mean(Car_hat_724)-mean(vCAR_tau)))

#opgave 12
df_2 = subset(merged_data, select=c("car_tau","event_id", "Size", "Market2Book", "Debt2Assets", "ROE"))

Yi_724=Car_hat_724
Xi_724=cbind(rep(1,724),merged_data$Size,merged_data$Market2Book,merged_data$Debt2Assets,merged_data$ROE)
solve(t(Xi_724)%*%Xi_724,tol=1e-23)
(betahat <- solve(t(Xi_724)%*%Xi_724,tol=1e-23)%*%t(Xi_724)%*%Yi_724)
(sigmahat_2 <- 1/(n - 5 - 1) * sum((Yi_724-Xi_724%*%betahat)^2))

alpha <- 0.05
xTx.inv <- (solve(t(Xi_724) %*% Xi_724,tol=1e-23))

for (i in 1:5){
  confint = betahat[i] + c(-1, 1) * sqrt(sigmahat_2) * sqrt(xTx.inv[i, i]) *
     qt(p = 1 - alpha/2, df = n - 3)
  print(confint)
}

(linmod = lm(merged_data$car_tau ~ merged_data$Size + merged_data$Market2Book + merged_data$Debt2Assets + merged_data$ROE, data = merged_data)) 
confint(linmod)

#opgave 13

new<-df$Market2Book
new[new>0]<-0
new[new<0]<-1
Xi_724_negbv=cbind(rep(1,724),df$Size,df$Market2Book,df$Debt2Assets,df$ROE, new)

(theta <- solve(t(Xi_724_negbv)%*%Xi_724_negbv,tol=1e-22)%*%t(Xi_724_negbv)%*%Yi_724)
#(fortolkning) dette fortæller os at hvis et selvskab har en negativ market2book value vil deres car_hat være 0.05 højere, svarende til 5%

#opgave 14
fin_ins=ifelse(as.numeric(substr(df$NAICS,1,2))==52,1,0)
fin_ins
Xi_724_finins=cbind(rep(1,724),df$Size,df$Market2Book,df$Debt2Assets,df$ROE, fin_ins)
(theta <- solve(t(Xi_724_finins)%*%Xi_724_finins,tol=1e-22)%*%t(Xi_724_finins)%*%Yi_724)

#Opgave 15
hist(df$Size)
lnSize = log(df$Size)
hist(lnSize)

#Opgave 16
Xi_724_size = cbind(rep(1,724),lnSize)
(theta <- solve(t(Xi_724_size)%*%Xi_724_size,tol=1e-22)%*%t(Xi_724_size)%*%Yi_724)
(linmod_2 = lm(df))
size_alpha = theta[1]
size_beta = theta[2]
size_epsilon = (Yi_724-(size_alpha+size_beta*Xi_724_size))

plot(lnSize,size_epsilon[,2])
plot(lnSize,Yi_724)
epsDiag=diag((size_epsilon[,2])^2)

whiteEstimator1 = (solve(t(Xi_724_size)%*%Xi_724_size)%*%t(Xi_724_size)%*%epsDiag%*%Xi_724_size%*%solve(t(Xi_724_size)%*%Xi_724_size)) 
whiteEstimator1

#Hypotesetest
size_muhat=mean(Yi_724)
size_sigma2=sum((size_epsilon[,2])^2)/n
size_stderror=sqrt(size_sigma2)
size_teststat=abs(size_beta-0)/size_stderror #minus 0 fordi vi har sat mu=0 i nulhypotesen
(size_pval=2*(1-pt(size_teststat,df=L1-1)))
size_beta/(sqrt(size_sigma2/n))

#white Hypotesetest
white_size_epsilon =(Yi_724-(whiteEstimator1[1,1]+whiteEstimator1[2,2]*Xi_724_size))
white_size_sigma2=sum((white_size_epsilon[,2]^2))/n
white_size_stderror=sqrt(white_size_sigma2)
white_size_teststat=abs(whiteEstimator1[2,2]-0)/white_size_stderror #minus 0 fordi vi har sat mu=0 i nulhypotesen
(white_size_pval=2*(1-pt(white_size_teststat,df=L1-1)))
plot(lnSize,white_size_epsilon[,2])
plot(lnSize,Yi_724, pch=1, col="blue", xlab = "", ylab = "")
points(lnSize, white_size_epsilon[,2], pch=4, col="orange")
points(lnSize, size_epsilon[,2], pch=20, col="magenta")


#Opgave 18

ordered_size <- merged_data[with(merged_data,order(-Size)),]
top10 = head(ordered_size,n=72)
bot10 = tail(ordered_size,n=72)
#sammenligner
mean(top10$car_tau)
mean(bot10$car_tau)

L3_car = vCAR_mat[,45:49]
event_id = c(rep(1:897))
L3_car_id = cbind(event_id, L3_car)

top10_id = cbind(rep(1),top10$event_id)
bot10_id = cbind(rep(1),bot10$event_id)
colnames(top10_id) <- c("mean","event_id")
colnames(bot10_id) <- c("mean","event_id")

merged_top10 = merge(x=top10_id, y=L3_car_id, by=c("event_id"))
merged_bot10 = merge(x=bot10_id, y=L3_car_id, by=c("event_id"))

merged_top10$mean <- ((rowSums(merged_top10[,3:7]))/5)
merged_bot10$mean <- ((rowSums(merged_bot10[,3:7]))/5)

top10_l3_mean = mean(merged_top10$mean)
top10_l3_mean = mean(merged_bot10$mean)
