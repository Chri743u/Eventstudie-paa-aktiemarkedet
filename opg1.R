#rm(list = ls()) # Clear environment
#cat("\014")  # Clear console # ctrl+L 

BerkHathLubrizol = data.frame(BerkHathLubrizol_text)

#Den korte udgave af opg.1:

VRi=BerkHathLubrizol$Ri #180x1 vektor
VRm=BerkHathLubrizol$Rm #180x1 vektor
ValueL=10*(10^6)*prod((1+VRi[105:163])) #værdi af Lubrizol
print(ValueL)
ValueSP=10*(10^6)*prod((1+VRm[105:163])) #Værdi af S&P
print(ValueSP)
Diff=ValueL-ValueSP #værdiforskel på de to investeringer
print(Diff)

#Den korte udgave af opg.2:

VRi1=BerkHathLubrizol$Ri #180x1 vektor
VRm1=BerkHathLubrizol$Rm #180x1 vektor
ValueL1=10*(10^6)*prod((1+VRi1[152:181])) #værdi af Lubriuzol
print(ValueL1)
ValueSP1=10*(10^6)*prod((1+VRm1[152:181])) #Værdi af S&P
print(ValueSP1)
Diff1=ValueL1-ValueSP1 #værdiforskel på de to investeringer
print(Diff1)

#Opg.3

#spg. 3 estimationsperioden fra t=-151 til t=-51, estimer afkast i den periode og lav en hypotesetest
VY=VRi[1:100] #L1x1 vektor
L1=100
muhat=mean(VY)
sigma2_zetahat=sum((VY-muhat)^2)/(L1-1) #skal bruge mindst 3 observationer for at beregne varians

#hypotesetest: H_0 : mu=0
#t-test
stderror=sqrt(sigma2_zetahat)
teststat=abs(muhat-0)/stderror #minus 0 fordi vi har sat mu=0 i nulhypotesen
pval=2*(1-pt(teststat,df=L1-1)) #2x fordi det er både store og små værdier der er kritiske (den er dobbeltsidet).
#p-værdi måler sandsynlighed for at vi oplever der er noget som en endnu mere usandsynlighed???
print(pval) #pval=0.44, da p>0.05 fail to reject (Ronald A. Fisher udviklede P-værdi) (p-værdi er modelkontrollen)

VCL95pct=cbind(muhat-qt(0.975,L1-1)*stderror,muhat-qt(0.025,L1-1)*stderror)
print(VCL95pct) #alle værdier indenfor intervallet afvises ikke


#modelkontrol

qqnorm(VY)
qqline(VY)

#Opg. 4
1-pnorm(0.27, muhat,stderror)
#Infinitesimal lille p-værdi, så vi afventer respons fra David 
#om vi skal finde den 10^-x lille værdi?

#Opg. 5
VY=VRi[1:100] #L1x1 vektor
VX=VRm[1:100] #L1x1 vektor
linmod = lm(VY ~ VX, data = BerkHathLubrizol)

Xi <- cbind(rep(1, 100), VX) #L2X2 vector med 1 taller på første række
thetahat <- solve(t(Xi) %*% Xi) %*% (t(Xi) %*% VY) #er det ikke theta_hat? indeholder alpha på index [1] og beta på index [2]
epsilon_hat=VY-Xi%*%thetahat #epsilon hat i estimations perioden
sigma2hat <- (1/(100 - 2)) * (t(epsilon_hat)%*%epsilon_hat) #sigmahat^2 i estimationsperioden

qqnorm(VX)#modelkontrol
qqline(VX)

alpha = thetahat[1]
beta = thetahat[2]

confint(linmod) #95% confidence interval for linmod funktionen

#Opg. 6
(VY_STJ = VRi[101:181])#afkast for lubrizol i event-perioden
(VX_STJ = VRm[101:181])#afkast for markedet i event-perioden

Xi_STJ <- cbind(rep(1, 81), VX_STJ) #L2X2 vector med 1 taller på første række
Xi_STJ
epsilon_STJ = VY_STJ - Xi_STJ%*%thetahat
print(epsilon_STJ)
epsilon_S = VY_STJ - (alpha+beta*VX_STJ)
print(epsilon_S) 
#to af de samme måder at lave epsilon_stjerne på i event-perioden, den ene er givet
#i opgaven og den anden er mackinley formlen som der også bliver brugt i opgaven før

qqnorm(epsilon_STJ) #modelkontrol
qqline(epsilon_STJ)

Event_vindue = seq(-50,30)
cepsilon = cumsum(epsilon_STJ)
plot(Event_vindue,cepsilon) #plot af den comulative sum af epsilon af eventperioden

#opg. 7

gamma_start=rep(0,78)
gamma=append(gamma_start, c(1,1,1),49) #for at lave en vector med 1 fra tau_1 til tau_2
eps=c(cepsilon) #vektorisere espilson_STJ
CAR=t(gamma)*eps #trække epsilon værdierne ud fra tau_1 til tau_2
CAR_BAR=sum(CAR) #summer over epsilon værdierne 
#vi har sigma2hat fra opgave før, vi har at sigma2hat_subset(epsilon_i)=sigma2hat_subset(i)
plot(eps)
sig=c(sigma2hat) # vektorisering af sigma2hat
SCAR=CAR/sqrt(sig) #scar_hat udregnes
SCAR_BAR=sum(SCAR) #der summes over scar_hat, som giver scar_bar

(J_1=sum(CAR)/sqrt(sum(sigma2hat))) #teststørrelsen J1 er givet
(J_2=sqrt((L1-4)/(L1-2))*SCAR_BAR) #teststørrelsen J2 er givet
(pval_J_1=2*(1-pt(J_1,df=L1-1)))
(pval_J_2=2*(1-pt(J_2,df=L1-1)))










