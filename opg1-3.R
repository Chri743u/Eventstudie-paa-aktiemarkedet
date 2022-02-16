#Den korte udgave af opg.1:

VRi=BerkHathLubrizol$Ri #180x1 vektor
VRm=BerkHathLubrizol$Rm #180x1 vektor
ValueL=10*(10^6)*prod((1+VRi[105:163])) #værdi af Lubriuzol
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
stderror=sqrt(sigma2_zetahat)/sqrt(L1)
teststat=abs(muhat-0)/stderror #minus 0 fordi vi har sat mu=0 i nulhypotesen
pval=2*(1-pt(teststat,df=L1-1)) #2x fordi det er både store og små værdier der er kritiske (den er dobbeltsidet).
#p-værdi måler sandsynlighed for at vi oplever der er noget som en endnu mere usandsynlighed???
print(pval) #pval=0.44, da p>0.05 fail to reject (Ronald A. Fisher udviklede P-værdi) (p-værdi er modelkontrollen)

VCL95pct=cbind(muhat-qt(0.975,L1-1)*stderror,muhat-qt(0.025,L1-1)*stderror)
print(VCL95pct) #alle værdier indenfor intervallet afvises ikke
