CPRURL <- "https://github.com/Chri743u/Eventstudie-paa-aktiemarkedet/blob/main/CRSPCompustatdata.Rda?raw=true"

#opg 11

df = datacrspcompustat_final

id_CAR_tau=data.frame(cbind(seq(1:897),vCAR_tau,vVAR_car))

colnames(id_CAR_tau) <- c("event_id","car_tau","var_car")
merged_data = merge(x=df,y=id_CAR_tau,by.x="event_id",all.x=TRUE)
n=724
Car_hat_724=merged_data$car_tau
Car_bar_724=sum(Car_hat_724)/n
Var_car_724=merged_data$var_car
std_car_724=sum(Var_car_724)/n^2

mean(id_CAR_tau$car_tau)
x=pt(Car_bar_724,n)


(VCL95pct=cbind(Car_bar_724-qt(0.975,100-1)*std_car_724,Car_bar_724-qt(0.025,100-1)*std_car_724))


#opg 1.12
df_car_hat = id_CAR_tau$car_tau
df_size=df$Size
df_Mark2book=df$Market2Book
df_debt2asse=df$Debt2Assets
df_ROE=df$ROE

Xmarket=cbind(rep(1,n),df_size,df_Mark2book,df_debt2asse,df_ROE)
thetahat <- solve(t(Xmarket) %*% Xmarket) %*% (t(Xmarket) %*% df_car_hat)

t(Xmarket)%*%Xmarket


VYi=VRi[1:100] #L1x1 vektor
VXm=VRm[1:100] #L1x1 vektor
linmod = lm(VYi ~ VXm, data = BerkHathLubrizol) #for at sammenligne vores svar med

Xim <- cbind(rep(1,100),1,) #L2X2 vector med 1 taller på første række
thetahat <- solve(t(Xim) %*% Xim) %*% (t(Xim) %*% VYi) #er det ikke theta_hat? indeholder alpha på index [1] og beta på index [2]
epsilon_hat=VYi-Xim%*%thetahat #epsilon hat i estimations perioden
sigma2hat <- (1/(100 - 2)) * (t(epsilon_hat)%*%epsilon_hat) #sigmahat^2 i estimationsperioden

#modelkontrol
qqnorm(VXm)
qqline(VXm)

alpha = thetahat[1] #alpha-værdien der findes på førstekoordinatet på theta, tjekket på lm fct
beta = thetahat[2] #beta-værdien der findes på andetkoordinatet på theta, tjekket på lm fct















