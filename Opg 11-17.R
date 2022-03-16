CPRURL <- "https://github.com/Chri743u/Eventstudie-paa-aktiemarkedet/blob/main/CRSPCompustatdata.Rda?raw=true"
load(url(CRPURL))

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

Yi_724=Car_hat_724
Xi_724=cbind(rep(1,724),df$Size,df$Market2Book,df$Debt2Assets,df$ROE)
Xi_724

(theta <- solve(t(Xi_724)%*%Xi_724,tol=1e-22)%*%t(Xi_724)%*%Yi_724)
#christian kigger på resten



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


