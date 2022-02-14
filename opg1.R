rm(list = ls()) # Clear environment
cat("\014")  # Clear console # ctrl+L 

#opgave 1
#Hvor meget er der tjent ved køb d. 5. januar og salg d. 30 marts (-46:12)
#(\pi_{n=-46}^{12} (Ri_n+1)-1)*10.000.000

Berk=(data.frame(BerkHathLubrizol))
i=Berk$EventTime


for(i in -46:12)
{
  afkast_i=subset(Berk,Ri==i)
  
  
}
