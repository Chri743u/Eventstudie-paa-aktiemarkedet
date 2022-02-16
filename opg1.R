rm(list = ls()) # Clear environment
cat("\014")  # Clear console # ctrl+L 

#opgave 1
#Hvor meget er der tjent ved køb d. 5. januar og salg d. 30 marts (-46:12)
#(\Pi_{n=-46}^{12} (Ri_n+1)-1)*10.000.000

Berk=(data.frame(BerkHathLubrizol))
i=Berk$EventTime
afkast=1

for(i in -46:12)
{
  afkast_i=subset(Berk,EventTime==i)
  Dagsafkast=afkast_i$Ri+1
  afkast=afkast*Dagsafkast
}

print(paste("Afkast i procent: ",format(round((afkast-1)*100, 2), nsmall = 2)))
print(paste("Afkast ved investering á 10.000.000: ",format(round((afkast-1)*10000000, 2), nsmall = 2)))
