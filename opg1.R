#Lars

rm(list = ls()) # Clear environment
cat("\014")  # Clear console # ctrl+L 


#opgave 1
#Hvor meget er der tjent ved køb d. 5. januar og salg d. 30. marts (-46:12)

#(\Pi_{n=-46}^{12} (Ri_n+1)-1)*10,000,000
#(\pi_{n=-46}^{12} (Ri_n+1)-1)*10,000,000

Berk=(data.frame(BerkHathLubrizol))
i=Berk$EventTime

Berk=(data.frame(BerkHathLubrizol))
i=Berk$EventTime
afkast_Ri=1
afkast_Rm=1

for(i in -46:12)
{
  afkast_i=subset(Berk,EventTime==i)
  Dagsafkast_Ri=afkast_i$Ri+1
  afkast_Ri=afkast_Ri*Dagsafkast_Ri
}

print(paste("Afkast i procent: ",format(round((afkast_Ri-1)*100, 2), nsmall = 2)))
print(paste("Afkast ved investering á 10.000.000: ",format(round((afkast_Ri-1)*10000000, 2), nsmall = 2)))
print(afkast_Ri-1)

for(i in -46:12)
{
  afkast_i=subset(Berk,EventTime==i)
  Dagsafkast_Rm=afkast_i$Rm+1
  afkast_Rm=afkast_Rm*Dagsafkast_Rm
}

print(paste("Afkast i procent i S&P500: ",format(round((afkast_Rm-1)*100, 2), nsmall = 2,big.mark = ",")))
print(paste("Afkast ved investering á 10,000,000 i S&P500: ",format(round((afkast_Rm-1)*10000000, 2), nsmall = 2,big.mark = ",")))
print(afkast_Rm-1)

print(paste("Forskel på de to: ",format(round((afkast_Ri-afkast_Rm)*10000000,2),nsmall=2,big.mark = ",")))

# Opgave 2
# Hvor meget er der tjent ved køb d. 15. marts før åbning og salg d. 26. april efter lukketid (1:30)


for(i in 1:30)
{
  afkast_i=subset(Berk,EventTime==i)
  Dagsafkast=afkast_i$Rm+1
  afkast_Rm=afkast_Rm*Dagsafkast
}
print(paste("Afkast i procent: ",format(round((afkast_Rm-1)*100, 2), nsmall = 2,big.mark = ",")))
print(paste("Afkast ved investering á 10,000,000: ",format(round((afkast_Rm-1)*10000000, 2), nsmall = 2,big.mark = ",")))

