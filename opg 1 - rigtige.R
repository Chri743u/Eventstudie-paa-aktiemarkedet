#opg. 1 - Sophie

rm(list=1s())#Clear enviroment
cat(''/104'')#Clear console#ctrl+L
rm(afkast,afkast_i,Berk_i,SogP500_m)

#Investerede d. 5. januar 10 mio. USD.
#Hvor meget tjente han, da han solgte d. 30. marts?

Berk=(data.frame(BerkHathLubrizol_text))
i=Berk$EventTime
afkast_Ri = 1

for(i in -46:12)
{
 afkast_Ri_i=subset(Berk,EventTime==i) 
 dagsafkast=afkast_Ri_i$Ri+1
 afkast_Ri=afkast_Ri*dagsafkast
}

print(paste('Afkast i procent for Berk:', format(round((afkast_Ri-1)*100,2),nsmall=2)))
print(paste('Afkast ved investering på 10 mio. USD i Berk:',format(round((afkast_Ri-1)*10000000,2),nsmall=2)))
 
#Afkast v. investering på 10 mio. USD i S&P 500

SogP500=(data.frame(BerkHathLubrizol_text))
m=SogP500$EventTime
afkast_Rm=1

for(i in 1:30)
{
  afkast_Rm_i=subset(Berk,EventTime==i)
  dagsafkast_Rm=afkast_Rm_i$Rm+1
  afkast_Rm=afkast_Rm*dagsafkast_Rm
}  
print(paste('Afkast i procent for SogP500:',format(round((afkast_Rm-1)*100,2),nsmall=2)))
print(paste('Afkast ved investering på 10 mio. USD i SogP500:',format(round((afkast_Rm-1)*10000000,2),nsmall = 2)))
#Hvor meget mere tjener han ved at investerer i Berk fremfor SogP 500:
((2775600.43-442888.27)/2775600.43)*100





#Opgave 2
#Hvor meget tjener han i Berk, hvis han købte d. 15 marts 2011 og solgte d. 26 april 2011?

Berk_s=(data.frame(BerkHathLubrizol_text))
i_s=Berk$EventTime
afkast_Ri_s = 1

for(i in 1:30)
{
  afkast_Ri_s_i=subset(Berk,EventTime==i) 
  dagsafkast=afkast_Ri_s_i$Ri+1
  afkast_Ri_s=afkast_Ri_s*dagsafkast 
}

print(paste('Afkast i procent for Berk:',format(round((afkast_Ri_s-1)*100,2),nsmall = 2)))
print(paste('Afkast ved investering på 10 mio. USD i Berk på et senere tidspunkt:',format(round((afkast_Ri_s-1)*10000000,2),nsmall = 2)))

