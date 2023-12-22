# In R, Run:
# to install foreign type "install.packages('foreign')" 

library(foreign)

FA <- read.dta("figuredata.dta")
FA <- FA[FA$year > 1986 & FA$year < 2007,]

plot(EV~year,data=FA)

resids <- lm(FA$f1EV~factor(FA$ccode)+factor(FA$year))$residuals

par(mfrow=c(3,1))

i = c(2,4:8)[4]

plot(ave(resids[FA$uniquecol3==i],FA$year[FA$uniquecol3==i])~FA$year[FA$uniquecol3==i],main=paste("Britain, N =",sum(FA$uniquecol3==i)),xlab="Year",ylab="Avg. Residualized Aid",ylim = c(-.1,.1))
abline(v=unique(FA$year[FA$uniquecol3==i & FA$CPcol2 == 1]),col="gray")
abline(v=unique(FA$year[FA$uniquecol3==i & FA$CPcol2 == 1])+1,col="black")

i = c(2,4:8)[3]

plot(ave(resids[FA$uniquecol3==i],FA$year[FA$uniquecol3==i])~FA$year[FA$uniquecol3==i],main=paste("France, N =",sum(FA$uniquecol3==i)),xlab="Year",ylab="Avg. Residualized Aid",ylim=c(-0.35,0.35))
abline(v=unique(FA$year[FA$uniquecol3==i & FA$CPcol2 == 1]),col="gray")
abline(v=unique(FA$year[FA$uniquecol3==i & FA$CPcol2 == 1])+1,col="black")

i = c(2,4:8)[1]

plot(ave(resids[FA$uniquecol3==i],FA$year[FA$uniquecol3==i])~FA$year[FA$uniquecol3==i],main=paste("Spain, N =",sum(FA$uniquecol3==i)),xlab="Year",ylab="Avg. Residualized Aid",ylim=c(-0.35,0.35))
abline(v=unique(FA$year[FA$uniquecol3==i & FA$CPcol2 == 1]),col="gray")
abline(v=unique(FA$year[FA$uniquecol3==i & FA$CPcol2 == 1])+1,col="black")

# # for(i in c(2,4:8)[c(4,3,1)]){
# #) {

# plot(ave(resids[FA$uniquecol3==i],FA$year[FA$uniquecol3==i])~FA$year[FA$uniquecol3==i],main=paste("N=",sum(FA$uniquecol3==i)),xlab="Year",ylab="Residualized Aid")#,ylim=c(-.4,.4))
# #plot(ave(FA$f1EV[FA$uniquecol3==i],FA$year[FA$uniquecol3==i])~FA$year[FA$uniquecol3==i],main=paste("N=",sum(FA$uniquecol3==i)))
# abline(v=unique(FA$year[FA$uniquecol3==i & FA$CPcol2 == 1]),col="gray")
# abline(v=unique(FA$year[FA$uniquecol3==i & FA$CPcol2 == 1])+1,col="black")

# }

#?ave


#table(FA$year[FA$uniquecol3==i],FA$CPcol1[FA$uniquecol3==i])

