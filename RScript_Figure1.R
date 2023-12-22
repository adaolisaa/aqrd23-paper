#Generalte Figure In R**** Figure 1
#to install plotrix and gtools type "install.packages('plotrix')" and "install.packages('gtools')"

library(plotrix)
library(gtools)

quartz("",8,4)

par(mfrow=c(1,2),family="Times",mar=c(4.75, 4.1, 3, 1))

point=15

# Empowerment
coefs <- c(2.711  ,      1.494     ,   0.908  ,      0.390   ,     0.527     ,  0.0848)
ses <- c( 1.349    ,    1.140      ,  0.450  ,      0.741   ,     0.831     ,   0.562)

plotCI(x=c(0:(length(coefs)-1)),y=coefs,uiw=1.96*ses,xlab="Years Forward",ylab="Effect of Foreign Aid",ylim=c(-6,6),pch=point,xaxp=c(0,length(coefs)-1,(length(coefs)-1)),scol="#999999",main="CIRI Human Empowerment Index")

#plotCI(x=c(0:(length(coefs)-1)),y=coefs,pch=point,uiw=1.64*ses,xlab="Years Forward",add=TRUE)
lines(y=c(0,0),x=c(-100,100))

# Polity Score
coefs <- c(     1.398  ,      1.384  ,      2.323    ,    2.504    ,    2.018     ,   1.326)
ses <- c( 2.333      ,  0.926     ,   0.765 ,       0.484   ,     0.935    ,    1.233)

plotCI(x=c(0:(length(coefs)-1)),y=coefs,uiw=1.96*ses,xlab="Years Forward",ylab="Effect of Foreign Aid",ylim=c(-10,10),pch=point,xaxp=c(0,length(coefs)-1,(length(coefs)-1)),scol="#999999",main="Polity IV Score")

#plotCI(x=c(0:(length(coefs)-1)),y=coefs,pch=point,uiw=1.64*ses,xlab="Years Forward",add=TRUE)
lines(y=c(0,0),x=c(-100,100))
