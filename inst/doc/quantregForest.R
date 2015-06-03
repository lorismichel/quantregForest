### R code from vignette source 'quantregForest.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: R options
###################################################
options(width = 60)
options(SweaveHooks = list(fig = function() par(mar=c(3,3,1,0.5),mgp = c(2,1,0))))


###################################################
### code chunk number 2: quantregForest.Rnw:27-28
###################################################
library(quantregForest)


###################################################
### code chunk number 3: quantregForest.Rnw:36-37
###################################################
help(quantregForest)


###################################################
### code chunk number 4: quantregForest.Rnw:42-45
###################################################
data(ozone,package="gss")
xozone <- ozone[-1]
yozone <- ozone$upo3


###################################################
### code chunk number 5: quantregForest.Rnw:48-49
###################################################
qrfozone <- quantregForest(xozone,yozone)


###################################################
### code chunk number 6: quantregForest.Rnw:53-54
###################################################
print(qrfozone) # or simply just type qrfozone


###################################################
### code chunk number 7: quantregForest.Rnw:57-58
###################################################
qrfozone$origNodes


###################################################
### code chunk number 8: quantregForest.Rnw:63-64
###################################################
qrfozone$origObs


###################################################
### code chunk number 9: quantregForest.Rnw:73-74
###################################################
object<-qrfozone


###################################################
### code chunk number 10: quantregForest.Rnw:76-79
###################################################
predict(object, newdata=NULL,
		quantiles=c(0.1,0.5,0.9),
		all=FALSE, obs=1)


###################################################
### code chunk number 11: quantregForest.Rnw:83-84
###################################################
predict(qrfozone)


###################################################
### code chunk number 12: quantregForest.Rnw:88-89
###################################################
predict(qrfozone,quantiles=0.5)


###################################################
### code chunk number 13: quantregForest.Rnw:94-98
###################################################
xozone329 <- ozone[-330,-1]
yozone329 <- ozone$upo3[-330]
qrfozone329 <- quantregForest(xozone329,yozone329)
predict(qrfozone329,quantiles=0.5,newdata=ozone[330,-1])


###################################################
### code chunk number 14: quantregForest.Rnw:103-107
###################################################
casp=read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00265/CASP.csv")
xcasp <- casp[1:10000,-1]
ycasp <- casp$RMSD[1:10000]
qrfcasp <- quantregForest(xcasp,ycasp)


###################################################
### code chunk number 15: quantregForest.Rnw:109-110
###################################################
system.time(predict(qrfcasp,quantiles=0.5,newdata=casp[10001:11000,-1]))


###################################################
### code chunk number 16: quantregForest.Rnw:113-114
###################################################
system.time(predict(qrfcasp,quantiles=0.5,newdata=casp[10001:11000,-1],all=TRUE))


###################################################
### code chunk number 17: quantregForest.Rnw:118-119
###################################################
predict(qrfozone,quantiles=0.5,obs=3)


###################################################
### code chunk number 18: ozoneplot
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(qrfozone)


###################################################
### code chunk number 19: quantregForest.Rnw:140-142
###################################################
plot(qrfozone,all=TRUE)
plot(qrfozone,obs=3)


###################################################
### code chunk number 20: quantregForest.Rnw:148-149
###################################################
qrfozone <- quantregForest(xozone,yozone,importance=TRUE)


###################################################
### code chunk number 21: quantregForest.Rnw:153-154
###################################################
qrfozone$importance


###################################################
### code chunk number 22: quantregForest.Rnw:157-158
###################################################
importance(qrfozone)


###################################################
### code chunk number 23: quantregForest.Rnw:161-162
###################################################
importance(qrfozone,quantile=0.5)


###################################################
### code chunk number 24: varimpozone
###################################################
getOption("SweaveHooks")[["fig"]]()
varImpPlot.qrf(qrfozone)


###################################################
### code chunk number 25: quantregForest.Rnw:178-180
###################################################
varImpPlot.qrf(qrfozone,quantiles=c(0.5,0.9),
symbols=FALSE,color=TRUE,which.sort=2)


###################################################
### code chunk number 26: quantregForest.Rnw:187-193
###################################################
ozoneoutliers <- ozone
ozoneoutliers$upo3[c(50,150,200)] <- ozoneoutliers$upo3[c(50,150,200)]*100
x <- ozoneoutliers[-1]
y <- ozoneoutliers$upo3
qrf <- quantregForest(x,y)
which(y>predict(qrf,quantile=0.99))


###################################################
### code chunk number 27: quantregForest.Rnw:197-210
###################################################
quantiles <- c(0.05,0.5,0.95)
quant <- predict(qrfcasp,quantiles=quantiles,newdata=casp[11000:11500,-1])
z <- quant[,3]-quant[,1]
or <- order(z)
ynew <- casp$RMSD[11000:11500]
n <- length(ynew)

# center and order the quantiles

med <- quant[or,2]-quant[or,2]
upp <- quant[or,3]-quant[or,2]
low <- quant[or,1]-quant[or,2]
ytrain <- ynew[or]-quant[or,2]


###################################################
### code chunk number 28: excasp
###################################################
getOption("SweaveHooks")[["fig"]]()
# Plot the centred observations and the prediction intervals

plot(1:n,ynew[or]-quant[or,2],pch=20,xlab="ordered samples", 
ylab="observed response and prediction 
intervals(centred)",type="n",main="90% prediction intervals") 

dist <- 0.01

for (i in 1:n){
  polygon( c(i-dist,i+dist,i+dist,i-dist), 
  c(upp[i],upp[i],low[i],low[i]) ,col=rgb(0.8,0.8,0.8) ,border=NA)
}
for (i in 1:n){
  lines(c(i-dist,i+dist) , c(upp[i],upp[i]) )
  lines(c(i-dist,i+dist) , c(low[i],low[i]) )
}

inpred <- (ytrain<= upp) & (ytrain>=low)
for (i in  1:n) points(i,ynew[or[i]]-quant[or[i],
2],col=as.numeric(inpred)[i]+2,pch=20)


