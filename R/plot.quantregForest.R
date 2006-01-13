"plot.quantregForest" <-
function(x, ...){

  quantiles <- c(0.05,0.5,0.95)
  quant <- predict(x,quantiles=quantiles)

  or <- order(quant[,2])
  n <- length(x$origObs)
  plot(quant[or,2],x$origObs[or],pch=20,xlab="predicted median values", ylab ="observed response",type="n",main="95 \% prediction intervals on out-of-bag data") 


  med <- quant[or,2]
  upp <- quant[or,3]
  low <- quant[or,1]
  ytrain <- x$origObs[or]
  
  dist <- mean(diff(sort(med)),trim=0.05)/3

  for (i in 1:n){
    polygon( c(med[i]-dist,med[i]+dist,med[i]+dist,med[i]-dist), c(upp[i],upp[i],low[i],low[i]) ,col=rgb(0.8,0.8,0.8) ,border=NA)
  }
  for (i in 1:n){
    lines(c(med[i]-dist,med[i]+dist) , c(upp[i],upp[i]) )
    lines(c(med[i]-dist,med[i]+dist) , c(low[i],low[i]) )
  }
  
  inpred <- (ytrain<= upp) & (ytrain>=low)
  for (i in  1:n) points(med[i],x$origObs[or[i]],col=as.numeric(inpred)[i]+2,pch=20)
  abline(c(0,1),col=rgb(0.5,0.5,0.5))

  legend(x=min(med),y=max(x$origObs), legend=c("inside prediction interval","outside predicition interval"),fill=c(3,2))
  
}
  
