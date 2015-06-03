"varImpPlot.qrf"<- function(x, quantiles=x$quantiles, symbols=TRUE, color=TRUE , sort=TRUE, which.sort=1,n.var=min(30, nrow(x$importance)),main=deparse(substitute(x)), ...)
{
  if (!inherits(x, "quantregForest"))
    stop("This function only works for objects of class `quantregForest'")
  
  if (which.sort > length(quantiles) || !{which.sort%%1==0} || which.sort < 1){
    stop("which.sort has to be an integer value between 1 and length(quantiles)")
  }
  
  if (symbols==FALSE && color==FALSE){
    stop("Neither symbols nor color has value TRUE")
  }
  
  imp <- importance(x, quantiles=quantiles, ...)
  nmeas <- ncol(imp)
  
  for (i in 1:nmeas) {
    
    xmin <- 0
    if(i==1){
      ord <- if (sort) rev(order(imp[,which.sort],
                                 decreasing=TRUE)[1:n.var]) else 1:n.var
      dotchart(imp[ord,i], xlab="%IncQuantileLoss", ylab="",
              main=main,
              xlim=c(xmin, max(imp)), ...)
    }
    else{
      if(symbols==TRUE && color==FALSE){
        points(imp[ord,i],1:n.var,pch=i)
      }
      else{
        if(symbols==FALSE && color==TRUE){
          points(imp[ord,i],1:n.var,col=i)
        }
        else{
          points(imp[ord,i],1:n.var,pch=i,col=i)
        }
      }
    }
  }
  if(symbols==TRUE && color==FALSE){
    legend("bottomright",pch=1:nmeas,legend=colnames(imp))
  }
  else{
    if(symbols==FALSE && color==TRUE){
      legend("bottomright",fill=1:nmeas,legend=colnames(imp))
    }
    else{
      legend("bottomright",pch=1:nmeas,col=1:nmeas,legend=colnames(imp))
    }
  }
  invisible(imp)
}