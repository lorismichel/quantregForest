"importance.quantregForest" <- function(x,quantiles=x$quantiles,...) {
  if (!inherits(x, "quantregForest"))
    stop("x is not of class quantregForest")
  
  hasImp <- (ncol(x$importance)>=1)
  
  if (!hasImp)
    stop("That measure has not been computed")
  
  indices<-match(quantiles,x$quantiles)
  indices<-unique(indices)
  
  if (sum(is.na(indices))>0){
    stop("That measure has not been computed for all given quantiles")
  }
  
  imp <- as.matrix(x$importance[,indices])
  
  if(length(quantiles)==1){
    colnames(imp)<-paste("quantile=",quantiles)
  }
  return(imp)
}