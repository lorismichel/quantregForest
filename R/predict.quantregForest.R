"predict.quantregForest"<-function(object,newdata=NULL, what=c(0.1,0.5,0.9), keep.conditional.samples = F,...  )
{
    class(object) <- "randomForest"
    if(is.null(newdata)){
        if(is.null(object[["valuesOOB"]])) stop("need to fit with option keep.inbag=TRUE \n if trying to get out-of-bag observations")
        valuesPredict <- object[["valuesOOB"]]
    }else{
        predictNodes <- attr(predict(object,newdata=newdata,nodes=TRUE),"nodes")
        rownames(predictNodes) <- NULL
        valuesPredict <- 0*predictNodes
        ntree <- ncol(object[["valuesNodes"]])
        for (tree in 1:ntree){
            valuesPredict[,tree] <- object[["valuesNodes"]][ predictNodes[,tree],tree]  
        }
    }
    if(is.function(what)){
        if(is.function(what(1:4))){
            result <- apply(valuesPredict,1,what)
        }else{
            if(length(what(1:4))==1){
                result <- apply(valuesPredict,1,what)
            }else{
                result <- t(apply(valuesPredict,1,what))
            }
        }
    }else{
        if( !is.numeric(what)) stop(" `what' needs to be either a function or a vector with quantiles")
        if( min(what)<0) stop(" if `what' specifies quantiles, the minimal values needs to be non-negative")
        if( max(what)>1) stop(" if `what' specifies quantiles, the maximal values cannot exceed 1")
        if(length(what)==1){
            result <- apply( valuesPredict,1,quantile, what,na.rm=TRUE)
        }else{
            result <- t(apply( valuesPredict,1,quantile, what,na.rm=TRUE))
            colnames(result) <- paste("quantile=",what)
        }
    }

    if (keep.conditional.samples) {
      return(list(quantiles = result, samples = valuesPredict))
    } else {
      return(result)
    }
    
    
}
