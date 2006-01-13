"predict.quantregForest" <-
function(object, newdata= NULL, quantiles= c(0.1,0.5,0.9), ... ) {

  ### Checking arguments
  if (!inherits(object, "quantregForest")) 
    stop("object not of class quantregForest")
  if(min(quantiles)<0 | max(quantiles)>1 )
    stop("quantiles must be in [0,1]")
  
  x <- newdata
  if(!is.null(x)){
    if (nrow(x) == 0) 
      stop("newdata has 0 rows")
    if (any(is.na(x))) 
      stop("missing values in newdata")
    keep <- 1:nrow(x)
    rn <- rownames(x)
    if (is.null(rn)) rn <- keep
  }

  if (is.data.frame(x)) {
    for(i in seq(along=ncol(x))) {
      if(is.ordered(x[[i]])) x[[i]] <- as.numeric(x[[i]])
    }
    cat.new <- sapply(x, function(x) if (is.factor(x) && !is.ordered(x)) 
                      length(levels(x)) else 1)
    if (length(cat.new) != length(object$forest$ncat))
      stop("Number of variables in newdata does not match the model.")
    if (!all(object$forest$ncat == cat.new)) 
      stop("Type of predictors in new data do not match that of the training data.")
  }

  if(!is.null(newdata)){
    vname <- if (is.null(dim(object$importance))) {
      names(object$importance)
    } else {
      rownames(object$importance)
    }
    
    if (any(colnames(x) != vname))
      stop("names of predictor variables do not match")
  }
  
  #### Out-of-bag prediction or not ?
    if(is.null(newdata)){
    origObs <- object$origObs
    nobs <- length(origObs)

    origNodes <- object$origNodes
    
    quant <- matrix(nrow=nobs,ncol=length(quantiles))
    ntree <- object$ntree

    normalise <- 0
    weightvec <- rep(0,nobs*nobs)
    counti <- rep(0,nobs)
    thres <- 5*.Machine$double.eps

    
    result <- .C("findweightsinbag",
                 as.double(as.vector(origNodes)),
                 as.integer(as.vector(object$inbag)),
                 weightvec=as.double(weightvec),
                 as.integer(nobs),
                 as.integer(ntree),
                 as.double(thres),
                 as.integer(counti),
                 as.integer(normalise),
                 DUP=FALSE,
                 PACKAGE="quantregForest")


    
    weights <- matrix(result$weightvec,nrow= nobs)

    ord <- order(origObs)
    origObs <- origObs[ord]
    weights <- weights[ord,]
    cumweights <- apply(weights,2,cumsum)
    cumweights <- sweep(cumweights,2,cumweights[nobs,],FUN="/")

    for (qc in 1:length(quantiles)){
      larg <- cumweights<quantiles[qc]
      wc <- apply(larg,2,sum)+1
      quant[,qc] <- origObs[wc]
    }
    
  }else{
    origObs <- object$origObs
    
    origNodes <- object$origNodes
    
    quant <- matrix(nrow=nrow(x),ncol=length(quantiles))
    nodes <- getnodes(object,x)
    ntree <- object$ntree

    nobs <- length(origObs)
    nnew <- nrow(x)
    normalise <- 0
    weightvec <- rep(0,nobs*nnew)
    counti <- rep(0,nobs)
    thres <- 5*.Machine$double.eps
    result <- .C("findweights",
                 as.double(as.vector(origNodes)),
                 as.double(as.vector(nodes)),
                 weightvec=as.double(weightvec),
                 as.integer(nobs),
                 as.integer(nnew),
                 as.integer(ntree),
                 as.double(thres),
                 as.integer(counti),
                 as.integer(normalise),
                 DUP=FALSE,
                 PACKAGE="quantregForest")
    
    weights <- matrix(result$weightvec,nrow= nobs)

    ord <- order(origObs)
    origObs <- origObs[ord]
    weights <- weights[ord,]
    cumweights <- apply(weights,2,cumsum)
    cumweights <- sweep(cumweights,2,cumweights[nobs,],FUN="/")

    for (qc in 1:length(quantiles)){
      larg <- cumweights<quantiles[qc]
      wc <- apply(larg,2,sum)+1
      quant[,qc] <- origObs[wc]
    }

  }
  colnames(quant) <- paste("quantile=",quantiles)
  return(quant)
  
}

