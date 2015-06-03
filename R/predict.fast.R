"predict.fast" <-
function(object, newdata= NULL, quantiles= c(0.1,0.5,0.9),obs=1,...) {
  
    ### Checking arguments
    if (!inherits(object, "quantregForest")) 
      stop("object not of class quantregForest")
    if(min(quantiles)<0 | max(quantiles)>1 )
      stop("quantiles must be in [0,1]")
    
    x <- newdata
    if(!is.null(x)){
      if(is.vector(x)){
        x <- matrix(x,nrow=1)
      }
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
        stop("Type of predictors in new data do not match types of the training data.")
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
      
      weightvec <- rep(0,nobs*nobs)
      thres <- 5*.Machine$double.eps
      
      filterednodes<-rep(0,nobs*ntree)
      z<-matrix(nrow=nobs,ncol=ntree)
      newnodes<-matrix(nrow=nobs,ncol=ntree)
      newindex<-matrix(0,nrow=nobs,ncol=ntree)
      z<-apply(origNodes,2,myorder)#ordering the nodes with randomization
      newnodes<-sapply(seq(ncol(z)),function(x) origNodes[z[,x],x])
      
      result <- .C("findweightsinbagfast",
                   as.double(as.vector(origNodes)),
                   as.double(as.vector(newnodes)),
                   as.double(filterednodes),
                   as.integer(as.vector(z)),
                   as.integer(as.vector(newindex)),
                   as.integer(as.vector(object$inbag)),
                   weightvec=as.double(weightvec),
                   as.integer(nobs),
                   as.integer(ntree),
                   as.double(thres),
                   as.integer(obs),
                   PACKAGE="quantregForest")
      
      weights <- matrix(result$weightvec,nrow= nobs)
      
      ord <- order(origObs)
      origObs <- origObs[ord]
      weights <- weights[ord,,drop=FALSE]
      cumweights <- apply(weights,2,cumsum)
      cumweights <- sweep(cumweights,2,as.numeric(cumweights[nobs,]),FUN="/")
      
      for (qc in 1:length(quantiles)){
        larg <- cumweights<quantiles[qc]
        wc <- apply(larg,2,sum)+1
        ind1 <- which(wc<1.1) 
        indn1 <- which(wc>1.1)
        quant[ind1,qc] <- rep(origObs[1],length(ind1))
        quantmax <- origObs[wc[indn1]]
        quantmin <- origObs[wc[indn1]-1]
        weightmax <- cumweights[cbind(wc[indn1],indn1)]
        weightmin <- cumweights[cbind(wc[indn1]-1,indn1)]
        factor <- numeric(length(indn1))
        indz <- weightmax-weightmin<10^(-10)
        factor[indz] <- 0.5
        factor[!indz] <- (quantiles[qc]-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
        quant[indn1,qc] <- quantmin + factor* (quantmax-quantmin)
      }
      
    }else{
      origObs <- object$origObs
      
      origNodes <- object$origNodes
      
      quant <- matrix(nrow=nrow(x),ncol=length(quantiles))
      nodes <- getnodes(object,x)
      ntree <- object$ntree
      
      nobs <- length(origObs)
      nnew <- nrow(x)
      
      weightvec <- rep(0,nobs*nnew)
      thres <- 5*.Machine$double.eps
      
      filterednodes<-rep(0,nobs*ntree)
      z<-matrix(nrow=nobs,ncol=ntree)
      newnodes<-matrix(nrow=nobs,ncol=ntree)
      newindex<-matrix(0,nrow=nobs,ncol=ntree)
      z<-apply(origNodes,2,myorder)#ordering the nodes with randomization
      newnodes<-sapply(seq(ncol(z)),function(x) origNodes[z[,x],x])
      
      result <- .C("findweightsfast",
                   as.double(as.vector(newnodes)),
                   as.double(as.vector(nodes)),
                   as.double(filterednodes),
                   as.integer(as.vector(z)),
                   as.integer(as.vector(newindex)),
                   weightvec=as.double(weightvec),
                   as.integer(nobs),
                   as.integer(nnew),
                   as.integer(ntree),
                   as.double(thres),
                   as.integer(obs),
                   PACKAGE="quantregForest")
      
      weights <- matrix(result$weightvec,nrow= nobs)
      
      ord <- order(origObs)
      origObs <- origObs[ord]
      weights <- weights[ord,,drop=FALSE]
      cumweights <- apply(weights,2,cumsum)
      cumweights <- sweep(cumweights,2,as.numeric(cumweights[nobs,]),FUN="/")
  
      for (qc in 1:length(quantiles)){
        larg <- cumweights<quantiles[qc]
        wc <- apply(larg,2,sum)+1
        ind1 <- which(wc<1.1)
        indn1 <- which(wc>1.1)
        quant[ind1,qc] <- rep(origObs[1],length(ind1))
        quantmax <- origObs[wc[indn1]]
        quantmin <- origObs[wc[indn1]-1]
        weightmax <- cumweights[cbind(wc[indn1],indn1)]
        weightmin <- cumweights[cbind(wc[indn1]-1,indn1)]
        factor <- numeric(length(indn1))
        indz <- weightmax-weightmin<10^(-10)
        factor[indz] <- 0.5
        factor[!indz] <- (quantiles[qc]-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
        quant[indn1,qc] <- quantmin + factor* (quantmax-quantmin)
      }
      
    }
    colnames(quant) <- paste("quantile=",quantiles)
    return(quant)
    
  }
