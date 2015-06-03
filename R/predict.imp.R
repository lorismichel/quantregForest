"predict.imp" <-
function(object, quantiles= c(0.1,0.5,0.9),obs=1,origpred,...) {
  
    ### Checking arguments
    if (!inherits(object, "quantregForest")) 
      stop("object not of class quantregForest")
    if(min(quantiles)<0 | max(quantiles)>1 )
      stop("quantiles must be in [0,1]")
      
    
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
      
    npred<-ncol(origpred)
    countbreak<-rep(0,npred+1)
    pred.perm<-array(NA,c(nobs,npred,npred))
    origNodes.perm<-array(NA,c(nobs,ntree,npred))
    for(i in 1:npred){
      pred.perm[,,i]<-as.matrix(origpred)
      pred.perm[,i,i]<-sample(origpred[,i],size=nobs,replace=FALSE)
      origNodes.perm[,,i]<-as.matrix(getnodes(object,pred.perm[,,i]))
    }
    weightvec.perm<-rep(0,nobs*nobs*npred)
      
    result <- .C("findweightsinbagfastimp",
                  as.double(as.vector(origNodes)),
                  as.double(as.vector(origNodes.perm)),
                  as.double(as.vector(newnodes)),
                  as.double(filterednodes),
                  as.integer(as.vector(z)),
                  as.integer(as.vector(newindex)),
                  as.integer(as.vector(object$inbag)),
                  weightvec=as.double(weightvec),
                  weightvecperm=as.double(weightvec.perm),
                  as.integer(npred),
                  as.integer(nobs),
                  as.integer(ntree),
                  as.double(thres),
                  as.integer(obs),
                  as.integer(as.vector(countbreak)),
                  PACKAGE="quantregForest")
      
    weights <- matrix(result$weightvec,nrow= nobs)
    weights.perm<-array(result$weightvecperm,dim=c(nobs,nobs,npred))
    ord <- order(origObs)
    origObs <- origObs[ord]
    weights <- weights[ord,,drop=FALSE]
    cumweights <- apply(weights,2,cumsum)
    cumweights <- sweep(cumweights,2,as.numeric(cumweights[nobs,]),FUN="/")
    
    cumweights.perm<-array(NA,dim=c(nobs,nobs,npred))
    for(i in 1:npred){
      weights.perm[,,i]<-weights.perm[ord,,i,drop=FALSE]
      cumweights.perm[,,i]<-apply(weights.perm[,,i],2,cumsum)
      cumweights.perm[,,i]<-sweep(cumweights.perm[,,i],2,as.numeric(cumweights.perm[nobs,,i]),FUN="/")
    }
    quant.perm<-array(NA,dim=c(nobs,length(quantiles),npred))
      
    for (qc in 1:length(quantiles)){
      for(i in 1:npred){
        larg.perm<-cumweights.perm[,,i]<quantiles[qc]
        wc<-apply(larg.perm,2,sum)
        ind1<-which(wc<1.1)
        indn1<-which(wc>1.1)
        quant.perm[ind1,qc,i]<-rep(origObs[1],length(ind1))
        quantmax<-origObs[wc[indn1]]
        quantmin<-origObs[wc[indn1]-1]
        weightmax<-cumweights.perm[cbind(wc[indn1],indn1,i)]
        weightmin<-cumweights.perm[cbind(wc[indn1]-1,indn1,i)]
        factor<-numeric(length(indn1))
        indz<-weightmax-weightmin<10^(-10)
        factor[indz]<-0.5
        factor[!indz] <- (quantiles[qc]-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
        quant.perm[indn1,qc,i] <- quantmin + factor* (quantmax-quantmin)
      }
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
    
    obs.oob<-object$origObs
    if(sum(is.na(quant[,1]))>0){
    indices.na<-(which(is.na(quant[,1])))
    quant<-quant[-indices.na,]
    quant.perm<-quant.perm[-indices.na,,]
    noob<-nrow(quant.perm)
    quant.perm<-array(quant.perm,c(noob,length(quantiles),npred))
    obs.oob<-obs.oob[-indices.na]
    }
    resOOB<-apply(loss(obs.oob,quantiles,quant),2,mean)
    errimp<-matrix(0,nrow=npred,ncol=length(quantiles))
    for(mr in 1:npred){
      errimp[mr,]<-(apply(loss(obs.oob,quantiles,quant.perm[,,mr]),2,mean)-resOOB)/resOOB
    }
    errimp<-100*errimp
    row.names(errimp)<-rownames(object$importance)
    colnames(errimp)<-paste("quantile=",quantiles)
    return(errimp)
  }