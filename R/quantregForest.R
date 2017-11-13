"quantregForest" <-
function(x,y, nthreads = 1, keep.inbag=FALSE, ...){

  ## Some checks
  #if(! class(y) %in% c("numeric","integer") )
  #  stop(" y must be numeric ")

  if(is.null(nrow(x)) || is.null(ncol(x)))
    stop(" x contains no data ")


  if( nrow(x) != length(y) )
    stop(" predictor variables and response variable must contain the same number of samples ")

  if (any(is.na(x))) stop("NA not permitted in predictors")
  if (any(is.na(y))) stop("NA not permitted in response")



  ## Check for categorial predictors with too many categories (copied from randomForest package)
   if (is.data.frame(x)) {
        ncat <- sapply(x, function(x) if(is.factor(x) && !is.ordered(x))
                       length(levels(x)) else 1)
      } else {
        ncat <- 1
    }
    maxcat <- max(ncat)
    if (maxcat > 32)
        stop("Can not handle categorical predictors with more than 32 categories.")


  ## Note that crucial parts of the computation
  ## are only invoked by the predict method
  cl <- match.call()
  cl[[1]] <- as.name("quantregForest")

  qrf <- if(nthreads > 1){
    parallelRandomForest(x=x, y=y, nthreads = nthreads,keep.inbag=keep.inbag, ...)
  }else{
    randomForest( x=x,y=y ,keep.inbag=keep.inbag,...)
  }

  nodesX <- attr(predict(qrf,x,nodes=TRUE),"nodes")
  rownames(nodesX) <- NULL
  nnodes <- max(nodesX)
  ntree <- ncol(nodesX)
  n <- nrow(x)
  valuesNodes  <- matrix(nrow=nnodes,ncol=ntree)

  for (tree in 1:ntree){
      shuffledNodes <- nodesX[rank(ind <- sample(1:n,n)),tree]
      useNodes <- sort(unique(as.numeric(shuffledNodes)))
      valuesNodes[useNodes,tree] <- y[ind[match(useNodes,shuffledNodes )]]
  }



  qrf[["call"]] <- cl
  qrf[["valuesNodes"]] <- valuesNodes

  if(keep.inbag){
    
    # create a prediction vector with same shape as predictOOBNodes
    predictOOBNodes <- attr(predict(qrf,newdata=x,nodes=TRUE),"nodes")
    rownames(predictOOBNodes) <- NULL
    valuesPredict <- 0*predictOOBNodes
    ntree <- ncol(valuesNodes)
    valuesPredict[ qrf$inbag >0] <- NA


    # for each tree and observation sample another observation of the same node
    for (tree in 1:ntree){

      is.oob <- qrf$inbag[,tree] == 0
      n.oob <- sum(is.oob)

      if(n.oob!=0) {
      	
	  y.oob  <- sapply(which(is.oob),
		    function(i) {
			    cur.node <- nodesX[i,tree]
			    cur.y <- if(length(cur.y <- y[setdiff(which(nodesX[,tree]==cur.node),i)])!=0) {
				    	sample(x = cur.y,size = 1)
			             } else {
			              	NA
			    	     }			   
			    return(cur.y)
		       })
          valuesPredict[is.oob, tree] <- y.oob
      }
    }

    minoob <- min( apply(!is.na(valuesPredict),1,sum))
    if(minoob<10) stop("need to increase number of trees for sufficiently many out-of-bag observations")
    valuesOOB <- t(apply( valuesPredict,1 , function(x) sample( x[!is.na(x)], minoob)))
    qrf[["valuesOOB"]] <- valuesOOB
  }
  class(qrf) <- c("quantregForest","randomForest")

  return(qrf)
}
