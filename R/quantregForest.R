"quantregForest" <-
function(x,y, mtry= ceiling(ncol(x)/3)  , nodesize= 10, ntree= 1000){

  ## Some checks 
  if(! class(y) %in% c("numeric","integer") )
    stop(" y must be numeric ")
  
  if(is.null(nrow(x)) || is.null(ncol(x)))
    stop(" x contains no data ")
    
  if(length(unique(y))<=4)
    stop(" The response variable y contains less than 5 unique values! Quantile Regression assumes a continuous response variable. ")

  
  if(length(unique(y))<10)
    warning(" The response variable y contains less than 10 unique values! Quantile Regression assumes a continuous response variable.")

  if(mtry < 1 || mtry > ncol(x)){
    warning(" The value of mtry is too low or high! Has been reset to default value.")
    mtry <- max( floor(ncol(x)/3) ,1)
  }

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

  qrf <- randomForest( x=x,y=y,keep.forest=TRUE, mtry=mtry, nodesize=nodesize, ntree=ntree, keep.inbag=TRUE )
  class(qrf) <- c("quantregForest","randomForest")

  qrf[["call"]] <- cl
  qrf[["origNodes"]] <- getnodes(qrf,x)
  qrf[["origObs"]] <- y

  
  return(qrf)
}

