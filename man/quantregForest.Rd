\name{quantregForest}
\alias{quantregForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Quantile Regression Forests }
\description{
  Quantile Regression Forests infer conditional quantile functions from
  data
}
\usage{
quantregForest(x, y, mtry = ceiling(ncol(x)/3), nodesize = 10, ntree = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A matrix or data.frame containing the predictor variables }
  \item{y}{ The response variable; a numerical vector  }
  \item{mtry}{ The number of variables to try for each split; same
  default setting as for Random Forests }
  \item{nodesize}{ The minimal number of instances in each terminal node;
  the default setting is slightly higher than for Random Forests }
  \item{ntree}{ The number of trees to be grown }
}

\details{
  It might be useful to try various values of \code{mtry} and see which
  one works best;
  however, results are typically not heavily dependent
  on this parameter.
}
\value{
  A value of class \code{quantregForest}, for which \code{print}, \code{plot}, and
  \code{predict} methods are available.
}
\references{N. Meinshausen (2006)
"Quantile Regression Forests", Journal of Machine Learning Research 7, 983-999
\url{http://jmlr.csail.mit.edu/papers/v7/}}
\author{ Nicolai Meinshausen }
\seealso{ For prediction, see \code{\link{predict.quantregForest}} }
\examples{

################################################
##  Load air-quality data (and preprocessing) ##
################################################

data(airquality)
set.seed(1)


## remove observations with mising values
airquality <- airquality[ !apply(is.na(airquality), 1,any), ]

## number of remining samples
n <- nrow(airquality)


## divide into training and test data
indextrain <- sample(1:n,round(0.6*n),replace=FALSE)
Xtrain     <- airquality[ indextrain,2:6]
Xtest      <- airquality[-indextrain,2:6]
Ytrain     <- airquality[ indextrain,1]
Ytest      <- airquality[-indextrain,1]



################################################
##     compute Quantile Regression Forests    ##
################################################

qrf <- quantregForest(x=Xtrain, y=Ytrain)


## plot out-of-bag predictions for the training data
plot(qrf)

## compute out-of-bag predictions 
quant.outofbag <- predict(qrf)

## predict test data
quant.newdata  <- predict(qrf, newdata= Xtest)



}
\keyword{ regression }
\keyword{ tree }
