\name{quantregForest}
\alias{quantregForest}
\title{ Quantile Regression Forests }
\description{
  Quantile Regression Forests infer conditional quantile functions from
  data
}
\usage{
quantregForest(x,y, mtry= ceiling(ncol(x)/3)  , nodesize= 10, ntree= 100, 
importance=FALSE ,quantiles=c(0.1,0.5,0.9))
}
\arguments{
  \item{x}{ A matrix or data.frame containing the predictor variables }
  \item{y}{ The response variable; a numerical vector  }
  \item{mtry}{ The number of variables to try for each split; same
  default setting as for Random Forests }
  \item{nodesize}{ The minimal number of instances in each terminal node;
  the default setting is slightly higher than for Random Forests }
  \item{ntree}{ The number of trees to be grown }
  \item{importance}{ If \code{TRUE} importance of predictors is assessed }
  \item{quantiles}{ A vector of quantiles (with numerical values in
    [0,1]) for which the variable importance measure should be computed. (Ignored if \code{importance=FALSE})  }
}

\details{
  It might be useful to try various values of \code{mtry} and see which
  one works best;
  however, results are typically not heavily dependent
  on this parameter.
}
\value{
  A value of class \code{quantregForest}, for which \code{print}, \code{plot}, \code{predict} and \code{importance} methods are available.
  Class \code{quantregForest} is a list of the following components additional to the ones given by class \code{randomForest}:
  \item{call}{the original call to \code{quantregForest}}
  \item{origNodes}{the nodes for the original data in each tree}
  \item{origObs}{the values of the response variable used to fit the tree}
  \item{importance}{if \code{importance=TRUE} a matrix. The first column contains the mean increase in prediction error for the first entry in the vector \code{quantiles}, the second column containes the same for the second entry of \code{quantiles} and so on. }
  \item{quantiles}{list of quantiles for which the variable importance measure was computed if \code{importance=TRUE}}
}
\references{N. Meinshausen (2006)
"Quantile Regression Forests", Journal of Machine Learning Research 7, 983-999
\url{http://jmlr.csail.mit.edu/papers/v7/}}
\author{ Nicolai Meinshausen, Lukas Schiesser }
\seealso{
\code{\link{predict.quantregForest}},
\code{\link{plot.quantregForest}},
\code{\link{importance.quantregForest}},
\code{\link{varImpPlot.qrf}},
}
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

qrf <- quantregForest(x=Xtrain, y=Ytrain, importance=TRUE)


## plot out-of-bag predictions for the training data
plot(qrf)

## compute out-of-bag predictions 
quant.outofbag <- predict(qrf)

## predict test data using all obervations per node for prediction
quant.newdata  <- predict(qrf, newdata= Xtest, all=TRUE)

## look at computed importance measure of predictors
importance(qrf)

}
\keyword{ regression }
\keyword{ tree }
