\name{importance}
\alias{importance}
\alias{importance.quantregForest}
\title{Extract variable importance measure}
\description{
  This is the extractor function for variable importance measures as
  produced by \code{\link{quantregForest}}.
}
\usage{
\method{importance}{quantregForest}(x, quantiles=x$quantiles, ...)
}
\arguments{
  \item{x}{an object of class \code{\link{quantregForest}}}
  \item{quantiles}{A vector of quantiles (with numerical values in
    [0,1]) for which the variable importance measure should be extracted. Only quantiles for which the measure already has been computed with \code{\link{quantregForest}} are allowed}
  \item{...}{not used}
}
\value{
  A matrix of importance measure, one row for each predictor variable and one column for each quantile.
}
\details{
  The variable importance measure is computed from permuting OOB data: First, the prediction error on out-of-bag portion of data is recorded (quantile loss function).
  Then the same is done after permuting each predictor variable. The
  differences between the two are then averaged over the number of out-of-bag data. This is done for each quantile separately specified by the input \code{quantiles} in \code{quantregForest}
}
\author{Lukas Schiesser}
\seealso{
  \code{\link{quantregForest}}, \code{\link{varImpPlot.qrf}}
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

## look at computed importance measure of predictors
importance(qrf)

## print the measure only for one quantile
importance(qrf, quantiles=0.5)
}