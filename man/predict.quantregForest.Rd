\name{predict.quantregForest}
\alias{predict.quantregForest}
\title{
Prediction method for class quantregForest
}
\description{
Prediction of test data with quantile regression forests.
}

\usage{
\method{predict}{quantregForest}(object, newdata = NULL, 
                         what = c(0.1, 0.5, 0.9), keep.conditional.samples = F,...)
}
\arguments{
\item{object}{ An object of class \code{quantregForest} }
\item{newdata}{ A data frame or matrix containing new data. 

If left at default \code{NULL}, the out-of-bag predictions (OOB) are returned, for which the option \code{keep.inbag} has to be set to \code{TRUE} at the time of fitting the object. }
\item{what}{ Can be a vector of quantiles or a function.
 
 Default for \code{what} is a a vector of quantiles (with numerical values in
    [0,1]) for which the conditional quantile estimates should be returned.

If a function it has to take as argument a numeric vector and return either a summary statistic (such as \code{mean},\code{median} or \code{sd} to get conditional mean, median or standard deviation) or a vector of values (such as with \code{quantiles} or via \code{sample}) or a function (for example with \code{ecdf}).}  
\item{keep.conditional.samples}{a logical value, should be keep the conditional samples?}
\item{...}{ Additional arguments (currently not in use).}
}

\value{
A vector, matrix or list. 

If \code{what} is a vector with desired quantiles (the default is \code{what=c(0.1,0.5,0.9)}), a matrix with one column per requested quantile returned. 

If just a single quantile is specified (for example via \code{what=0.5}), a vector is returned.

If \code{what} is a function with numerical return value (for example via \code{what=function(x) sample(x,10,replace=TRUE)} to sample 10 new observations from conditional distribution), the output is also a matrix (or vector if just a scalar is returned). 

If \code{what} has a function as output (such as \code{what=ecdf}), a list will be returned with one element per new sample point (and the element contains the desired function).
}
\author{
Nicolai Meinshausen, Christina Heinze
}

\seealso{
\code{\link{quantregForest}},
\code{\link{predict.quantregForest}}
}
\examples{
\dontshow{
library(quantregForest)
}

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
qrf <- quantregForest(x=Xtrain, y=Ytrain, nodesize=10,sampsize=30)


## predict 0.1, 0.5 and 0.9 quantiles for test data
conditionalQuantiles  <- predict(qrf,  Xtest)
print(conditionalQuantiles[1:4,])

## predict 0.1, 0.2,..., 0.9 quantiles for test data
conditionalQuantiles  <- predict(qrf, Xtest, what=0.1*(1:9))
print(conditionalQuantiles[1:4,])

## estimate conditional standard deviation
conditionalSd <- predict(qrf,  Xtest, what=sd)
print(conditionalSd[1:4])

## estimate conditional mean (as in original RF)
conditionalMean <- predict(qrf,  Xtest, what=mean)
print(conditionalMean[1:4])

## sample 10 new observations from conditional distribution at each new sample
newSamples <- predict(qrf, Xtest,what = function(x) sample(x,10,replace=TRUE))
print(newSamples[1:4,])


## get ecdf-function for each new test data point
## (output will be a list with one element per sample)
condEcdf <- predict(qrf,  Xtest, what=ecdf)
condEcdf[[10]](30) ## get the conditional distribution at value 30 for i=10
## or, directly, for all samples at value 30 (returns a vector)
condEcdf30 <- predict(qrf, Xtest, what=function(x) ecdf(x)(30))
print(condEcdf30[1:4])

## to use other functions of the package randomForest, convert class back
class(qrf) <- "randomForest"
importance(qrf) ## importance measure from the standard RF


}
