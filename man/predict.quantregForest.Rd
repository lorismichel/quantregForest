\name{predict.quantregForest}
\alias{predict.quantregForest}
\title{
Prediction method for class quantregForest
}
\description{
Prediction of test data with quantile regression forests.
}

\usage{
\method{predict}{quantregForest}(object, newdata = NULL, quantiles = c(0.1, 0.5, 0.9), all = FALSE, obs = 1, ...)
}
\arguments{
\item{object}{ An object of class \code{quantregForest} }
\item{newdata}{ A data frame or matrix containing new data. If
    not given, the out-of-bag prediction in \code{object} is returned }
\item{quantiles}{ A vector of quantiles (with numerical values in
    [0,1]) for which the quantile estimates should be returned }
\item{all}{ A logical value. \code{all=TRUE} uses all observations for prediction. \code{all=FALSE} uses only a certain number of observations per node for prediction (set with argument \code{obs}). The default is \code{all=FALSE} }
\item{obs}{ An integer number. Determines the maximal number of observations per node to use for prediction. The input is ignored for \code{all=TRUE}. The default is \code{obs=1} }
\item{...}{ Further arguments (not in use in the current version) }
 
}

\details{
  It might be useful to try \code{all=TRUE} if the function is slow when given a high number of observations to fit the model and predicting the quantiles only for few new sample points.
}
\value{
A matrix. The first column contains the conditional quantile estimates for the first entry in the vector quantiles. The second column contains the estimates for the second entry of quantiles and so on.
}

\author{
Nicolai Meinshausen, Lukas Schiesser
}

\seealso{
\code{\link{quantregForest}},
\code{\link{predict.quantregForest}}
}
\examples{
\dontshow{
library(quantregForest)
}
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

qrf <- quantregForest(x=Xtrain, y=Ytrain)

## plot out-of-bag predictions for the training data
plot(qrf)

## compute out-of-bag predictions 
quant.outofbag <- predict(qrf)

## predict test data using all obervations per node for prediction
quant.newdata  <- predict(qrf, newdata= Xtest,all=TRUE)
}
