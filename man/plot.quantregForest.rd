\name{plot.quantregForest}
\alias{plot.quantregForest}
\title{
Plot method for class quantregForest
}
\description{
Plots the 90\%-prediction intervals on out-of-bag data for a given object of class \code{quantregForest}.
}

\usage{
\method{plot}{quantregForest}(x, all = FALSE, obs = 1, ...)
}
\arguments{
\item{x}{ An object of class \code{quantregForest} }
\item{all}{ A logical value. \code{all=TRUE} uses all observations for prediction. \code{all=FALSE} uses only a certain number of observations per node for prediction (set with argument \code{obs}). The default is \code{all=FALSE} }
\item{obs}{ An integer number. Determines the maximal number of observations per node used for prediction. The input is ignored for \code{all=TRUE}. The default is \code{obs=1} }
\item{...}{ Further arguments (not in use in the current version) }
 
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
}
