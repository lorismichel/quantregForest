\name{predict.quantregForest}
\alias{predict.quantregForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{predict method for class quantregForest }
\description{
  Prediction of test data with Quantile Regression Forests
}
\usage{
\method{predict}{quantregForest}(object, newdata = NULL, quantiles =
c(0.1, 0.5, 0.9), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ An object of class \code{quantregForest} }
  \item{newdata}{ A data frame or matrix containing new data. If
    not given, the out-of-bag prediction in \code{object} is returned }
  \item{quantiles}{ A vector of quantiles (with numerical values in
    [0,1]) for which the quantile estimates should be returned }
  \item{...}{ Further arguments (not in use in the current version) }
}
\value{
  A matrix. The first column contains the conditional quantile estimates
  for the first entry in the vector \code{quantiles}, the second column
  contains the estimates for the second entry of \code{quantiles} and so
  on. 
}
\references{N. Meinshausen (2006)
"Quantile Regression Forests", Journal of Machine Learning Research 7, 983-999
\url{http://jmlr.csail.mit.edu/papers/v7/} }
\author{ Nicolai Meinshausen }
\seealso{ \code{\link{quantregForest}} for fitting Quantile Regression Forests }
%\examples{ }
\keyword{ regression }
\keyword{ tree }
