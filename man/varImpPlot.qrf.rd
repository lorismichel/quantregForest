\name{varImpPlot.qrf}
\alias{varImpPlot.qrf}
\title{Variable Importance Plot}
\description{
  Dotchart of variable importance as measured by a Quantile Regression Forest
}
\usage{
varImpPlot.qrf(x, quantiles=x$quantiles, symbols=TRUE, color=TRUE,
              sort=TRUE, which.sort=1,
              n.var=min(30, nrow(x$importance)),
              main=deparse(substitute(x)), ...) 
}
\arguments{
  \item{x}{An object of class \code{quantregForest}}
  \item{quantiles}{A vector of quantiles (with numerical values in
    [0,1]) for which the variable importance measure should be extracted. Only quantiles for which the measure already has been computed with \code{\link{quantregForest}} are allowed}
  \item{symbols}{A logical value. If \code{TRUE} symbols are used to distinguish the values for the different quantiles}
  \item{color}{A logical value. If \code{TRUE} colors are used to distinguish the values for the different quantiles}
  \item{sort}{Should the variables be sorted in decreasing order of
    importance?}
  \item{which.sort}{A number between 1 and length(\code{quantiles}). Specifies which quantile is used to order the predictors for the plot (Ignored if \code{sort=FALSE})}
  \item{n.var}{How many variables to show? (Ignored if
    \code{sort=FALSE})}
  \item{main}{plot title.}
  \item{...}{Other graphical parameters to be passed on to
    \code{\link{dotchart}}}
}
\value{
  Invisibly, the importance of the variables that were plotted.
}
\author{Lukas Schiesser}
\seealso{
  \code{\link{quantregForest}}, \code{\link{importance.quantregForest}}
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

## produce a simple variable importance plot
varImpPlot.qrf(qrf)

## plot only one quantile
varImpPlot.qrf(qrf, quantile=0.5)

## use second quantile to order values
varImpPlot.qrf(qrf, which.sort=2)
}