# quantregForest 
 
[![Build Status](https://travis-ci.org/lorismichel/quantregForest.svg?branch=master)](https://travis-ci.org/lorismichel/quantregForest)
[![Build status](https://ci.appveyor.com/api/projects/status/8ea2sqbnfq9rsu1s/branch/master?svg=true)](https://ci.appveyor.com/project/lorismichel/quantregforest/branch/master)
[![codecov](https://codecov.io/gh/lorismichel/quantregForest/branch/master/graph/badge.svg)](https://codecov.io/gh/lorismichel/quantregForest)
[![HitCount](http://hits.dwyl.io/lorismichel/quantregForest.svg)](http://hits.dwyl.io/lorismichel/quantregForest)
[![CRAN](http://www.r-pkg.org/badges/version-ago/quantregForest)](http://www.r-pkg.org/badges/version-ago/quantregForest)
[![CRAN](http://cranlogs.r-pkg.org/badges/quantregForest)](http://cranlogs.r-pkg.org/badges/quantregForest)
[![Rdoc](http://www.rdocumentation.org/badges/version/quantregForest)](http://www.rdocumentation.org/packages/quantregForest)
## Overview
Quantile Regression Forests is a tree-based ensemble
method for estimation of conditional quantiles [(Meinshausen, 2006)](http://stat.ethz.ch/~nicolai/quantregforests.pdf). It is
particularly well suited for high-dimensional data. Predictor
variables of mixed classes can be handled. The package is
dependent on the package 'randomForest', written by Andy Liaw.

## Installation

you can install the official version from CRAN using the command:

``` r
install.packages("quantregForest")
```

if you want to install the development version  on github use:

``` r
install.packages("devtools")
devtools::install_github("lorismichel/quantregForest")
```

## Issues

To report an issue, please use the [issue tracker](http://github.com/lorismichel/quantregForest/issues) on github.com.
