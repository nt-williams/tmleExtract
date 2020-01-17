
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tmleExtract

The goal of tmleExtract is to extract treatment specific mean estimates
from TMLE fits produced by the *tmle* package.

## Installation

You can install with:

`devtools::install_github("nt-williams/tmleExtract")`

## Example

Basic example:

``` r
library(tmleExtract)
#> Loading required package: tmle
#> Loading required package: glmnet
#> Loading required package: Matrix
#> Loaded glmnet 3.0-1
#> Loading required package: SuperLearner
#> Loading required package: nnls
#> Super Learner
#> Version: 2.0-25
#> Package created on 2019-08-05
#> Welcome to the tmle package, version 1.4.0.1
#> 
#> Use tmleNews() to see details on changes and bug fixes
library(tmle)

set.seed(1)
n <- 250
W <- matrix(rnorm(n*3), ncol=3)
A <- rbinom(n,1, 1/(1+exp(-(.2*W[,1] - .1*W[,2] + .4*W[,3]))))
Y <- A + 2*W[,1] + W[,3] + W[,2]^2 + rnorm(n)
tmle_fit <- tmle(Y,A,W, Q.SL.library = "SL.glm", g.SL.library = "SL.glm")

tmle_extract(tmle_fit, A, Y)
#>   parameter estimate   variance standard_error        z            p
#> 1       Q1W 2.081951 0.05366308      0.2316529 8.987370 2.532171e-19
#> 2       Q0W 1.066229 0.05208988      0.2282321 4.671688 2.987347e-06
#> 3       ATE 1.015722 0.06779690      0.2603784 3.900945 9.581804e-05
#>    conf.low conf.high
#> 1 1.6279192  2.535982
#> 2 0.6189024  1.513556
#> 3 0.5053894  1.526054
```
