
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tmleExtract

The goal of `tmleExtract` is to extract parameter estimates from TMLE
fits produced by the
[`tmle`](https://cran.r-project.org/web/packages/tmle/index.html)
package in a tidy format.

## Installation

You can install with:

`devtools::install_github("nt-williams/tmleExtract")`

## Example

Basic example:

``` r
library(tmleExtract)

set.seed(1)
n <- 250
W <- matrix(rnorm(n*3), ncol=3)
A <- rbinom(n,1, 1/(1+exp(-(.2*W[,1] - .1*W[,2] + .4*W[,3]))))
Y <- A + 2*W[,1] + W[,3] + W[,2]^2 + rnorm(n)
tmle_fit <- tmle::tmle(Y,A,W, Q.SL.library = "SL.glm", g.SL.library = "SL.glm")
#> Loading required package: nnls

tmle_extract(tmle_fit, A, Y)
#>   parameter estimate   variance standard_error        z            p
#> 1       Q1W 2.081951 0.05366308      0.2316529 8.987370 2.532171e-19
#> 2       Q0W 1.066229 0.05208988      0.2282321 4.671688 2.987347e-06
#> 3       ATE 1.015722 0.06779690      0.2603784 3.900945 9.581804e-05
#> 4       ATT 1.009826 0.07016059      0.2648784 3.812414 1.376163e-04
#> 5       ATC 1.030625 0.06696742      0.2587806 3.982619 6.815996e-05
#>    conf.low conf.high
#> 1 1.6279192  2.535982
#> 2 0.6189024  1.513556
#> 3 0.5053894  1.526054
#> 4 0.4906645  1.528988
#> 5 0.5234146  1.537835
```
