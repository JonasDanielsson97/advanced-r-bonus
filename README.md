
# BonusLab

<!-- badges: start -->
[![R-CMD-check](https://github.com/JonasDanielsson97/advanced-r-bonus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JonasDanielsson97/advanced-r-bonus/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of BonusLab is to implement linear regression and ridge regression with S3 methods for printing, predicting and extracting coefficients.

## Installation

You can install the development version of lab06 from [GitHub](https://github.com/) with:
  
  ``` r
# install.packages("pak")
pak::pak("JonasDanielsson97/advanced-r-bonus")
```

## Example

This is a basic example which shows you how to solve a common problem:
  
  ``` r
library(BonusLab)
data(iris)

ridgereg_testobj <- ridgereg(formula = Petal.Length~Species, data = iris, lambda = 1)

print(ridgereg_testobj)
predict(ridgereg_testobj)
coef(ridgereg_testobj)

linreg_testobj <- linreg(formula = Petal.Length~Species, data = iris)

print(linreg_testobj)
residuals(linreg_testobj)
pred(linreg_testobj)
coef(linreg_testobj)
```
