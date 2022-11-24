# SurrogateRsq: an R Package for evaluating the goodness of fit using the surrogate R-squared 

<!-- badges: start -->

[![](https://img.shields.io/cran/v/SurrogateRsq?logo=R)](https://cran.r-project.org/package=SurrogateRsq)
[![CRAN checks](https://cranchecks.info/badges/summary/SurrogateRsq)](https://cran.r-project.org/web/checks/check_results_SurrogateRsq.html)
[![](https://cranlogs.r-pkg.org/badges/grand-total/SurrogateRsq?color=blue)](https://cranlogs.r-pkg.org/badges/grand-total/SurrogateRsq)
[![](https://cranlogs.r-pkg.org/badges/last-month/SurrogateRsq?color=green)](https://cranlogs.r-pkg.org/badges/last-month/SurrogateRsq?color=green)
[![](https://cranlogs.r-pkg.org/badges/last-week/SurrogateRsq?color=yellow)](https://cranlogs.r-pkg.org/badges/last-week/SurrogateRsq?color=yellow)
[![](https://api.travis-ci.com/XiaoruiZhu/SurrogateRsq.svg?branch=master)](https://api.travis-ci.com/XiaoruiZhu/SurrogateRsq.svg)
<!-- badges: end -->

Overview
--------

This package implements the tools for the goodness-of-fit analysis for the probit model (Dungang Liu, Xiaorui Zhu, Brandon Greenwell, and Zewei Lin (2022)). This package can generate a point or interval measure of the surrogate $R^2$. It can also provide a ranking measure of each variable's contribution in terms of surrogate goodness-of-fit measure. This ranking assessment allows one to check the importance of each variable in terms of their explained variance. It can be jointly used with other existing R packages for variable selection and model diagnostics in the model-building process. 

## Installation

The `SurrogateRsq` package will be available on [SurrogateRsq CRAN](https://CRAN.R-project.org/package=SurrogateRsq) soon.

### Install `SurrogateRsq` development version from GitHub (recommended)

``` r
# Install the development version from GitHub
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("XiaoruiZhu/SurrogateRsq")
```

### Install `SurrogateRsq` from the CRAN

``` r
# Install from CRAN
install.packages("SurrogateRsq")
```

## Example

The following example shows the R code for analyzing white wine-tasting data ( data("WhiteWine") ).The tasting dataset of white wine contains 4898 samples and 11 explanatory variables. The explanatory variables are the physicochemical features of the wine, for example, the acidity, sugar, dioxide, pH, and others. The response variable is the tasting rating score of the wine, which ranges from 0 (very bad) to 10 (excellent).

``` r 
library(SurrogateRsq)
library(MASS)
data("WhiteWine")

# Build the full model
full_formula_white <- 
  as.formula(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
                       residual.sugar + chlorides + free.sulfur.dioxide +
                       total.sulfur.dioxide + density + pH + sulphates + 
                       alcohol)

naive_model_white <- polr(formula = full_formula_white, 
                          data    = WhiteWine, 
                          method  = "probit")

# selected model
select_model_white <- 
  update(naive_model_white, 
         formula. = 
           ". ~ . - citric.acid - chlorides - total.sulfur.dioxide - density")

# surrogate R-squared
surr_obj_white <- 
  surr_rsq(model      = select_model_white,
           full_model = select_model_white, 
           data       = WhiteWine, 
           avg.num    = 30)
surr_obj_white$surr_rsq

# surrogate R-squared rank table
test_var_set_white <- 
  list(
    c("alcohol"),
    c("volatile.acidity"),
    c("residual.sugar"),
    c("free.sulfur.dioxide"),
    c("sulphates"),
    c("fixed.acidity"),
    c("pH")
    )

Rank_table_white <- 
  surr_rsq_rank(object  = select_model_white, 
                data    = WhiteWine,
                var.set = test_var_set_white, 
                avg.num = 30)

Rank_table_white$Reduction <- 
  percent(surr_obj_white$surr_rsq - Rank_table_white$SurrogateR2, 0.01)
Rank_table_white

# 95% confidence interval surrogate R-squared

surr_obj_white_ci <- surr_rsq_ci(surr_rsq = surr_obj_white,
                                alpha     = 0.05,
                                B         = 2000)
```


