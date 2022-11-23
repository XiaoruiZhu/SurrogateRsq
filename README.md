# SurrogateRsq: an R Package for the surrogate R-squared, a goodness-of-fit measure for the models with categorical response 

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/SurrogateRsq)](https://www.r-pkg.org/badges/version/)
[![CRAN checks](https://cranchecks.info/badges/summary/SurrogateRsq)](https://cran.r-project.org/web/checks/)
[![](https://cranlogs.r-pkg.org/badges/grand-total/SurrogateRsq?color=blue)](https://cranlogs.r-pkg.org/badges/grand-total/)
[![](https://cranlogs.r-pkg.org/badges/last-month/SurrogateRsq?color=green)](https://cranlogs.r-pkg.org/badges/last-month/)
[![](https://cranlogs.r-pkg.org/badges/last-week/SurrogateRsq?color=yellow)](https://cranlogs.r-pkg.org/badges/last-week/)

<!-- badges: end -->

Overview
--------

An implementation of the 

## Installation

The `SurrogateRsq` package will be available on [SurrogateRsq CRAN](https://CRAN.R-project.org/package=) soon.

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

# surrogate R-square
surr_obj_white <- 
  surr_rsq(model      = select_model_white,
           full_model = select_model_white, 
           data       = WhiteWine, 
           avg.num    = 30)
surr_obj_white$surr_rsq

# surrogate R-square rank table
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
                avg.num = 300)

Rank_table_white$Reduction <- 
  percent(surr_obj_white$surr_rsq - Rank_table_white$SurrogateR2, 0.01)
Rank_table_white

# 95% confidence interval surrogate R-squared

surr_obj_white_ci <- surr_rsq_ci(surr_rsq = surr_obj_white,
                                alpha     = 0.05,
                                B         = 2000)
```


