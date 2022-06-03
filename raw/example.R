# An example ---------------------------------------------------------------
library(DescTools); library(knitr)
library(doParallel);
library(magrittr)
library(MASS)  # for mvtrnorm() and polr() functions
# library(sure)  # for surrogate() function
library(knitr); library(kableExtra); library(performance)
library(VGAM);
library(stargazer)
library("PerformanceAnalytics")
library(ordinal)
library(dplyr)
library(DescTools)
library(scales)
library(glmnet);library(mgcv);library(psych);library(PRROC);
library(ResourceSelection); library(ncvreg); #library(parcor)
library(rlang)


wine_white <- read.csv(file = "winequality-white.csv",header = T, sep = ";")
data <- wine_white %>%mutate(quality = as.ordered(quality))
y<-"quality"
# Find all covariates
x_names <- colnames(data)[!(colnames(data) %in% y)]
full_formula <- as.formula(paste(y, paste(x_names, collapse = " + "), sep = " ~ "))
full_mod <- polr(formula = full_formula, data = data, method = "probit")
allsig_formula <- update(full_formula, ~ . - citric.acid - chlorides - total.sulfur.dioxide)
allsig_mod <- polr(formula = allsig_formula, data = data, method = "probit")
sur2<-surr_rsq(allsig_mod,full_mod,data)
sur2$surr_rsq

############
rsq(allsig_mod,full_mod,data,"McFadden")
rsq(allsig_mod,full_mod,data,"Surrogate")

########
ci<-surr_rsq_ci(sur2,B=1000)
