# library(sure)
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

set.seed(7)

NewFolder <- getwd()

# Functions ---------------------------------------------------------------
source(file = paste(NewFolder, "/raw/SurrogateR2.R", sep = ""))

# An example ---------------------------------------------------------------

wine_white <- read.csv(file = paste(NewFolder, "/raw/winequality-white.csv", sep = ""),
                       header = T, sep = ";")

# Manipulate data
data <- wine_white %>%
  dplyr::mutate(quality = as.ordered(quality))

# Find all covariates
x_names <- colnames(data)[!(colnames(data) %in% y)]

# Run full model
full_formula <- as.formula(paste(y, paste(x_names, collapse = " + "), sep = " ~ "))
full_mod <- polr(formula = full_formula, data = data, method = "probit")
summary(full_mod)

# Run a model with all significant covariates
allsig_formula <- update(full_formula, ~ . - citric.acid - chlorides - total.sulfur.dioxide)
allsig_mod <- polr(formula = allsig_formula, data = data, method = "probit")

# Calculate the surrogate R-squared of the full model, please check the arguments first!
R2_surr_full <- SurrogateR2(data = data,
                            formula = full_formula,
                            formula_full = full_formula,
                            which = "Surrogate", link = "probit", avg = 30)
R2_McF_full <- SurrogateR2(data = data,
                           formula = full_formula,
                           formula_full = full_formula,
                           which = "McFadden", link = "probit", avg = 30)
R2_MZ_full <- SurrogateR2(data = data,
                          formula = full_formula,
                          formula_full = full_formula,
                          which = "McK", link = "probit", avg = 30)

# Calculate the surrogate R-squared of a reduced model, please check the arguments first!
R2_surr_allsig <- SurrogateR2(data = data,
                              formula = allsig_formula,
                              formula_full = full_formula,
                              which = "Surrogate", link = "probit", avg = 30)
R2_McF_allsig <- SurrogateR2(data = data,
                             formula = allsig_formula,
                             formula_full = full_formula,
                             which = "McFadden", link = "probit", avg = 30)
R2_MZ_allsig <- SurrogateR2(data = data,
                            formula = allsig_formula,
                            formula_full = full_formula,
                            which = "McK", link = "probit", avg = 30)

# Combine the results together
R2_mods <-
  data.frame(R2_Surrogate=scales::percent(c(R2_surr_full, R2_surr_allsig), accuracy = 0.01),
             R2_MZ = scales::percent(c(R2_MZ_full, R2_MZ_allsig), accuracy = 0.01),
             R2_McF = scales::percent(c(R2_McF_full, R2_McF_allsig), accuracy = 0.01),
             row.names = c("Full", "AllSig"))
R2_mods

