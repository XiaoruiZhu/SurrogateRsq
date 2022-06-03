test_that("test surr_rsq which==Surrogate", {

  library(R2Cate)
  library(dplyr); library(MASS)

  # Read the csv file of the white wine data
  wine_white <- read.csv(file = "raw/winequality-white.csv",header = T, sep = ";")
  data <- wine_white %>% mutate(quality = as.ordered(quality))
  y <- "quality"

  # Find all covariates
  x_names <- colnames(data)[!(colnames(data) %in% y)]

  full_formula <- as.formula(paste(y, paste(x_names, collapse = " + "), sep = " ~ "))

  full_mod <- polr(formula = full_formula,
                   data = data,
                   method = "probit")

  allsig_formula <- update(full_formula, ~ . - citric.acid - chlorides - total.sulfur.dioxide)

  allsig_mod <- polr(formula = allsig_formula,
                     data = data,
                     method = "probit")

  test1 <- rsq(model = allsig_mod,
               data = data,
               full_model,
               which = "Surrogate",
               avg.num = 30)

  sur2 <- surr_rsq(model = allsig_mod,
                   full_model = full_mod,
                   data = data,
                   avg.num = 100)
  expect_type(sur2, "list")
  expect_lt(object = sur2$surr_rsq, expected = 0.33)
})
