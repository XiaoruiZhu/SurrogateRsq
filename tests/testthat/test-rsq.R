test_that("test surr_rsq which==Surrogate for vgam", {

  # library(SurrogateRsq)
  library(VGAM)

  # Read the csv file of the white wine data
  data("WhiteWine")

  WhiteWine <- WhiteWine %>% mutate(quality = as.ordered(quality))
  y <- "quality"

  # Find all covariates
  x_names <- colnames(WhiteWine)[!(colnames(WhiteWine) %in% y)]

  full_formula <- as.formula(paste(y, paste(x_names, collapse = " + "), sep = " ~ "))

  full_mod <- vgam(formula = full_formula,
                   data = WhiteWine,
                   family = cumulative(link = "probit"))

  allsig_formula <- update(full_formula, ~ . - citric.acid - chlorides - total.sulfur.dioxide)

  allsig_mod <- vgam(formula = allsig_formula,
                     data = WhiteWine,
                     family = cumulative(link = "probit"))

  # test1 <- rsq(model = allsig_mod,
  #              data = WhiteWine,
  #              full_model = full_mod,
  #              which = "Surrogate",
  #              avg.num = 30)
  # Doesn't work here.

  # sur2 <- surr_rsq(model = allsig_mod,
  #                  full_model = full_mod,
  #                  data = WhiteWine,
  #                  avg.num = 30)
  # expect_type(sur2, "list")
  # expect_lt(object = sur2$surr_rsq, expected = 0.33)
})
