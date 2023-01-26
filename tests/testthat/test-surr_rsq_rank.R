test_that("test surr_rsq_rank", {
  # library(SurrogateRsq)
  library(MASS)

  # Read the csv file of the white wine data
  data("WhiteWine")

  WhiteWine <- WhiteWine %>% mutate(quality = as.ordered(quality))
  y <- "quality"

  # Find all covariates
  x_names <- colnames(WhiteWine)[!(colnames(WhiteWine) %in% y)]

  full_formula <- as.formula(paste(y, paste(x_names, collapse = " + "), sep = " ~ "))

  full_mod <- polr(formula = full_formula,
                   data = WhiteWine,
                   method = "probit")

  allsig_formula <- update(full_formula, ~ . - citric.acid - chlorides - total.sulfur.dioxide)

  sele_formula <- update(full_formula, ~ . - total.sulfur.dioxide - chlorides - density - citric.acid)

  allsig_mod <- polr(formula = allsig_formula,
                     data = WhiteWine,
                     method = "probit")
  sele_mod <- polr(formula = sele_formula,
                     data = WhiteWine,
                     method = "probit")

  sur2 <- surr_rsq(model = allsig_mod,
                   full_model = full_mod,
                   data = WhiteWine,
                   avg.num = 100)

  rank_tab_sur2 <- surr_rsq_rank(object  = sur2,
                                 data    = WhiteWine,
                                 avg.num = 30)

  sur3 <- surr_rsq(model = sele_mod,
                   full_model = sele_mod,
                   data = WhiteWine,
                   avg.num = 100)

  rank_tab_sur3 <- surr_rsq_rank(object  = sur3,
                                 data    = WhiteWine,
                                 avg.num = 30)

  expect_true(!is.null(rank_tab_sur2), "list")
  expect_true(!is.null(rank_tab_sur3), "list")

})

test_that("test surr_rsq_rank example", {
  library(MASS)
  data("WhiteWine")

  sele_formula <- as.formula(quality ~ fixed.acidity + volatile.acidity +
                               residual.sugar +  + free.sulfur.dioxide +
                               pH + sulphates + alcohol)

  sele_mod <- polr(formula = sele_formula,
                   data = WhiteWine,
                   method = "probit")

  sur1 <- surr_rsq(model = sele_mod,
                   full_model = sele_mod,
                   data = WhiteWine,
                   avg.num = 100)

  rank_tab_sur1 <- surr_rsq_rank(object  = sur1,
                                 data    = WhiteWine,
                                 avg.num = 30)
  expect_true(!is.null(rank_tab_sur1), "list")
})
