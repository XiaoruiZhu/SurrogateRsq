test_that("test surr_rsq", {
  # library(SurrogateRsq)
  library(MASS)
  library(dplyr)
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

  allsig_mod <- polr(formula = allsig_formula,
                     data = WhiteWine,
                     method = "probit")

  sur2 <- surr_rsq(model = allsig_mod,
                   full_model = full_mod,
                   # data = WhiteWine,
                   avg.num = 100)
  expect_type(sur2, "list")
  expect_lt(object = sur2$surr_rsq, expected = 0.33)
})

test_that("test surr_rsq which==Surrogate for plor", {
  library(dplyr)
  data("RedWine")

  full_formula <- as.formula(quality ~ fixed.acidity + volatile.acidity +
                               citric.acid+ residual.sugar + chlorides + free.sulfur.dioxide +
                               total.sulfur.dioxide + density + pH + sulphates + alcohol)

  full_mod <- polr(formula = full_formula,
                   data=RedWine, method  = "probit")

  select_model <- update(full_mod, formula. = ". ~ . - fixed.acidity -
                       citric.acid - residual.sugar - density")

  # all.equal(select_model$model, (full_mod$model[,names(select_model$model)]),
  #           check.attributes = FALSE)

  surr_obj_sele_mod <- surr_rsq(model = select_model,
                                full_model = full_mod,
                                avg.num = 30)

  # unname(DescTools::PseudoR2(select_model, which = "McFadden"))

  expect_type(surr_obj_sele_mod, "list")
})

test_that("test surr_rsq which==Surrogate for glm", {
  library(dplyr)
  data("RedWine")
  RedWine$quality.bin <- as.factor(ifelse(RedWine$quality>=6, 1, 0))

  full_formula_bin <- as.formula(quality.bin ~ fixed.acidity + volatile.acidity +
                               citric.acid+ residual.sugar + chlorides + free.sulfur.dioxide +
                               total.sulfur.dioxide + density + pH + sulphates + alcohol)

  full_mod_glm <- glm(formula = full_formula_bin,
                      data=RedWine, family = binomial(link = "logit"))
  # summary(full_mod_glm)

  select_model_glm <- update(full_mod_glm, formula. = ". ~ . - fixed.acidity -
                         residual.sugar - density - pH")

  # all.equal(select_model$model, (full_mod$model[,names(select_model$model)]),
  #           check.attributes = FALSE)

  # both are logit model
  surr_obj_sele_mod_bin <- surr_rsq(model = select_model_glm,
                                full_model = full_mod_glm,
                                avg.num = 30)

  # unname(DescTools::PseudoR2(select_model_glm, which = "McFadden"))

  # both are probit model, glm probit does not accept factor binary response
  RedWine$quality.bin <- ifelse(RedWine$quality>=6, 1, 0)

  full_mod_glm_probit <- glm(formula = full_formula_bin,
                      data=RedWine, family = binomial(link = "probit"))
  # summary(full_mod_glm_probit)
  # summary(full_mod_glm)

  select_model_glm_probit <- update(full_mod_glm_probit, formula. = ". ~ . - fixed.acidity -
                         residual.sugar - density - pH")

  surr_obj_sele_mod_bin_probit <- surr_rsq(model = select_model_glm_probit,
                                    full_model = full_mod_glm_probit,
                                    avg.num = 30)

  # unname(DescTools::PseudoR2(select_model_glm_probit, which = "McFadden"))

  # One is probit model one is logit model: it generates different data, models are not checked.
  # surr_obj_sele_mod_bin_probit2 <- surr_rsq(model = select_model_glm_probit,
                                           # full_model = full_mod_glm,
                                           # avg.num = 30)

  expect_type(surr_obj_sele_mod_bin, "list")
})
