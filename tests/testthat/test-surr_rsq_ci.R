test_that("test surr_rsq_ci", {
  # library(SurrogateRsq)
  # data("RedWine")

  # full_formula <- as.formula("quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol")

  # fullmodel <- polr(formula = full_formula,data=RedWine, method  = "probit")

  # select_model <- update(fullmodel, formula. = ". ~ . - fixed.acidity -
  # citric.acid - residual.sugar - density")

  # surr_rsq_select <- surr_rsq(select_model, fullmodel, avg.num = 30)

  # BS_fullmodel <- update(fullmodel, data = RedWine[sample(1:dim(RedWine)[1], dim(RedWine)[1], replace = T), ])
  # summary(fullmodel)
  # summary(BS_fullmodel)

  # CI_select <- surr_rsq_ci(surr_rsq_select, alpha = 0.05, B = 1000) # Not run, it takes time.
  # CI_select$surr_rsq
  # CI_select$surr_rsq_ci
  # hist(CI_select$surr_rsq_BS)
  # CI_select$data

  # expect_true(as.numeric(CI_select$surr_rsq_ci[2,1]) <= CI_select$surr_rsq, TRUE)
  # expect_true(as.numeric(CI_select$surr_rsq_ci[2,2]) >= CI_select$surr_rsq, TRUE)

})

test_that("test surr_rsq_ci with parallel = TRUE", {
  # Skips
  # skip_on_cran()
  # skip_if_not_installed("MASS")
  # skip_if_not_installed("stats")
  # skip_if_not_installed("foreach")
  # skip_if_not_installed("scales")
  # skip_if_not_installed("progress")
  #
  # library(SurrogateRsq)
  # data("RedWine")
  # RedWine2 <- subset(RedWine, total.sulfur.dioxide <= 200)
  #
  # naive_model <- polr(quality ~ ., data = RedWine2, method = "probit")
  #
  # select_model <-
  #   update(naive_model,
  #          formula. =
  #            ". ~ . - fixed.acidity - citric.acid - residual.sugar - density")
  #
  # mod_full <-
  #   update(select_model, formula. = ". ~ . + I(sulphates^2) + I(sulphates^3)")
  #
  # system.time(
  #   surr_obj_mod_full <-
  #     surr_rsq(model      = mod_full,
  #              full_model = mod_full,
  #              avg.num    = 30)
  # )
  # system.time(
  #   surr_obj_mod_full <-
  #     surr_rsq(model      = mod_full,
  #              full_model = mod_full,
  #              asym = TRUE,
  #              avg.num    = 30)
  # )
  # set.seed(77)
  #
  # # Test no parallel progression bar--------------------------------------------------------
  #
  # system.time(
  #   full_mod_rsq_ci <-
  #     surr_rsq_ci(object = surr_obj_mod_full,
  #                 alpha    = 0.05,
  #                 B        = 2000,
  #                 parallel = FALSE)
  # )
  #
  # # user  system elapsed
  # # 378.679  17.147 395.301
  #
  # # test parallel of surr_rsq_ci() function with registerDoSNOW progression bar ----------------------------
  # library(doParallel); library(progress)
  # library(doSNOW)
  # numCores <- detectCores()  # Not too aggressive!
  # cl <- makeCluster(numCores)
  # # doParallel does not support the progress multicore option. Therefore, a progress bar will NOT display if registerDoParallel is used instead of registerDoSNOW.
  # registerDoSNOW(cl) # on Mac or Linux
  # # registerDoParallel(cl) # Win
  #
  # system.time(
  #   full_mod_rsq_ci <-
  #     surr_rsq_ci(object = surr_obj_mod_full,
  #                 alpha    = 0.05,
  #                 B        = 2000,
  #                 parallel = TRUE)
  # )
  # full_mod_rsq_ci
  # # user  system elapsed
  # # 0.873   0.223  35.978
  #
  # # test asym version w/o parallel of surr_rsq_ci()  ------------------------
  #
  # system.time(
  #   full_mod_rsq_ci_asym <-
  #     surr_rsq_ci(object = surr_obj_mod_full,
  #                 alpha    = 0.05,
  #                 B        = 2000,
  #                 asym     = TRUE,
  #                 parallel = FALSE)
  # )
  # # user  system elapsed
  # # 143.079   6.689 149.427
  # full_mod_rsq_ci_asym
  #
  #
  # # test asym version w parallel of surr_rsq_ci()  ------------------------
  # system.time(
  #   full_mod_rsq_ci_asym_para <-
  #     surr_rsq_ci(object = surr_obj_mod_full,
  #                 alpha    = 0.05,
  #                 B        = 2000,
  #                 asym     = TRUE,
  #                 parallel = TRUE)
  # )
  # # user  system elapsed
  # # 0.818   0.181  21.057
  # full_mod_rsq_ci_asym_para

})
