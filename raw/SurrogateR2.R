# Functions ---------------------------------------------------------------


rtrunc <- function (n, spec, a = -Inf, b = Inf, ...) {
  qtrunc(runif(n, min = 0, max = 1), spec, a = a, b = b, ...)
}
qtrunc <- function (p, spec, a = -Inf, b = Inf, ...) {
  tt <- p
  G <- get(paste("p", spec, sep = ""), mode = "function")
  Gin <- get(paste("q", spec, sep = ""), mode = "function")
  G.a <- G(a, ...)
  G.b <- G(b, ...)
  pmin(pmax(a, Gin(G(a, ...) + p * (G(b, ...) - G(a, ...)), ...)), b)
}


#' SurrogateR2
#'
#' @param data a data set contains the categorical responses, predictors.
#' @param formula a reduced model that needs to be investigated. The reported surrogate R-square is for this reduced model.
#' @param formula_full a full model for the construction of surrogate R-square. The surrogate responses need to be generated from this full model.
#' @param which an argument to specify which pseudo R-square to calculate.
#' @param link the link function for the binary logistic or probit model.
#' @param avg the number of replication for the averaging of surrogate R-square.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
SurrogateR2 <-
  function(data,
           formula,
           formula_full,
           which = c("Surrogate", "McFadden", "McKelveyZavoina",
                     "CoxSnell", "Nagelkerke", "Tjur"),
           link = c("logit", "probit"), avg = 30, ...) {

    # Test header:
    # data = inSample; formula = formula; formula_full = formula_full;
    # which = "Surrogate"; link = "logit"; avg = 30

    # Match the R-square method want to use!
    which <- match.arg(which)
    link <- match.arg(link)

    # Fit models to ordinal response -----
    # fit_y_red <- glm(formula = formula, data = data, family = binomial(link = link)) # This is for binary logistic regression

    # This is for the multilevel response probit model
    fit_y_red <- polr(formula = formula, data = data, method = link)

    if (which == "Surrogate") {

      # Fit models to ordinal response -----
      # fit_y_full <- glm(formula = formula_full, data = data, family = binomial(link = link)) # This is for binary logistic regression

      # This is for the multilevel response probit model
      fit_y_full <- polr(formula = formula_full, data = data, method = link)

      # Surrogate with latent variables directly! -----
      # Generate surrogate response values
      # set.seed(1521)
      res_s_temp <- rep(NA, times = avg)
      for (i in 1:avg) {
        data$s_full <- PAsso::surrogate(fit_y_full)

        # Second approach: generate surrogate from True Null hypothesis!
        fit_s <- lm(formula = update(formula, s_full ~ . ), data = data)

        res_s_temp[i] <- c(summary(fit_s)$r.squared)
      }
      res_s <- mean(res_s_temp)
      res_s

    } else if (which == "McFadden") {
      unname(DescTools::PseudoR2(fit_y_red, which = "McFadden"))
    } else { # "McKelveyZavoina"

      if (inherits(fit_y_red, what = "polr")) {
        sse <- sum((fit_y_red$lp - mean(fit_y_red$lp)) ^ 2)
        return(sse / (sse + nobs(fit_y_red)))
      } else if (inherits(fit_y_red, what = "glm")) {
        sse <- sum((fit_y_red$linear.predictors - mean(fit_y_red$linear.predictors)) ^ 2)
        return(sse / (sse + nobs(fit_y_red)))
      } else {
        stop(
          "Only objects of class \"polr\" or \"glm\" are currently supported.", call. = FALSE
        )
      }
    }
}


