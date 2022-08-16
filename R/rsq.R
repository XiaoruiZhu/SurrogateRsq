#' A function to calculate most of the available pseudo R-squared measures
#'
#' @description A function to include most of the available pseudo R-squared measures including Surrogate R-squared, McFadden R-squared, McKelveyZavoina R-squared, CoxSnell R-squared, Nagelkerke R-squared, and Tjur R-squared.
#' @param model A reduced or working model that needs to be investigated. The reported surrogate
#'  R-square is for this reduced model.
#' @param full_model A full model that needs to be specified for surrogate R-square approach.
#' @param data A data set contains the categorical responses and all necessary predictors
#' (including all the predictors in the \code{full_model}).
#' @param which An argument to specify which pseudo R-squared to calculate. This package supports
#' surrogate R-squared, McFadden R-squared, McKelveyZavoina R-squared,
#' CoxSnell R-squared, Nagelkerke R-squared, and Tjur R-squared.
#' @param avg.num The number of replication for the averaging of surrogate R-square.
#' @param ... Additional optional arguments.
#'
#' @return An object of class \code{"surr_rsq"} when \code{which = "Surrogate"}, or a R-squared value
#' when other pseudo R-squared are specified.
#'
#' @examples
#' data("RedWine")
#'
#' full_formula <- as.formula(quality ~ fixed.acidity + volatile.acidity + citric.acid
#' + residual.sugar + chlorides + free.sulfur.dioxide +
#' total.sulfur.dioxide + density + pH + sulphates + alcohol)
#'
#' model <- polr(formula = full_formula, data=RedWine, method  = "probit")
#'
#' rsq(model=model, data=RedWine, which="McKelveyZavoina")
#'
#' @export
#'
#' @importFrom DescTools PseudoR2
#'
rsq <-
  function(model,
           full_model = NULL,
           data,
           which = c("Surrogate", "McFadden", "McKelveyZavoina",
                     "CoxSnell", "Nagelkerke", "Tjur"),
           avg.num = 30, ...) {
    which <- match.arg(which)

    if (which == "Surrogate") {

      if (is.null(full_model)) {
        # Need to revise:
        # x_names <- colnames(data)[!(colnames(data) %in% y)]
      }

      surr_rsq(model = model,
               full_model = full_model,
               data = data,
               avg.num = avg.num)

    } else if (which == "McFadden") {
      unname(PseudoR2(model, which = "McFadden"))
    } else { # "McKelveyZavoina"

      if (inherits(model, what = "polr")) {
        sse <- sum((model$lp - mean(model$lp)) ^ 2)
        return(sse / (sse + nobs(model)))
      } else if (inherits(model, what = "glm")) {
        sse <- sum((model$linear.predictors - mean(model$linear.predictors)) ^ 2)
        return(sse / (sse + nobs(model)))
      } else {
        stop(
          "Only objects of class \"polr\" or \"glm\" are currently supported.", call. = FALSE
        )
      }
    }
  }



