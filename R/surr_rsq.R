#' A function to calculate the surrogate R-squared measure.
#'
#' @param model A reduced model that needs to be investigated. The reported surrogate R-square is for this reduced model.
#' @param full_model A full model that contains all of the predictors in the data set.
#' @param data A data set contains the categorical responses, predictors.
#' @param avg.num The number of replication for the averaging of surrogate R-square.
#' @param ... Additional optional arguments.
#'
#' @return An object of class \code{"surr_rsq"} is a list containing the following components:
#' \item{\code{surr_rsq}}{the surrogate R-square value;}
#' \item{\code{reduced_model}}{the reduced model under investigation. It should be a subset
#' of the full model;}
#' \item{\code{full_model}}{the full model used for generating the surrogate response. It should
#' have passed initial variable screening and model diagnostics (see Paper for reference);}
#' \item{\code{data}}{the dataset contains the response variable and all the predictors.}
#'
#' @references
#' Zhu, X., Liu, D., Lin, Z. (2022). SurrRsq: an R package for evaluating goodness of fit using
#' surrogate R-squared
#'
#' @importFrom PAsso surrogate
#' @importFrom stats update lm nobs quantile
#'
#' @examples
#' data("RedWine")
#'
#' full_formula <- as.formula(quality ~ fixed.acidity + volatile.acidity + citric.acid
#' + residual.sugar + chlorides + free.sulfur.dioxide +
#' total.sulfur.dioxide + density + pH + sulphates + alcohol)
#'
#' fullmodel <- polr(formula = full_formula,data=RedWine, method  = "probit")
#'
#' select_model <- update(fullmodel, formula. = ". ~ . - fixed.acidity
#' - citric.acid - residual.sugar - density")
#'
#' surr_rsq(select_model, fullmodel, data=RedWine, avg.num=500)
#'
#' @export
#'
surr_rsq <-
  function(model,
           full_model,
           data,
           avg.num = 30, ...){
    # full_model_formula <- eval(full_model$call[[2]])
    # model_formula <- eval(model$call[[2]])
    full_model_formula <- formula(full_model$terms)
    model_formula <- formula(model$terms)

    #get the formula of reduced model and full model
    if(all(names(model$coefficients) %in% names(full_model$coefficients))){
      #make sure the set of predictors in reduced model is the subset of the predictors in full model
      if(model$method == full_model$method){
        # Fit models to ordinal response -----
        # fit_y_full <- glm(formula = formula_full, data = data, family = binomial(link = link))
        # This is for binary logistic regression
        # Surrogate with latent variables directly! -----
        # Generate surrogate response values
        # set.seed(1521)
        res_s_temp <- rep(NA, times = avg.num)
        for (i in 1:avg.num) {
          data$s_full <- surrogate(full_model)
          #import PAsso surrogate

          # Second approach: generate surrogate from True Null hypothesis!
          fit_s <- lm(formula = update(model_formula, s_full ~ . ), data = data)

          res_s_temp[i] <- c(summary(fit_s)$r.squared)
        }
        res_s <- mean(res_s_temp)
        return_list <-list("surr_rsq"      = res_s,
                           "reduced_model" = model,
                           "full_model"    = full_model,
                           "data"          = data)

        class(return_list) <- c("surr_rsq", class(return_list))

        #print.object(s3 class)
        #add class for the return list
        #reduced model, data=NULL
        #data?
        return(return_list)
      }
      else{
        stop("The types of model and full model are different. Please check your models")
      }
    }
    else{
      stop("Full model does not contain all the variables in the reduced model.")
    }
  }


#' @title Print surrogate R-squared measure
#' @param x A surr_rsq object for printing out results.
#'
#' @param digits A default number to specify decimal digit values.
#' @param ... Additional optional arguments.
#'
#' @name print
#' @method print surr_rsq
#'
#' @return Print surrogate R-squared measure of a surr_rsq object
#'
#' @importFrom stats formula
#'
#' @export
#' @examples
#' # See surr_rsq for the example.
#'
print.surr_rsq <- function(x, digits = max(2, getOption("digits")-2), ...) {
  cat("------------------------------------ \n")
  cat("The surrogate R-squared of the model \n------------------------------------ \n",
      paste(format(formula(x$reduced_model$terms)), "\n"),
      "------------------------------------ \nis: \n", sep = "")
  # x$corr[lower.tri(x$corr)] <- NA

  # print.default(format(x$corr, digits = max(2, (digits))),
  # print.gap = 2, na.print = "",
  # quote = FALSE, ...)

  temp <- format(round(x$surr_rsq, digits=max(2, (digits))),
                 digits = max(2, (digits)), ...)

  print.default(temp,
                print.gap = 2, na.print = "",
                quote = FALSE, ...)
}
