#' A function to calculate the surrogate R-squared measure.
#'
#' @description It can provide the surrogate R-squared for a user specified model.
#' This function will generate an S3 object of surrogate R-squared measure that will
#' be called from other functions of this package. The generic S3 function `print`
#' is also developed to present the surrogate R-squared measure.
#' @param model A reduced model that needs to be investigated. The reported surrogate R-squared is for this reduced model.
#' @param full_model A full model that contains all of the predictors in the data set. This model object
#' should also contain the dataset for fitting the full model and the reduced model in the first argument.
#' @param avg.num The number of replication for the averaging of surrogate R-squared.
#' @param asym A logical argument whether use the asymptotic version of our surrogate R-squared.
#' More details are in the paper Liu et al. (2023).
#' @param ... Additional optional arguments.
#'
#' @return An object of class `"surr_rsq"` is a list containing the following components:
#' \item{`surr_rsq`}{the surrogate R-squared value;}
#' \item{`reduced_model`}{the reduced model under investigation. It should be a subset
#' of the full model;}
#' \item{`full_model`}{the full model used for generating the surrogate response. It should
#' have passed initial variable screening and model diagnostics (see Paper for reference);}
#' \item{`data`}{the dataset contains the response variable and all the predictors.}
#'
#' @references
#' Zhu, X., Liu, D., Lin, Z., Greenwell, B. (2022). SurrogateRsq: an R package for categorical
#' data goodness-of-fit analysis using the surrogate R-squared
#'
#' Liu, D., Zhu, X., Greenwell, B., & Lin, Z. (2023). A new goodness‐of‐fit measure for
#' probit models: Surrogate R2. British Journal of Mathematical and Statistical
#' Psychology, 76(1), 192-210.
#'
#' @importFrom PAsso surrogate
#' @importFrom stats update lm nobs quantile
#'
#' @examples
#' data("RedWine")
#'
#' full_formula <- as.formula(quality ~ fixed.acidity + volatile.acidity +
#' citric.acid+ residual.sugar + chlorides + free.sulfur.dioxide +
#' total.sulfur.dioxide + density + pH + sulphates + alcohol)
#'
#' full_mod <- polr(formula = full_formula,
#' data=RedWine, method  = "probit")
#'
#' select_model <- update(full_mod, formula. = ". ~ . - fixed.acidity -
#' citric.acid - residual.sugar - density")
#' surr_obj_sele_mod <- surr_rsq(model = select_model, full_model = full_mod,
#'                                data = RedWine, avg.num = 30)
#' print(surr_obj_sele_mod$surr_rsq, digits = 3)
#'
#' @export
#'
surr_rsq <-
  function(model,
           full_model,
           avg.num = 30,
           asym = FALSE, ...){
    # full_model_formula <- eval(full_model$call[[2]])
    # model_formula <- eval(model$call[[2]])

    #get the formula of reduced model and full model
    model_formula <- formula(model$terms)
    full_model_formula <- formula(full_model$terms)


    # Get set of predictors in reduced model and the predictors in full model
    reduc_vars <- names(model$coefficients)
    full_vars <- names(full_model$coefficients)

    # Get coefficients for asymptotic version
    p <- length(full_vars)
    coefs_full <- matrix(coef(full_model), nrow = p, ncol = 1)

    # Check if datasets from two model objects are the same!
    data <- checkDataSame(model = model, full_model = full_model)
    # data <- full_model$model

    # Get design matrix of reduced model
    x_reduc_cen <- scale(as.matrix(data[,reduc_vars]),
                         center = TRUE, scale = FALSE)
    x_full_cen <- scale(as.matrix(data[,full_vars]),
                        center = TRUE, scale = FALSE)

    n <- dim(x_full_cen)[1]

    if(all(reduc_vars %in% full_vars)){
      # make sure the set of predictors in reduced model is the subset of the predictors in full model
      if(model$method == full_model$method){
        # Fit models to ordinal response -----
        # fit_y_full <- glm(formula = formula_full, data = data, family = binomial(link = link))
        # This is for binary logistic regression

        # Surrogate with latent variables directly! -----
        # Generate surrogate response values

        if (asym == FALSE) {
          # Use the proposed way taking average to construct the surrogate R-squared
          res_s_temp <- rep(NA, times = avg.num)
          for (i in 1:avg.num) {
            # critical: generate surrogate response from the full model.
            data$s_full <- surrogate(full_model)
            # Generate surrogate from True Null hypothesis!
            fit_s <- lm(formula = update(model_formula, s_full ~ . ), data = data)

            res_s_temp[i] <- c(summary(fit_s)$r.squared)
          }
          res_s <- mean(res_s_temp)
        } else {
          # Use the asymptotic version of our surrogate R-squared
          numer <- t(coefs_full) %*% t(x_full_cen) %*% x_reduc_cen %*% solve(crossprod(x_reduc_cen), t(x_reduc_cen)) %*% x_full_cen %*% coefs_full

          fullbase <- t(coefs_full) %*% crossprod(x_full_cen) %*% coefs_full + n

          surr_asym <- numer/fullbase
          res_s <- surr_asym
          message("The asymptotic version of surrogate R-squared is used, no average is taken!")
        }

        return_list <-list("surr_rsq"      = res_s,
                           "reduced_model" = model,
                           "full_model"    = full_model,
                           "data"          = data)

        class(return_list) <- c("surr_rsq", class(return_list))

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
#' @keywords internal
print.surr_rsq <- function(x, digits = max(2, getOption("digits")-2), ...) {
  cat("------------------------------------------------------------------------ \n")
  cat("The surrogate R-squared of the model \n------------------------------------------------------------------------ \n",
      paste(format(formula(x$reduced_model$terms)), "\n"),
      "------------------------------------------------------------------------ \nis: \n", sep = "")

  temp <- format(round(x$surr_rsq, digits=max(2, (digits))),
                 digits = max(2, (digits)), ...)

  print.default(temp,
                print.gap = 2, na.print = "",
                quote = FALSE, ...)
}


#' @keywords internal
getDatafromModel <- function(object) {
  UseMethod("getDatafromModel")
}

getDatafromModel.polr <- function(object) {
  object$model
}

getDatafromModel.glm <- function(object) {
  # family_attr <- family(object)
  object$data
}

#' @keywords internal
checkDataSame <- function(model, full_model) {

  # Get the dataset from the model object
  dataRedu <- getDatafromModel(model)
  dataFull <- getDatafromModel(full_model)

  If_same_data <- all.equal(dataRedu,
                            dataFull[,names(dataRedu)],
                            check.attributes = FALSE)

  if (isTRUE(If_same_data) == FALSE) {
    warning("The datasets in two model objects are different. The dataset in 'full_model' is used. Use with cautions!")
  }
  # return the data
  return(dataFull)
}
