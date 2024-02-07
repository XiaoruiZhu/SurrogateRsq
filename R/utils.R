################################################################################
# Generic function for extracting the dataset from the model object
################################################################################

#' @title getDatafromModel
#' @description
#' get data matrix from the fitted model objects in different classes
#'
#' @keywords internal
getDatafromModel <- function(object) {
  UseMethod("getDatafromModel")
}

getDatafromModel.polr <- function(object) {
  object$model
}

getDatafromModel.glm <- function(object) {
  object$model
}

################################################################################
# Generic function for extracting coefficients from the model object
################################################################################

#' @title getCoefsfromModel
#' @description
#' get variables' names from the fitted model objects in different classes
#' @keywords internal
getCoefsfromModel <- function(object) {
  UseMethod("getCoefsfromModel")
}

getCoefsfromModel.polr <- function(object) {
  object$model$coefficients
}

getCoefsfromModel.glm <- function(object) {
  object$coefficients
}

################################################################################
# Generic function for check if datasets from reduced and full models are equal!
################################################################################

#' @keywords internal
checkDataSame <- function(model, full_model) {

  # Get the dataset from the model object
  dataRedu <- getDatafromModel(model)
  dataFull <- getDatafromModel(full_model)

  If_same_data <- all.equal(dataRedu,
                            dataFull[, names(dataRedu)],
                            check.attributes = FALSE)

  if (isTRUE(If_same_data) == FALSE) {
    warning("The two model objects should have the same dataset. The dataset in 'full_model' is used.")
  }
  # return the data
  return(dataFull)
}



################################################################################
# Generic function for extracting the name of the assumed distribution from a
# cumulative link model
################################################################################

#' @keywords internal
getDistributionName <- function(object) {
  UseMethod("getDistributionName")
}


#' @keywords internal
getDistributionName.clm <- function(object) {
  switch(object$link,
         "logit" = "logis",
         "probit" = "norm",
         "loglog" = "gumbel",
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}


#' @keywords internal
getDistributionName.glm <- function(object) {
  switch(object$family$link,
         "logit" = "logis",
         "probit" = "norm",
         # "loglog" = "gumbel",  # glm does not support this link function
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}


#' @keywords internal
getDistributionName.lrm <- function(object) {
  "logis"
}


#' @keywords internal
getDistributionName.orm <- function(object) {
  switch(object$family,
         "logistic" = "logis",
         "probit" = "norm",
         "loglog" = "gumbel",
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}


#' @keywords internal
getDistributionName.polr <- function(object) {
  switch(object$method,
         "logistic" = "logis",
         "probit" = "norm",
         "loglog" = "gumbel",
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}


#' @keywords internal
getDistributionName.vglm <- function(object) {
  switch(object@family@infos()$link,
         "logit" = "logis",
         "probit" = "norm",
         "loglog" = "gumbel",
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}
