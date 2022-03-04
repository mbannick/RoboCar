#' Makes a model class for the specified adjustment method
#' with settings for covariate randomization
#' scheme and vcovHC type.
#' 
#' @param adj_method Type of adjustment method
#' @param car_scheme Type of covariate adaptive randomization
#' @param vcovHC Type of heteroskedasticity-consistent SE's
.make.model <- function(adj_method, car_scheme, vcovHC) {
  
  model <- structure(
    list(
      car_scheme=car_scheme,
      vcovHC=vcovHC
    ), 
    class=c("LinModel", adj_method)
  )
  
  return(model)
}

#' Checks compatibility between the model settings and the data object.
#' 
#' @param model Object of class LinModel
#' @param data Object of class RoboDataLinear
.check.compatible_model <- function(model, data){
  
  errors <- c()
  cov_adjust <- any(class(model) %in% c("ANCOVA", "ANHECOVA"))

  if(cov_adjust){
    if(is.null(data$covariate)){
      errors <- c(errors, "Specified covariate adjustment, 
                           but provided no covariates.")
    }
  } else if(!cov_adjust) {
    if(!is.null(data$covariate)){
      warning("Specified ANOVA, but covariates provided. 
              Will not do covariate adjustment.")
    }
  } else {
    stop("Unrecognized model class.")
  }
  
  .return.error(errors)
}

#' Gets the vcovHC weights for the sample size and number of parameters
#' 
#' @param vcovHC vcovHC option
#' @param n Number of data points
#' @param p Number of coefficients
.get.vcovHC <- function(vcovHC, n, p){
  if(vcovHC == "HC0"){
    wgt <- 1
  } else if(vcovHC == "HC1") {
    wgt <- n / (n - p)
  } else if(vcovHC == "HC3") {
    wgt <- (n / (n - p)) ** 2
  } else {
    stop("Unrecognized vcovHC method.")
  }
  return(wgt)
}

#' Helper function for model fitting. Fits
#' without intercept on all variables in the dataset but
#' the first variables as treatment.
#' 
#' @importFrom stats lm
#' @param df Data frame with at least response and treatment variable
.fit <- function(df){
  mod <- lm(response ~ 0 + treat + ., data=df)
  return(mod)
}

#' Fits a linear model with settings based on model and data.
#' The linear model is not the "true" model, but it's the linear model
#' that will aid in adjustment for ANCOVA and ANHECOVA, and used
#' to estimate the treatment effects for ANOVA (equivalent to a sample)
#' mean per group.
#' 
#' @param model Object of class LinModel
#' @param data Object of class RoboDataLinear
#' @export
linmod <- function(model, data){
  UseMethod("linmod", model)
}

#' Fits a linear model to use in ANOVA
#' 
#' @inheritParams linmod
#' @exportS3Method RoboCar::linmod
linmod.ANOVA <- function(model, data){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  mod <- .fit(df)
  return(mod)
}

#' Fits a linear model to use in ANCOVA
#' 
#' @inheritParams linmod
#' @exportS3Method RoboCar::linmod
linmod.ANCOVA <- function(model, data){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  df <- cbind(df, data$covariate)
  df <- cbind(df, data$strata)
  mod <- .fit(df)
  return(mod)
}

#' Fits a linear model to use in ANHECOVA
#' 
#' @inheritParams linmod
#' @exportS3Method RoboCar::linmod
linmod.ANHECOVA <- function(model, data){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  df <- cbind(df, data$covariate)
  df <- cbind(df, data$strata)
  mod <- .fit(df)
  return(mod)
}

#' Generic function for linear adjustment
#'
#' @param model Object of class LinModel
#' @param data Object of class RoboDataLinear
#' @export
linadjust <- function (model, data) {
  UseMethod("linadjust", model)
}

#' Perform linear model adjustment using the ANOVA adjustment method
#' 
#' @inheritParams linadjust
#' @param model Object of class ANOVA
#' @exportS3Method RoboCar::linadjust
linadjust.ANOVA <- function(model, data){
  
  vcov.wgt <- .get.vcovHC(vcovHC=model$vcovHC, n=data$n, p=data$k)
  mod <- linmod(model, data)
  
  return(list())
}

#' Perform linear model adjustment using the ANCOVA adjustment method
#'
#' @inheritParams linadjust
#' @param model Object of class ANCOVA
#' @exportS3Method RoboCar::linadjust
linadjust.ANCOVA <- function(model, data){
  
  vcov.wgt <- .get.vcovHC(vcovHC=model$vcovHC, n=data$n, p=data$k)
  mod <- linmod(model, data)
  
  return(list())
}

#' Perform linear model adjustment using the ANHECOVA adjustment method
#' 
#' @inheritParams linadjust
#' @param model Object of class ANHECOVA
#' @exportS3Method RoboCar::linadjust
linadjust.ANHECOVA <- function(model, data){
  
  vcov.wgt <- .get.vcovHC(vcovHC=model$vcovHC, n=data$n, p=data$k)
  mod <- linmod(model, data)
  
  return(list())
}