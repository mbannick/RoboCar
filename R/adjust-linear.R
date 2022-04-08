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
  mod <- lm(response ~ 0 + treat, data=df)
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
  dmat <- .get.dmat(data, model$adj_vars)
  df <- cbind(df, dmat)
  mod <- lm(response ~ 0 + treat + ., data=df)
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
  dmat <- .get.dmat(data, model$adj_vars)
  df <- cbind(df, dmat)
  mod <- lm(response ~ 0 + treat + treat:., data=df)
  return(mod)
}

#' Perform linear model adjustment using the ANOVA adjustment method
#' 
#' @inheritParams adjust
#' @param model Object of class ANOVA
#' @exportS3Method RoboCar::adjust
adjust.ANOVA <- function(model, data){
  
  vcov.wgt <- .get.vcovHC(vcovHC=model$vcovHC, n=data$n, p=data$k)
  mod <- linmod(model, data)
  
  return(list())
}

#' Perform linear model adjustment using the ANCOVA adjustment method
#'
#' @inheritParams linadjust
#' @param model Object of class ANCOVA
#' @exportS3Method RoboCar::linadjust
adjust.ANCOVA <- function(model, data){
  
  vcov.wgt <- .get.vcovHC(vcovHC=model$vcovHC, n=data$n, p=data$k)
  mod <- linmod(model, data)
  
  return(list())
}

#' Perform linear model adjustment using the ANHECOVA adjustment method
#' 
#' @inheritParams adjust
#' @param model Object of class ANHECOVA
#' @exportS3Method RoboCar::linadjust
adjust.ANHECOVA <- function(model, data){
  
  vcov.wgt <- .get.vcovHC(vcovHC=model$vcovHC, n=data$n, p=data$k)
  mod <- linmod(model, data)
  
  return(list())
}