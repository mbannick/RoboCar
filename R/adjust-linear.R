#' Fits a linear model with settings based on model and data.
#' The linear model is not the "true" model, but it's the linear model
#' that will aid in adjustment for ANCOVA and ANHECOVA, and used
#' to estimate the treatment effects for ANOVA (equivalent to a sample)
#' mean per group.
#' 
#' @param model Object of class LinModel
#' @param data Object of class RoboDataLinear
linmod <- function(model, data, family, center=TRUE){
  UseMethod("linmod", model)
}

#' Fits a linear model to use in ANOVA
#' 
#' @inheritParams linmod
linmod.ANOVA <- function(model, data, family=gaussian, center=TRUE){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  mod <- glm(response ~ 0 + treat, data=df, family=family)
  return(mod)
}

#' Fits a linear model to use in ANCOVA
#' 
#' @inheritParams linmod
linmod.ANCOVA <- function(model, data, family=gaussian, center=TRUE){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  dmat <- .get.dmat(data, model$adj_vars)
  if(center) dmat <- .center.dmat(dmat)
  df <- cbind(df, dmat)
  if(center){
    mod <- glm(response ~ 0 + treat + ., data=df, family=family)
  } else {
    mod <- glm(response ~ 1 + treat + ., data=df, family=family)
  }
  return(mod)
}

#' Fits a linear model to use in ANHECOVA
#' 
#' @inheritParams linmod
linmod.ANHECOVA <- function(model, data, family=gaussian, center=TRUE){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  dmat <- .get.dmat(data, model$adj_vars)
  if(center) dmat <- .center.dmat(dmat)
  df <- cbind(df, dmat)
  if(center){
    mod <- glm(response ~ 0 + treat:., data=df, family=family)
  } else {
    mod <- glm(response ~ 1 + treat:., data=df, family=family)
  }
  return(mod)
}

#' Fits a custom GLM model using the user-specified
#' formula.
#' 
#' @inheritParams linmod
linmod.CUSTOM <- function(model, data, family=gaussian, center=TRUE){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  dmat <- .get.dmat(data, model$adj_vars)
  if(center) dmat <- .center.dmat(dmat)
  df <- cbind(df, dmat)
  mod <- glm(as.formula(data$formula), data=df, family=family)
  return(mod)
}

#' Perform adjustment for linear models, including ANOVA, ANCOVA,
#' and ANHECOVA, with or without covariate-adaptive randomization.
#' 
#' @param model A LinModel object and of class ANOVA, ANCOVA, or ANHECOVA
#' @param data A RobinDataLinear object
adjust.LinModel <- function(model, data){

  # Fit a model with the settings in model
  mod <- linmod(model, data)

  # Get the simple randomization variance and adjust if necessary
  asympt.variance <- vcov_car(model, data, mod)
  vcov_wt <- .get.vcovHC(model$vcovHC, n=data$n, p=mod$rank)
  variance <- asympt.variance * vcov_wt / data$n
  
  # Extract estimates and create results data
  est <- coef(mod)[1:data$k]
  result <- format.results(data$treat_levels, est, variance)
  
  return(
    structure(
      class="LinModelResult",
      list(result=result, varcov=variance, settings=model, 
           data=data, mod=mod)
    )
  )
}
