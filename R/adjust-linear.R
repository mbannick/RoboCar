#' Fits a linear model with settings based on model and data.
#' The linear model is not the "true" model, but it's the linear model
#' that will aid in adjustment for ANCOVA and ANHECOVA, and used
#' to estimate the treatment effects for ANOVA (equivalent to a sample)
#' mean per group.
#' 
#' @param model Object of class LinModel
#' @param data Object of class RoboDataLinear
#' @export
linmod <- function(model, data, family){
  UseMethod("linmod", model)
}

#' Fits a linear model to use in ANOVA
#' 
#' @inheritParams linmod
#' @exportS3Method RoboCar::linmod
linmod.ANOVA <- function(model, data, family=gaussian){
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
#' @exportS3Method RoboCar::linmod
linmod.ANCOVA <- function(model, data, family=gaussian){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  dmat <- .get.dmat(data, model$adj_vars)
  df <- cbind(df, dmat)
  mod <- glm(response ~ 0 + treat + ., data=df, family=family)
  return(mod)
}

#' Fits a linear model to use in ANHECOVA
#' 
#' @inheritParams linmod
#' @exportS3Method RoboCar::linmod
linmod.ANHECOVA <- function(model, data, family=gaussian){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  dmat <- .get.dmat(data, model$adj_vars)
  df <- cbind(df, dmat)
  mod <- glm(response ~ 0 + treat + treat:., data=df, family=family)
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
  asympt.variance <- vcov_sr(model, data, mod)
  variance <- asympt.variance / data$n
  # TODO: Adjust variance for Z, if required
  
  # Extract estimates and create results data
  est <- coef(mod)[1:data$k]
  result <- tibble(
    treat=data$treat_levels,
    estimate=c(est),
    se=diag(variance**0.5)
  )
  
  # Compute p-values based on the correct variance estimates
  result <- result %>% mutate(
    `pval (2-sided)`=2*pnorm(abs(estimate/se), lower.tail=F)
  )
  
  return(list(result=result, varcov=variance, settings=model))
}
