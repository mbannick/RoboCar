.x.exist.warn <- function() warning("Covariates specified, but no adjustment desired. Ignoring them.")
.z.exist.warn <- function() warning("Strata specified, but no adjustment desired. Ignoring them.")
.z.exist.warn.sr <- function() warning("Strata specified, but simple randomization chosen. Ignoring Z in adjustment.")

.x.miss.warn <- function() warning("Covariates not specified, but adjustment desired. Changing method to ANOVA.")
.z.miss.err <- function() stop("No strata specified, but covariate-adaptive randomization desired. Please provide strata.")

.car.min.err <- function() stop("Minimization is not compatible with desired adjustment method. Please use ANHECOVA instead.")

.anova.logic <- function(x_exists, z_exists, car_scheme, cov_strata){
  
  method <- "ANOVA"
  adj_se_z <- FALSE
  adj_vars <- NULL
  
  if(car_scheme == "SR"){
    if(x_exists) .x.exist.warn()
    if(z_exists) .z.exist.warn()
  }
  if(car_scheme == "minimization"){
    .car.min.err()
  }
  if(car_scheme == "applicable"){
    if(z_exists){
      if(x_exists) .x.exist.warn()
      adj_se_z <- TRUE
    } else {
      .z.miss.err()
    }
  }
  return(list(
    method=method,
    adj_se_z=adj_se_z,
    adj_vars=adj_vars
  ))
}
.ancova.logic <- function(x_exists, z_exists, car_scheme, cov_strata){
  
  method <- "ANCOVA"
  adj_se_z <- FALSE
  adj_vars <- NULL
  
  if(car_scheme == "SR"){
    if(z_exists) .z.exist.warn.sr()
    if(x_exists){
      adj_vars <- "x"
    } else {
      .x.miss.warn()
      method <- "ANOVA"
    }
  }
  if(car_scheme == "minimization"){
    .car.min.err()
  }
  if(car_scheme == "applicable"){
    if(z_exists){
      adj_se_z <- TRUE
      if(cov_strata){
        if(x_exists){
          adj_vars <- "joint_x_z"
        } else {
          adj_vars <- "z"
        }
      } else {
        if(x_exists){
          adj_vars <- "x"
        } else {
          method <- "ANOVA"
        }
      }
    } else {
      .z.miss.err()
    }
  }
  return(list(
    method=method,
    adj_se_z=adj_se_z,
    adj_vars=adj_vars
  ))
}
.anhecova.logic <- function(x_exists, z_exists, car_scheme, cov_strata){
  
  method <- "ANHECOVA"
  adj_se_z <- FALSE
  adj_vars <- NULL
  
  if(car_scheme == "SR"){
    if(z_exists) .z.exist.warn.sr()
    if(x_exists){
      adj_vars <- "x"
    } else {
      .x.miss.warn()
      method <- "ANOVA"
    }
  }
  if(car_scheme == "minimization"){
    if(z_exists){
      if(x_exists){
        adj_vars <- "joint_x_z"
      } else {
        adj_vars <- "joint_z"
      }
    } else {
      .z.miss.err()
    }
  }
  if(car_scheme == "applicable"){
    if(z_exists){
      adj_se_z <- TRUE
      if(cov_strata){
        if(x_exists){
          adj_vars <- "joint_x_z"
        } else {
          adj_vars <- "z"
        }
      } else {
        if(x_exists){
          adj_vars <- "x"
        } else {
          method <- "ANOVA"
        }
      }
    } else {
      .z.miss.err()
    }
  }
  return(list(
    method=method,
    adj_se_z=adj_se_z,
    adj_vars=adj_vars
  ))
}

.get.logic <- function(adj_method, ...){
  if(adj_method == "ANOVA") args <- .anova.logic(...)
  if(adj_method == "ANCOVA") args <- .ancova.logic(...)
  if(adj_method == "ANHECOVA") args <- .anhecova.logic(...)
  return(args)
}

#' Makes a model class for the specified adjustment method
#' with settings for covariate randomization
#' scheme and vcovHC type.
#' 
#' @param adj_method Type of adjustment method
#' @param car_scheme Type of covariate adaptive randomization
#' @param vcovHC Type of heteroskedasticity-consistent SE's
#' @param covariate_to_include_strata Include strata as covariates
.make.model <- function(data, adj_method, car_scheme, vcovHC,
                        covariate_to_include_strata) {
  
  if(is.null(covariate_to_include_strata)){
    if(adj_method == "ANHECOVA"){
      cov_strata <- TRUE
    } else {
      cov_strata <- FALSE
    }
  } else {
    cov_strata <- covariate_to_include_strata
  }
  
  x_exists <- !is.null(data$covariate)
  z_exists <- !is.null(data$strata)
  
  # Get logic for adjustment methods
  logic <- .get.logic(adj_method=adj_method, car_scheme=car_scheme,
                      x_exists=x_exists, z_exists=z_exists,
                      cov_strata=cov_strata)
  
  model <- structure(
    list(
      vcovHC=vcovHC,
      adj_se_z=logic$adj_se_z,
      adj_vars=logic$adj_vars
    ),
    class=c("LinModel", logic$method)
  )
  
  return(model)
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

.get.factor.xs <- function(data){
  factors <- c()
  for(col in colnames(data$covariate)){
    if(is.factor(data$covariate[[col]])) factors <- c(factors, col)
  }
  return(factors)
}

#' Get design matrix for the specified adjustment variables
#' using the data stored.
.get.dmat <- function(data, adj_vars){
  if(is.null(adj_vars)){
    dmat <- NULL
  } else if(adj_vars == "x"){
    dmat <- data$covariate
  } else if(adj_vars == "z"){
    dmat <- data$strata
  } else if(adj_vars == "joint_x"){
    factors <- .get.factor.xs(data)
    # TODO: Add logic for joint levels
  } else if(adj_vars == "joint_z"){
    dmat <- data$strata
    # TODO: Add logic for joint levels
  } else if(adj_vars == "joint_x_z"){
    factors <- .get.factor.xs(data)
    # TODO: Add logic for joint levels
  } else {
    stop(paste("Unrecognized adjustment variable type ", adj_vars))
  }
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