#' Makes a model class for the specified adjustment method
#' with settings for covariate randomization
#' scheme and vcovHC type.
#' 
#' @param adj_method Type of adjustment method
#' @param car_scheme Type of covariate adaptive randomization
#' @param vcovHC Type of heteroskedasticity-consistent SE's
.make.model <- function(adj_method, car_scheme,vcovHC) {
  
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
  
  if(!class(model) %in% c("ANCOVA", "ANHECOVA")){
    if(is.null(data$covariate)){
      errors <- c(errors, "Specified covariate adjustment, 
                           but provided no covariates.")
    }
  } else if(class(model) %in% c("ANCOVA", "ANHECOVA")) {
    if(!is.null(data$covariate)){
      warning("Specified ANOVA, but covariates provided. 
              Will not do covariate adjustment.")
    }
  } else {
    stop("Unrecognized model class.")
  }
  
  .return.error(errors)
}

.get.vcovHC <- function(data, vcovHC){
  
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
#' @param model Object of class ANOVA
#' @param data Object of class RoboDataLinear
#' @exportS3Method RoboCar::linadjust
linadjust.ANOVA <- function(model, data){
  
  return(list())
}

#' Perform linear model adjustment using the ANCOVA adjustment method
#'
#' @param model Object of class ANCOVA
#' @param data Object of class RoboDataLinear
#' @exportS3Method RoboCar::linadjust
linadjust.ANCOVA <- function(model, data){
  return(list())
}

#' Perform linear model adjustment using the ANHECOVA adjustment method
#' 
#' @param model Object of class ANHECOVA
#' @param data Object of class RoboDataLinear
#' @exportS3Method RoboCar::linadjust
linadjust.ANHECOVA <- function(model, data){
  return(list())
}