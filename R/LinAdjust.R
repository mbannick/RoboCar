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
linadjust.anova <- function(model, data){
  # Do the adjustment for ANOVA using settings in `model` and data from `data`
  return(list())
}

#' Perform linear model adjustment using the ANCOVA adjustment method
#'
#' @param model Object of class ANCOVA
#' @param data Object of class RoboDataLinear
#' @exportS3Method RoboCar::linadjust
linadjust.ancova <- function(model, data){
  return(list())
}

#' Perform linear model adjustment using the ANHECOVA adjustment method
#' 
#' @param model Object of class ANHECOVA
#' @param data Object of class RoboDataLinear
#' @exportS3Method RoboCar::linadjust
linadjust.anhecova <- function(model, data){
  return(list())
}