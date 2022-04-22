
predictions <- function(model, mod){
  UseMethod("predictions", model)
}

predictions.GCOMP <- function(model, mod){
  return()
}

predictions.AIPW <- function(model, mod){
  return()
}

#' Perform GLM adjustment, based on the classes
#' of the model. Will perform adjustment based on the linear
#' model type of `model` and also do G-computation or AIPW
#' based on the second model type of `model`.
#' 
#' @inheritParams adjust
#' @param model Object of class GLMModel
#' @exportS3Method RoboCar::adjust
adjust.GLM <- function(model, data){
  
  glmod <- linmod(model, data, family=model$g_family)
  preds <- predictions(model, glmod)
  
  return(list())
}
