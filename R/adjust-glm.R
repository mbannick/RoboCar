
predictions <- function(model, mod, treat_levels){
  UseMethod("predictions", model)
}

predictions.GCOMP <- function(model, mod, treat_levels){
  browser()
  set.treat <- function(a){
    dat <- mod$data
    dat$treat <- a
    return(dat)
  }
  pred.treat <- function(dat) predict(mod, newdata=dat, type="response")
  
  datas <- lapply(treat_levels, set.treat)
  preds <- lapply(datas, pred.treat)
  
  pred_cols <- do.call(cbind, preds)
  
  return(pred_cols)
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
adjust.GLMModel <- function(model, data){
  browser()
  glmod <- linmod(model, data, family=model$g_family)
  preds <- predictions(model, glmod, treat_levels=data$treat_levels)
  
  return(list())
}
