
predictions <- function(model, data, mod){
  UseMethod("predictions", model)
}

predictions.GLMModel <- function(model, data, mod){
  df <- data.frame(
    treat=data$treat,
    response=data$response
  )
  dmat <- .get.dmat(data, model$adj_vars)
  df <- cbind(df, dmat)
  preds <- predict(mod, newdata=df, type="response")
  return(preds)
}

predictions.GCOMP <- function(model, data, mod){
  
  set.treat <- function(a){
    dat <- copy(data)
    dat$treat <- rep(a, data$n)
    dat$treat <- factor(dat$treat, levels=data$treat_levels)
    return(dat)
  }
  pred.treat <- function(dat) predict.GLMModel(model, dat, mod)
  
  datas <- lapply(data$treat_levels, set.treat)
  preds <- lapply(datas, pred.treat)
  
  pred_cols <- do.call(cbind, preds)
  
  return(pred_cols)
}

predictions.AIPW <- function(model, data, mod){
  gcomp_preds <- predictions.GCOMP(model, data, mod)
  browser()
  return(gcomp_preds)
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
  
  glmod <- linmod(model, data, family=model$g_family)
  preds <- predictions(model, data, glmod)
  
  return(list())
}
