
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

.get.muhat <- function(model, data, mod){
  
  set.treat <- function(a){
    dat <- data
    dat$treat <- rep(a, data$n)
    dat$treat <- factor(dat$treat, levels=data$treat_levels)
    return(dat)
  }
  pred.treat <- function(dat) predictions.GLMModel(model, dat, mod)
  
  datas <- lapply(data$treat_levels, set.treat)
  preds <- lapply(datas, pred.treat)
  
  pred_cols <- do.call(cbind, preds)
  
  # TODO: Check prediction unbiasedness in the gcomputation estimator
  
  return(pred_cols)
}

.get.mutilde <- function(model, data, mod){
  
  # Get g-computation predictions
  muhat <- .get.muhat(model, data, mod)
  
  # Construct AIPW estimator
  t_ids <- sapply(data$treat_levels, function(x) data$treat == x)
  y <- data$response
  means <- mapply(FUN=function(u, i, m) u - sum(u[i])/sum(i) + sum(y[i])/sum(i),
                  u=as.list(data.frame(muhat)),
                  i=as.list(data.frame(t_ids)),
                  m=as.list(colMeans(muhat)))
  
  return(means)
}

aipw <- function(model, data, mod){

  mutilde <- .get.mutilde(model, data, mod)
  est <- colMeans(mutilde)
  
  return(est)
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
  estimate <- aipw(model, data, glmod)
  variance <- vcov_car(model, data, glmod)
  
  result <- format.results(data, estimate, variance)
  
  return(list(result=result, varcov=variance, settings=model))
}
