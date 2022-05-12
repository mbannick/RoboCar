
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

.get.response.matrix <- function(model, data, mod){
  
  set.treat <- function(a){
    dat <- data
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

group_means <- function(a, response, preds, treat){
  group_ind <- (treat == a)
  group_num <- sum(group_ind)
  group_mean <- mean()
  
  response[group_ind] - d
}

aipw <- function(model, data, mod){
  browser()
  # Get predictions from G-Computation model
  gcomp_preds <- .get.response.matrix(model, data, mod)
  # TODO: Check prediction unbiasedness in the gcomputation estimator
  
  # Construct AIPW estimator
  theta_aipw <- function(a){
    t_id <- which(data$treat_levels == a)
    if(length(t_id) == 0) stop()
    
    mu_hat <- gcomp_preds[, t_id]
    
    i_a <- (data$treat == a)
    n_a <- sum(i_a)
    
    ybar_a <- sum(data$response[i_a]) / n_a
    mubar_a <- sum(mu_hat[i_a]) / n_a
    mu_tilde <- mu_hat - mubar_a + ybar_a
    
    residuals <- data$response - mu_tilde
    theta <- residuals[i_a] / n_a + mean(mu_tilde)
    return(theta)
  }
  est <- sapply(data$treat_levels, theta_aipw)
    
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
  
  return(result)
}
