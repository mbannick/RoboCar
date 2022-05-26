
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
  return(pred_cols)
}

.get.mutilde <- function(model, data, mod, check_pu=TRUE){
  
  # Get g-computation predictions
  muhat <- .get.muhat(model, data, mod)
  
  # Get treatment group indicators and outcome
  t_ids <- sapply(data$treat_levels, function(x) data$treat == x)
  y <- data$response
  
  # Compute AIPW estimator by re-centering predictions
  # within treatment groups
  recenter <- function(u, i, m) u - sum(u[i])/sum(i) + sum(y[i])/sum(i)
  mutilde <- mapply(FUN=recenter,
                    u=as.list(data.frame(muhat)),
                    i=as.list(data.frame(t_ids)),
                    m=as.list(colMeans(muhat)))
  
  if(check_pu){
    # Check prediction un-biasedness for the original muhat
    # g-computation just for warning/error reporting,
    # only up to level of accuracy specified by the user
    check.pu <- function(u, i) round(mean(u[i] - y[i]), digits=model$g_accuracy)
    
    if(!model$pu_joint_z){
      
      # Check for prediction un-biasedness just based on treatment group
      resid <- mapply(FUN=check.pu,
                      u=as.list(data.frame(muhat)),
                      i=as.list(data.frame(t_ids)))
      
    } else {
      
      # Will create matrix for combination of strata and treatment groups
      sl <- data$joint_strata_levels
      tl <- data$treat_levels
      
      # Get indicators for joint strata group
      s_ids <- sapply(sl, function(x) data$joint_strata == x)
      
      # Calculate mean of residuals within each strata-tx group
      resid <- mapply(
        FUN=check.pu,
        # Repeat the mu columns for each strata
        u=as.list(data.frame(muhat[, rep(1:ncol(muhat), each=length(sl))])),
        # Get joint levels of treatment and strata groups
        i=as.list(data.frame(
          t_ids[, rep(1:ncol(t_ids), each=length(sl))] &
            s_ids[, rep(1:ncol(s_ids), times=length(tl))]
        ))
      )
    }
    
    # Report warning or error messages, whatever
    # is passed through the model settings, if not prediction unbiased.
    if(!all(resid == 0)){
      for(func in model$pu_funcs){
        func()
      }
    }
  }
  
  return(mutilde)
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
