# FUNCTIONS TO COMPUTE THE ASYMPTOTIC VARIANCE
# OF ESTIMATES FROM SIMPLE AND COVARIATE-ADAPTIVE RANDOMIZATION

#' Gets the diagonal sandwich variance component
#' for all linear models in the asymptotic variance formula.
.vcov_sr.diag <- function(data, mod){
  # Calculate the SD of the residuals from the model fit,
  # in order to compute sandwich variance -- this is
  # the asymptotic variance, not yet divided by n.
  result <- mod$data %>%
    mutate(resid=residuals(mod)) %>%
    group_by(treat) %>%
    summarize(se=sd(resid), .groups="drop") %>%
    mutate(se=se*sqrt(1/data$pie))
  
  return(diag(c(result$se)**2))
}

#' Gets the B matrix for ANCOVA models
#' in the asymptotic variance formula.
.vcov_sr.B <- function(data, mod){
  
  xbeta <- coef(mod)[-c(1:data$k)]
  if(length(xbeta) > data$k) stop("You don't need to calculate B for ANHECOVA model.")
  coefmat <- rep(xbeta, data$k) %>% matrix(byrow=FALSE, ncol=data$k)
  
  return(coefmat)
}

#' Gets the Script B matrix for AN(HE)COVA models
#' in the asymptotic variance formula.
.vcov_sr.ScriptB <- function(data, model){
  # Create an ANHECOVA model to get coefficients for the variance
  # calculation, that adjusts for whatever the adjustment variables were.
  browser()
  anhecova <- structure(list(adj_vars=model$adj_vars),
                        class=c("LinModel", "ANHECOVA")
  )
  mod.anhecova <- linmod(anhecova, data)
  
  xbeta <- coef(mod.anhecova)[-c(1:data$k)]
  coefmat <- matrix(xbeta, byrow=TRUE, ncol=data$k)
  
  return(coefmat)
}

#' Generic function for getting the asymptotic variance-covariance
#' matrix under simple randomization.
vcov_sr <- function(model, data, mod){
  UseMethod("vcov_sr", model)
}

#' Gets ANOVA asymptotic variance under simple randomization
vcov_sr.ANOVA <- function(model, data, mod){
  varcov <- .vcov_sr.diag(data, mod)
  return(varcov)
}

#' Gets ANCOVA asymptotic variance under simple randomization
vcov_sr.ANCOVA <- function(model, data, mod){
  diagmat <- .vcov_sr.diag(data, mod)
  covX <- cov(.get.dmat(data, model$adj_vars))
  
  B <- .vcov_sr.B(data, mod)
  ScriptB <- .vcov_sr.ScriptB(data, model)
  
  varcov <- diagmat + 
    t(ScriptB) %*% covX %*% B + 
    t(B) %*% covX %*% ScriptB -
    t(B) %*% covX %*% B
  
  return(varcov)
}

#' Gets ANHECOVA asymptotic variance under simple randomization
vcov_sr.ANHECOVA <- function(model, data, mod){
  browser()
  diagmat <- .vcov_sr.diag(data, mod)
  covX <- cov(.get.dmat(data, model$adj_vars))
  
  ScriptB <- .vcov_sr.ScriptB(data, model)
  
  varcov <- diagmat +
    t(ScriptB) %*% covX %*% ScriptB
  
  return(varcov)
}

estimate.rb <- function(model, pie, mod){
  rb <- diag(1/pie)
  
  # TODO: Estimate RB based on the model
  return(rb)
}

vcov_car <- function(model, data, mod){
  
  # Get the variance under simple randomization
  sr <- vcov_sr(model, data, mod)
  
  # Adjust this variance for Z, if necessary
  if(!is.null(model$omegaz_func)){
    
    # Estimate R(B)
    # Fit an ANHECOVA model
    anhecova <- structure(list(adj_vars="joint_z_x"),
                          class=c("LinModel", "ANHECOVA")
    )
    mod.anhecova <- linmod(anhecova, data)
    browser()
    means <- coef(mod.anhecova)[c(1:data$k)]
    
    preds <- mod$data %>%
      mutate(preds=predict(mod)) %>%
      group_by(treat) %>%
      summarize(mean=mean(preds), .groups="drop")
    
    # TODO: SHOULD THIS EXACTLY DROP OUT WHEN ANHECOVA IS USED?
    expectation <- preds$mean - means
    rb <- diag(c(expectation / data$pie))
    
    # Calculate Omega Z under the covariate adaptive randomization scheme
    # Right now this will only be zero, but it's here for more generality
    # if we eventually include the urn design
    omegaz <- model$omegaz_func(data$pie)
    
    # Calculate Omega Z under simple 
    omegaz_sr <- omegaz.closure("simple")(data$pie)
    variance <- sr - rb %*% (omegaz_sr - omegaz) %*% rb
  } else {
    variance <- sr
  }
  return(sr)
}

