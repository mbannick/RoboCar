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
  # if(length(xbeta) > data$k) stop("You don't need to calculate B for ANHECOVA model.")
  coefmat <- rep(xbeta, data$k) %>% matrix(byrow=FALSE, ncol=data$k)
  
  return(coefmat)
}

#' Gets the Script B matrix for AN(HE)COVA models
#' in the asymptotic variance formula.
.vcov_sr.ScriptB <- function(data, model){
  # Create an ANHECOVA model to get coefficients for the variance
  # calculation, that adjusts for whatever the adjustment variables were.
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
  
  # Get rid of variables that were collinear
  # in the regression
  C <- apply(is.na(ScriptB), FUN=any, MARGIN=1)
  
  if(length(C) > 1){
    ScriptB <- ScriptB[!C,]
    B <- B[!C,]
    covX <- covX[!C,!C]
  }
  varcov <- diagmat + 
    t(ScriptB) %*% covX %*% B + 
    t(B) %*% covX %*% ScriptB -
    t(B) %*% covX %*% B
  
  return(varcov)
}

#' Gets ANHECOVA asymptotic variance under simple randomization
vcov_sr.ANHECOVA <- function(model, data, mod){

  diagmat <- .vcov_sr.diag(data, mod)
  covX <- cov(.get.dmat(data, model$adj_vars))
  
  ScriptB <- .vcov_sr.ScriptB(data, model)
  C <- apply(is.na(ScriptB), FUN=any, MARGIN=1)
  
  if(length(C) > 1){
    ScriptB <- ScriptB[!C,]
    covX <- covX[!C,!C]
  }
  
  varcov <- diagmat +
    t(ScriptB) %*% covX %*% ScriptB
  
  return(varcov)
}

vcov_car <- function(model, data, mod){
  
  # Get the variance under simple randomization
  sr <- vcov_sr(model, data, mod)
  
  # Calculate Omega Z under simple 
  omegaz_sr <- omegaz.closure("simple")(data$pie)
  
  # Adjust this variance for Z, if necessary
  if(!is.null(model$omegaz_func)){
    
    # Calculate Omega Z under the covariate adaptive randomization scheme
    # Right now this will only be zero, but it's here for more generality
    # if we eventually include the urn design
    omegaz <- model$omegaz_func(data$pie)
    
    # Calculate the expectation of the residuals within each level of 
    # strata variables Z
    dat <- tibble(
      treat=data$treat,
      strata=data$joint_strata, 
      resid=residuals(mod)
    ) %>% 
      group_by(treat, strata) %>%
      summarize(mean=mean(resid), .groups="drop")
    
    # Calculate strata levels and proportions for
    # the outer expectation
    strata_levels <- data$joint_strata %>% levels
    strata_props <- data$joint_strata %>% table %>% proportions
    
    # Estimate R(B) by first getting the conditional expectation
    # vector for a particular strata (vector contains
    # all treatment groups), then dividing by the pi_t
    .get.cond.exp <- function(s) dat %>% 
      filter(strata==s) %>%
      arrange(treat) %>% 
      pull(mean)
    
    .get.rb <- function(s) diag(.get.cond.exp(s) / c(data$pie))
    
    # Get the R(B) matrix for all strata levels
    rb_z <- lapply(strata_levels, .get.rb)
    
    # Compute the R(B)[\Omega_{SR} - \Omega_{Z_i}]R(B) | Z_i
    # for each Z_i
    rb_omega_rb_z <- lapply(rb_z, function(x) x %*% (omegaz_sr - omegaz) %*% x)
    
    # Compute the outer expectation 
    ERB <- mapply(function(x, y) x*y, x=rb_omega_rb_z, y=strata_props, SIMPLIFY=FALSE)
    ERB <- Reduce("+", ERB)

    variance <- sr - ERB
  } else {
    variance <- sr
  }
  return(sr)
}

