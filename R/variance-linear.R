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
.vcov_sr.ScriptB <- function(data, mod){
  # Create an ANHECOVA model to get coefficients for the variance
  # calculation, that only adjusts for covariates X.
  anhecova <- structure(list(adj_vars="x"),
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
  covX <- cov(data$covariate)
  
  B <- .vcov_sr.B(data, mod)
  ScriptB <- .vcov_sr.ScriptB(data, mod)
  
  varcov <- diagmat + 
    t(ScriptB) %*% covX %*% B + 
    t(B) %*% covX %*% ScriptB -
    t(B) %*% covX %*% B
  
  return(varcov)
}

#' Gets ANHECOVA asymptotic variance under simple randomization
vcov_sr.ANHECOVA <- function(model, data, mod){
  diagmat <- .vcov_sr.diag(data, mod)
  covX <- cov(data$covariate)
  
  ScriptB <- .vcov_sr.ScriptB(data, mod)
  
  varcov <- diagmat +
    t(ScriptB) %*% covX %*% ScriptB
  
  return(varcov)
}