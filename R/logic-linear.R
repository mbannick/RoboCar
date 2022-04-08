# These are the covariate randomization schemes
# that are applicable; leaves out simple randomization
# and pocock-simon minimization because those are dealt with separately
APPLICABLE.SCHEMES <- c("permuted-block",
                        "biased-coin",
                        "urn")

.anova.logic <- function(x_exists, z_exists, car_scheme, cov_strata){
  
  method <- "ANOVA"
  adj_se_z <- FALSE
  adj_vars <- NULL
  
  if(car_scheme == "SR"){
    if(x_exists) .x.exist.warn()
    if(z_exists) .z.exist.warn()
  }
  if(car_scheme == "minimization"){
    .car.min.err()
  }
  if(car_scheme %in% APPLICABLE.SCHEMES){
    if(z_exists){
      if(x_exists) .x.exist.warn()
      adj_se_z <- TRUE
    } else {
      .z.miss.err()
    }
  }
  return(list(
    method=method,
    adj_se_z=adj_se_z,
    adj_vars=adj_vars
  ))
}

.ancova.logic <- function(x_exists, z_exists, car_scheme, cov_strata){
  
  method <- "ANCOVA"
  adj_se_z <- FALSE
  adj_vars <- NULL
  
  if(car_scheme == "SR"){
    if(z_exists) .z.exist.warn.sr()
    if(x_exists){
      adj_vars <- "x"
    } else {
      .x.miss.warn()
      method <- "ANOVA"
    }
  }
  if(car_scheme == "minimization"){
    .car.min.err()
  }
  if(car_scheme %in% APPLICABLE.SCHEMES){
    if(z_exists){
      adj_se_z <- TRUE
      if(cov_strata){
        if(x_exists){
          adj_vars <- "joint_z_x"
        } else {
          adj_vars <- "z"
        }
      } else {
        if(x_exists){
          adj_vars <- "x"
        } else {
          method <- "ANOVA"
        }
      }
    } else {
      .z.miss.err()
    }
  }
  return(list(
    method=method,
    adj_se_z=adj_se_z,
    adj_vars=adj_vars
  ))
}

.anhecova.logic <- function(x_exists, z_exists, car_scheme, cov_strata){
  
  method <- "ANHECOVA"
  adj_se_z <- FALSE
  adj_vars <- NULL
  
  if(car_scheme == "SR"){
    if(z_exists) .z.exist.warn.sr()
    if(x_exists){
      adj_vars <- "x"
    } else {
      .x.miss.warn()
      method <- "ANOVA"
    }
  }
  if(car_scheme == "minimization"){
    if(z_exists){
      if(x_exists){
        adj_vars <- "joint_z_x"
      } else {
        adj_vars <- "joint_z"
      }
    } else {
      .z.miss.err()
    }
  }
  if(car_scheme %in% APPLICABLE.SCHEMES){
    if(z_exists){
      adj_se_z <- TRUE
      if(cov_strata){
        if(x_exists){
          adj_vars <- "joint_x_z"
        } else {
          adj_vars <- "z"
        }
      } else {
        if(x_exists){
          adj_vars <- "x"
        } else {
          method <- "ANOVA"
        }
      }
    } else {
      .z.miss.err()
    }
  }
  return(list(
    method=method,
    adj_se_z=adj_se_z,
    adj_vars=adj_vars
  ))
}

.get.logic <- function(adj_method, ...){
  if(adj_method == "ANOVA") args <- .anova.logic(...)
  if(adj_method == "ANCOVA") args <- .ancova.logic(...)
  if(adj_method == "ANHECOVA") args <- .anhecova.logic(...)
  return(args)
}