# These are the covariate randomization schemes
# that are applicable; leaves out simple randomization
# and pocock-simon minimization because those are dealt with separately
APPLICABLE.SCHEMES <- c("permuted-block",
                        "biased-coin",
                        "urn")

.heterogeneous.logic <- function(x_exists, z_exists, car_scheme, cov_strata){
  
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

.homogeneous.logic <- function(x_exists, z_exists, car_scheme, cov_strata){
  
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