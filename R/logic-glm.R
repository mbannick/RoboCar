# These are the covariate randomization schemes
# that are applicable; leaves out simple randomization
# and pocock-simon minimization because those are dealt with separately
APPLICABLE.SCHEMES <- c("permuted-block",
                        "biased-coin",
                        "urn")

glmlogic <- function(adj_method, x_exists, z_exists, car_scheme, cov_strata){
  
  # Method to use for the GLM G-Computation adjustment,
  # and whether we should do AIPW on top of it.
  if(adj_method == "heterogeneous"){
    method <- "ANHECOVA"
  } else {
    method <- "ANCOVA"
  }
  adj_vars <- NULL
  
  aipw <- TRUE
  adj_se_z <- FALSE
  
  # Whether to check prediction un-biasedness in all joint levels
  # of Z, or just overall, and whether to report an error or warning
  pu_joint_z <- FALSE
  # Biased action will be one of "", "warning", "error"
  pu_funcs <- function(){}

  if(car_scheme == "simple"){
    if(z_exists) .z.exist.warn()
    if(x_exists){
      adj_vars <- "x"
      pu_func <- .pu.warn
    } else {
      .x.miss.warn()
      method <- "ANOVA"
      aipw <- FALSE
    }
  }
  if(car_scheme == "minimization"){
    
    if(heterogeneous){
      if(z_exists){
        if(!cov_strata){
          .z.include.warn()
        }
        pu_joint_z <- TRUE
        if(x_exists){
          adj_vars <- "joint_z_x"
          pu_funcs <- .pu.z.err
        } else {
          adj_vars <- "joint_z"
          pu_funcs <- c(.pu.z.calibrate, .pu.z.err)
        }
      } else {
        .z.miss.err()
      }
    } else {
      .homogeneous.min.error()
    }
    
  }
  if(car_scheme %in% APPLICABLE.SCHEMES){
    
    if(z_exists){
      adj_se_z <- TRUE
      if(x_exists){
        if(cov_strata){
          adj_vars <- "joint_z_x"
          pu_funcs <- .pu.warn
        } else {
          adj_vars <- "x"
        }
      } else {
        if(cov_strata){
          adj_vars <- "joint_z"
          pu_funcs <- .pu.warn
        } else {
          method <- "ANOVA"
          aipw <- FALSE
        }
      }
    } else {
      .z.miss.err()
    }
  }
  
  if(aipw){
    method=c("AIPW", "GLMModel", method)
  } else {
    method=c("GLMModel", method)
  }
  return(list(
    method=method,
    adj_vars=adj_vars,
    adj_se_z=adj_se_z,
    pu_joint_z=pu_joint_z,
    pu_funcs=pu_funcs
  ))
}
