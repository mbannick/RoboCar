#' Generic function for making model
#'
#' @param data Object of class Data
#' @export
.make.model <- function(data, ...){
  UseMethod(".make.model", data)
}

#' Makes a model class for the specified linear adjustment method
#' with settings for covariate randomization
#' scheme and vcovHC type.
#' 
#' @param adj_method Type of adjustment method
#' @param car_scheme Type of covariate adaptive randomization
#' @param vcovHC Type of heteroskedasticity-consistent SE's
#' @param covariate_to_include_strata Include strata as covariates
.make.model.RoboDataLinear <- function(data, adj_method, car_scheme, vcovHC,
                                       covariate_to_include_strata) {
  
  if(is.null(covariate_to_include_strata)){
    if(adj_method == "ANHECOVA"){
      cov_strata <- TRUE
    } else {
      cov_strata <- FALSE
    }
  } else {
    cov_strata <- covariate_to_include_strata
  }
  
  x_exists <- !is.null(data$covariate)
  z_exists <- !is.null(data$strata)
  
  # Get logic for adjustment methods
  logic <- lmlogic(meth=structure(list(), class=adj_method),
                   car_scheme=car_scheme,
                   x_exists=x_exists, z_exists=z_exists,
                   cov_strata=cov_strata)
  
  model <- structure(
    list(
      vcovHC=vcovHC,
      omegaz_func=logic$omegaz_func,
      adj_vars=logic$adj_vars,
      car_scheme=logic$car_scheme
    ),
    class=c("LinModel", logic$method)
  )
  
  return(model)
}

#' Makes a model class for the specified glm adjustment method
#' with settings for covariate randomization
#' scheme and vcovHC type.
#' 
#' @param adj_method Type of adjustment method
#' @param car_scheme Type of covariate adaptive randomization
#' @param vcovHC Type of heteroskedasticity-consistent SE's
#' @param covariate_to_include_strata Include strata as covariates
#' @param g_family Family for GLM model
#' @param g_accuracy Accuracy tolerance for prediction unbiasedness
.make.model.RoboDataGLM <- function(data, adj_method, car_scheme, vcovHC,
                                    covariate_to_include_strata,
                                    g_family, g_accuracy) {
  
  x_exists <- !is.null(data$covariate)
  z_exists <- !is.null(data$strata)
  
  # Get logic for adjustment methods
  logic <- glmlogic(adj_method=adj_method, car_scheme=car_scheme,
                    x_exists=x_exists, z_exists=z_exists,
                    cov_strata=covariate_to_include_strata,
                    formula=data$formula)
  model <- structure(
    list(
      vcovHC=vcovHC,
      g_family=g_family,
      g_accuracy=g_accuracy,
      adj_se_z=logic$adj_se_z,
      adj_vars=logic$adj_vars,
      pu_joint_z=logic$pu_joint_z,
      pu_funcs=logic$pu_funcs
    ),
    class=c(logic$method)
  )
  
  return(model)
}