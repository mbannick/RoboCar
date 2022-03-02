#' Estimate treatment-group-specific response means and (optionally) 
#' treatment group contrasts.
#' 
#' @param df A data.frame with the required columns
#' @param treat_col Name of column in df with treatment variable
#' @param response_col Name of the column in df with response variable
#' @param strata_cols Names of columns in df with strata variables
#' @param covariate_cols Names of columns in df with covariate variables
#' @param car_scheme Name of the type of covariate-adaptive randomization scheme
#' @param adj_method Name of linear adjustment method to use
#' @param vcovHC Type of heteroskedasticity-consistent variance estimates
#' @param conf_level Level for confidence intervals
#' @param contrast An optional function to specify a desired contrast
#' 
#' @export
robincar_linear <- function(df,
                            treat_col, response_col, strata_cols, covariate_cols,
                            car_scheme="simple", adj_method="ANOVA", vcovHC="HC0",
                            conf_level=0.95, contrast=NULL){
  
  .check.car_scheme(car_scheme)
  .check.adj_method(adj_method)
  .check.vcovHC(vcovHC)
  
  # Create data object and validate
  data <- .make.data(
    data=df, classname="RoboDataLinear",
    treat_col=treat_col,
    response_col=response_col,
    strata_cols=strata_cols,
    covariate_cols=covariate_cols
  )
  validate(data)
  
  # Create model object
  model <- .make.model(
    adj_method=adj_method,
    car_scheme=car_scheme,
    vcovHC=vcovHC
  )
  .check.compatible_model(model, data)
  
  # Perform linear adjustment
  result <- linadjust(model, data)
  
  # Create transformation object
  if(!is.null(contrast)){
    
  }
  
}