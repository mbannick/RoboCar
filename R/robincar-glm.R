#' Estimate treatment-group-specific response means and (optionally) 
#' treatment group contrasts.
#' 
#' @param df A data.frame with the required columns
#' @param treat_col Name of column in df with treatment variable
#' @param response_col Name of the column in df with response variable
#' @param strata_cols Names of columns in df with strata variables
#' @param covariate_cols Names of columns in df with covariate variables
#' @param car_scheme Name of the type of covariate-adaptive randomization scheme
#' @param adj_method Name of adjustment method to use, one of "heterogeneous" or "homogeneous"
#' @param vcovHC Type of heteroskedasticity-consistent variance estimates
#' @param covariate_to_include_strata Whether to include strata variables in covariate adjustment. Defaults to F for ANOVA and ANCOVA; defaults to T for ANHECOVA. User may override by passing in this argument.
#' @param conf_level Level for confidence intervals
#' @param contrast An optional function to specify a desired contrast
#' @param g_family Family that would be supplied to glm(...), e.g., binomial. If no link specified, will use default link, like behavior in glm.
#' @param g_accuracy Level of accuracy to check prediction un-biasedness.
#' @param formula An optional formula to use for adjustment specified using as.formula("..."). This overrides strata_cols and covariate_cols.
#' 
#' @export
robincar_glm <- function(df,
                         treat_col, response_col, strata_cols, covariate_cols,
                         car_scheme="simple", adj_method="heterogeneous", vcovHC="HC0",
                         covariate_to_include_strata=NULL,
                         g_family=gaussian, g_accuracy=7, formula=NULL,
                         conf_level=0.95, contrast=NULL){
  
  .check.car_scheme(car_scheme)
  .check.adj_method.glm(adj_method)
  .check.vcovHC(vcovHC)
  
  if(!is.null(formula)){
    formula_cols <- .get.varnames.from.formula(
      formula=formula, df=df, treat_col=treat_col
    )
    data <- .make.data(
      data=df, classname="RoboDataGLM",
      treat_col=treat_col,
      response_col=response_col,
      formula_cols=formula_cols
    )
  } else {
    data <- .make.data(
      data=df, classname="RoboDataGLM",
      treat_col=treat_col,
      response_col=response_col,
      strata_cols=strata_cols,
      covariate_cols=covariate_cols,
    )
  }
  validate(data)
  
  # Create model object
  model <- .make.model(
    data=data,
    adj_method=adj_method,
    car_scheme=car_scheme,
    vcovHC=vcovHC,
    covariate_to_include_strata=covariate_to_include_strata,
    g_family=g_family,
    g_accuracy=g_accuracy
  )
  
  # Perform adjustment
  result <- adjust(model, data)
  
  # Create transformation object
  if(!is.null(contrast)){
    
  }
  
}