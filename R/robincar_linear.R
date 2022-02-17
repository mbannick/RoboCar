
robincar_linear <- function(data,
                            treat_col, response_col, strata_cols, covariate_cols,
                            car_scheme="simple", adj_method="ANOVA", vcovHC="HC0",
                            conf_level=0.95, contrast=NULL){

  # Create data object
  data_obj <- new("Data", treat_col=treat_col, response_col=response_col,
                  strata_cols=strata_cols, covariate_cols=covariate_cols)

  # Create estimator object

  # Create transformation object
  if(!is.null(contrast)){

  }

}
