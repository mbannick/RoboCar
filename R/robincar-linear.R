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
#' @param covariate_to_include_strata Whether to include strata variables in covariate adjustment. Defaults to F for ANOVA and ANCOVA; defaults to T for ANHECOVA. User may override by passing in this argument.
#' @param conf_level Level for confidence intervals
#' @param contrast An optional function to specify a desired contrast
#' 
#' @import dplyr
#' @export
#' @examples
#' data<-RobinCar:::data_sim
#' data$A<-as.factor(data$A)
#' fit.anova<-robincar_linear(df = data, 
#'                            response_col="y",
#'                            treat_col="A",
#'                            strata_cols=c("z1", "z2"),
#'                            covariate_cols=c("x1", "x3"),
#'                            car_scheme="simple",
#'                            adj_method="ANOVA",
#'                            vcovHC="HC3")
#' fit.ancova<-robincar_linear(df = data, 
#'                            response_col="y",
#'                            treat_col="A",
#'                            strata_cols=c("z1", "z2"),
#'                            covariate_cols=c("x1", "x3"),
#'                            car_scheme="simple",
#'                            adj_method="ANCOVA",
#'                            vcovHC="HC0")
#' fit.anhecova<-robincar_linear(df = data, 
#'                            response_col="y",
#'                            treat_col="A",
#'                            strata_cols=c("z1", "z2"),
#'                            covariate_cols=c("x1", "x3"),
#'                            car_scheme="simple",
#'                            adj_method="ANHECOVA",
#'                            vcovHC="HC0")
#'                            
#' fit.anova<-robincar_linear(df = data, 
#'                            response_col="y",
#'                            treat_col="A",
#'                            strata_cols=c("z1", "z2"),
#'                            covariate_cols=c("x1", "x3"),
#'                            car_scheme="simple",
#'                            adj_method="ANOVA",
#'                            vcovHC="HC0",
#'                            contrast_h="diff")
#' odds.ratio<-function(theta){
#'   theta0<-theta[1]
#'   theta1<-theta[2]
#'   theta2<-theta[3]
#'   OR01<-theta1/(1-theta1)/(theta0/(1-theta0))
#'   OR02<-theta2/(1-theta2)/(theta0/(1-theta0))
#'   return(c(OR01,OR02))
#'}
#' robincar_contrast(fit.anova$main, contrast_h=odds.ratio)
#'                            
#' n <- 1000
#' df <- data.frame(A=rbinom(n, size=1, prob=0.5),
#'                  y=rnorm(n),
#'                  x1=rnorm(n),
#'                  x2=rnorm(n),
#'                  x3=as.factor(rbinom(n, size=1, prob=0.5)),
#'                  z1=rbinom(n, size=1, prob=0.5),
#'                  z2=rbinom(n, size=1, prob=0.5))
#' df$A <- as.factor(df$A)
#'
#' fit.ancova<-robincar_linear(df = df, 
#'                            response_col="y",
#'                            treat_col="A",
#'                            strata_cols=c("z1", "z2"),
#'                            covariate_cols=c("x1", "x2"),
#'                            car_scheme="biased-coin",
#'                            adj_method="ANCOVA",
#'                            vcovHC="HC2")
#' fit.anhecova<-robincar_linear(df = df, 
#'                            response_col="y",
#'                            treat_col="A",
#'                            strata_cols=c("z1", "z2"),
#'                            covariate_cols=c("x1"),
#'                            car_scheme="biased-coin",
#'                            adj_method="ANHECOVA",
#'                            vcovHC="HC0")
robincar_linear <- function(df,
                            treat_col, response_col, strata_cols=NULL, covariate_cols=NULL,
                            car_scheme="simple", adj_method="ANOVA", vcovHC="HC0",
                            covariate_to_include_strata=NULL,
                            conf_level=0.95, 
                            contrast_h=NULL, contrast_dh=NULL){
  
  .check.car_scheme(car_scheme)
  .check.adj_method.linear(adj_method)
  .check.vcovHC(vcovHC)
  
  # Create data object and validate
  data <- .make.data(
    df=df, classname="RoboDataLinear",
    treat_col=treat_col,
    response_col=response_col,
    strata_cols=strata_cols,
    covariate_cols=covariate_cols
  )
  validate(data)
  
  # Create model object
  model <- .make.model(
    data=data,
    adj_method=adj_method,
    car_scheme=car_scheme,
    vcovHC=vcovHC,
    covariate_to_include_strata=covariate_to_include_strata
  )

  # Perform linear adjustment
  result <- adjust(model, data)
  
  # Create transformation object
  if(!is.null(contrast_h)){
    c_result <- robincar_contrast(
      result=result,
      contrast_h=contrast_h,
      contrast_dh=contrast_dh
    )
    result <- list(main=result, contrast=c_result)
  }
  
  return(result)
}