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
#' @import dplyr
#' @export
#' @examples 
#' n <- 1000
#' set.seed(10)
#' df <- data.frame(A=rbinom(n, size=1, prob=0.5),
#'                  y=rbinom(n, size=1, prob=0.2),
#'                  x1=rnorm(n),
#'                  x2=rnorm(n),
#'                  x3=as.factor(rbinom(n, size=1, prob=0.5)),
#'                  z1=rbinom(n, size=1, prob=0.5),
#'                  z2=rbinom(n, size=1, prob=0.5))
#' df$A <- as.factor(df$A)
#' df$x1 <- df$x1 - mean(df$x1)
#' glm.homogeneous<-robincar_glm(df = df, 
#'                               response_col="y",
#'                               treat_col="A",
#'                               strata_cols=c("z1", "z2"),
#'                               covariate_cols=c("x1"),
#'                               car_scheme="simple",
#'                               g_family=binomial(link="logit"),
#'                               g_accuracy=7,
#'                               covariate_to_include_strata=TRUE,
#'                               adj_method="homogeneous",
#'                               vcovHC="HC0")
#' glm.heterogeneous<-robincar_glm(df = df, 
#'                               response_col="y",
#'                               treat_col="A",
#'                               strata_cols=c("z1", "z2"),
#'                               covariate_cols=c("x1"),
#'                               car_scheme="pocock-simon",
#'                               g_family=poisson,
#'                               g_accuracy=7,
#'                               covariate_to_include_strata=TRUE,
#'                               adj_method="heterogeneous",
#'                               vcovHC="HC0")
#' glm.heterogeneous<-robincar_glm(df = df, 
#'                               response_col="y",
#'                               treat_col="A",
#'                               strata_cols=c("z1", "z2"),
#'                               covariate_cols=c("x1"),
#'                               car_scheme="pocock-simon",
#'                               g_family=poisson,
#'                               g_accuracy=7,
#'                               covariate_to_include_strata=TRUE,
#'                               adj_method="heterogeneous",
#'                               vcovHC="HC0",
#'                               formula=formula(y ~ A + x1 + z1),
#'                               contrast_h="diff")
robincar_glm <- function(df,
                         treat_col, response_col, strata_cols=NULL, covariate_cols=NULL,
                         car_scheme="simple", adj_method="heterogeneous", vcovHC="HC0",
                         covariate_to_include_strata=NULL,
                         g_family=gaussian, g_accuracy=7, formula=NULL,
                         conf_level=0.95,
                         contrast_h=NULL, contrast_dh=NULL){
  
  .check.car_scheme(car_scheme)
  .check.adj_method.glm(adj_method)
  .check.vcovHC(vcovHC)

  data <- .make.data(
    df=df, classname="RoboDataGLM",
    treat_col=treat_col,
    response_col=response_col,
    strata_cols=strata_cols,
    covariate_cols=covariate_cols,
    formula=formula
  )
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
