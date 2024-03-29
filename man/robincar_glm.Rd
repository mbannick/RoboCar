% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robincar-glm.R
\name{robincar_glm}
\alias{robincar_glm}
\title{Estimate treatment-group-specific response means and (optionally) 
treatment group contrasts.}
\usage{
robincar_glm(
  df,
  treat_col,
  response_col,
  strata_cols,
  covariate_cols,
  car_scheme = "simple",
  adj_method = "heterogeneous",
  vcovHC = "HC0",
  covariate_to_include_strata = NULL,
  g_family = gaussian,
  g_accuracy = 7,
  formula = NULL,
  conf_level = 0.95,
  contrast_h = NULL,
  contrast_dh = NULL
)
}
\arguments{
\item{df}{A data.frame with the required columns}

\item{treat_col}{Name of column in df with treatment variable}

\item{response_col}{Name of the column in df with response variable}

\item{strata_cols}{Names of columns in df with strata variables}

\item{covariate_cols}{Names of columns in df with covariate variables}

\item{car_scheme}{Name of the type of covariate-adaptive randomization scheme}

\item{adj_method}{Name of adjustment method to use, one of "heterogeneous" or "homogeneous"}

\item{vcovHC}{Type of heteroskedasticity-consistent variance estimates}

\item{covariate_to_include_strata}{Whether to include strata variables in covariate adjustment. Defaults to F for ANOVA and ANCOVA; defaults to T for ANHECOVA. User may override by passing in this argument.}

\item{g_family}{Family that would be supplied to glm(...), e.g., binomial. If no link specified, will use default link, like behavior in glm.}

\item{g_accuracy}{Level of accuracy to check prediction un-biasedness.}

\item{formula}{An optional formula to use for adjustment specified using as.formula("..."). This overrides strata_cols and covariate_cols.}

\item{conf_level}{Level for confidence intervals}

\item{contrast}{An optional function to specify a desired contrast}
}
\description{
Estimate treatment-group-specific response means and (optionally) 
treatment group contrasts.
}
\examples{
n <- 1000
set.seed(10)
df <- data.frame(A=rbinom(n, size=1, prob=0.5),
                 y=rbinom(n, size=1, prob=0.2),
                 x1=rnorm(n),
                 x2=rnorm(n),
                 x3=as.factor(rbinom(n, size=1, prob=0.5)),
                 z1=rbinom(n, size=1, prob=0.5),
                 z2=rbinom(n, size=1, prob=0.5))
df$A <- as.factor(df$A)
df$x1 <- df$x1 - mean(df$x1)
glm.homogeneous<-robincar_glm(df = df, 
                              response_col="y",
                              treat_col="A",
                              strata_cols=c("z1", "z2"),
                              covariate_cols=c("x1"),
                              car_scheme="simple",
                              g_family=binomial(link="logit"),
                              g_accuracy=7,
                              covariate_to_include_strata=TRUE,
                              adj_method="homogeneous",
                              vcovHC="HC0")
glm.heterogeneous<-robincar_glm(df = df, 
                              response_col="y",
                              treat_col="A",
                              strata_cols=c("z1", "z2"),
                              covariate_cols=c("x1"),
                              car_scheme="pocock-simon",
                              g_family=poisson,
                              g_accuracy=7,
                              covariate_to_include_strata=TRUE,
                              adj_method="heterogeneous",
                              vcovHC="HC0")
glm.heterogeneous<-robincar_glm(df = df, 
                              response_col="y",
                              treat_col="A",
                              strata_cols=c("z1", "z2"),
                              covariate_cols=c("x1"),
                              car_scheme="pocock-simon",
                              g_family=poisson,
                              g_accuracy=7,
                              covariate_to_include_strata=TRUE,
                              adj_method="heterogeneous",
                              vcovHC="HC0",
                              formula=formula(y ~ A + x1 + z1),
                              contrast_h="diff")
}
