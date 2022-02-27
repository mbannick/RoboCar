test_that("Data", {

  df <- data.frame(treatment=rbinom(10, size=1, prob=0.5),
                   response=rnorm(10),
                   cov_1=rnorm(10),
                   cov_2=rnorm(10),
                   s_1=rnorm(10))

  data <- .df.toclass(df, "RoboDataLinear")
  expect_equal(class(data), "RoboDataLinear")

  validate(df)
})

test_that("Data with missing attributes", {

  df <- data.frame(treatment=rbinom(10, size=1, prob=0.5),
                   response=rnorm(10),
                   cov_1=rnorm(10),
                   cov_2=rnorm(10),
                   s_1=rnorm(10))

  data <- .df.toclass(df, "RoboDataLinear",
                      response_col="response",
                      covariate_cols=c("cov_1", "cov_2"),
                      strata_co)
  expect_error(validate(df))

})

test_that("Data with non-numeric response", {

  df <- data.frame(treatment=rbinom(10, size=1, prob=0.5),
                   response=rep(c("x", "y"), each=5),
                   cov_1=rnorm(10),
                   cov_2=rnorm(10),
                   s_1=rnorm(10))

  data <- .df.toclass(df, "RoboDataLinear",
                      treat_col="treatment",
                      response_col="response",
                      covariate_cols=c("cov_1", "cov_2"),
                      strata_cols=c("s_1"))
  expect_error(validate(df))

})
