test_that("Linear adjustment", {
  
  df <- data.frame(treatment=rbinom(10, size=1, prob=0.5),
                   response=rnorm(10),
                   cov_1=rnorm(10),
                   cov_2=rnorm(10),
                   s_1=rbinom(10, size=1, prob=0.5),
                   s_2=rbinom(10, size=1, prob=0.5))
  df$treatment <- as.factor(df$treatment)
  
  data <- .make.data(df, "RoboDataLinear",
                     treat_col="treatment",
                     response_col="response",
                     covariate_cols=c("cov_1", "cov_2"),
                     strata_cols=c("s_1", "s_2"))
  expect_equal(class(data), "RoboDataLinear")
  
  model <- RoboCar::.make.model(
    adj_method="ANCOVA",
    car_scheme="simple",
    vcovHC="HC0"
  )
  RoboCar::.check.compatible_model(model, data)
  
  # Perform linear adjustment HELPER function
  result <- RoboCar::linmod(model, data)
  
  # TODO: This will be where the model results are; empty for now.
  result <- RoboCar::linadjust(model, data)
})

