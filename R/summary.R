
print.LinModelResult <- function(res){
  output <- c()
  output <- c(
    output,
    sprintf("Treatment group mean estimates using fit from an %s model", 
            class(res$settings)[2])
  )
  if(class(res$settings)[2] != "ANOVA"){
    k <- length(res$data$treat_levels)
    name <- colnames(model.matrix(res$mod))[-c(1:k)]
    output <- c(
      output,
      sprintf("\n  using adjustment variables: %s.",
              paste0(name, collapse=", "))
    )
  }
  output <- c(
    output,
    sprintf("\nUsed %s-type of heteroskedasticity-consistent variance estimates ", 
            res$settings$vcovHC)
  )
  if(!is.null(res$settings$omegaz_func)){
    strata <- colnames(res$data$strata)
    output <- c(
      output,
      sprintf("\n  and adjusted variance-covariance matrix for randomization strata consistent with the 
              %s design: %s.", 
              res$settings$car_scheme, 
              paste0(strata, collapse=", "))
    )
  }
  for(o in output){
    cat(o)
  }
  cat("\n\n")
  cat("Estimates:\n")
  print(res$result)
  cat("\nVariance-Covariance Matrix:\n")
  print(res$varcov)
}
