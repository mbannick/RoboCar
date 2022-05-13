format.results <- function(data, estimates, varcov){
  # Extract estimates and create results data
  result <- tibble(
    treat=data$treat_levels,
    estimate=c(estimates),
    se=diag(varcov**0.5)
  )
  
  # Compute p-values based on the correct variance estimates
  result <- result %>% mutate(
    `pval (2-sided)`=2*pnorm(abs(estimate/se), lower.tail=F)
  )
  return(result)
}
