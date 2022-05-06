#' Compute the \Omega(Z) matrix
#' based on the given covariate adaptive randomization scheme.
#' 
#' This function returns a function to obtain the correct matrix
#' for a specified vector of randomization probabilities pi_t.
#' 
#' @examples 
#' omegaz.func <- omegaz.closure(car_scheme="biased-coin")
#' omegaz <- omegaz.func(pi_t=c(0.2, 0.4, 0.4))
omegaz.closure <- function(car_scheme){
  
  omegaz.func <- function(pi_t){
    # TODO: Check that minimization works out to 0
    if(car_scheme %in% c("simple", "minimization")){
      omegaz <- diag(pi_t) - pi_t %*% t(pi_t)
    } else {
      omegaz <- matrix(0, nrow=length(pi_t), ncol=length(pi_t))
    }
    return(omegaz)
  }
  return(omegaz.func)
}