#' Gets the vcovHC weights for the sample size and number of parameters
#' 
#' @param vcovHC vcovHC option
#' @param n Number of data points
#' @param p Number of coefficients
.get.vcovHC <- function(vcovHC, n, p){
  if(vcovHC == "HC0"){
    wgt <- 1
  } else if(vcovHC == "HC1") {
    wgt <- n / (n - p)
  } else if(vcovHC == "HC3") {
    wgt <- (n / (n - p)) ** 2
  } else {
    stop("Unrecognized vcovHC method.")
  }
  return(wgt)
}

#' Get design matrix for the specified adjustment variables
#' using the data stored.
.get.dmat <- function(data, adj_vars){
  if(is.null(adj_vars)){
    dmat <- NULL
  } else if(adj_vars == "x"){
    dmat <- data$covariate
  } else if(adj_vars == "z"){
    dmat <- data$strata
  } else if(adj_vars == "joint_z"){
    dmat <- data$strata
    # TODO: Add logic for joint levels
  } else if(adj_vars == "joint_z_x"){
    factors <- .get.factor.xs(data)
    # TODO: Add logic for joint levels
  } else {
    stop(paste("Unrecognized adjustment variable type ", adj_vars))
  }
}