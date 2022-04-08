# Defines allowable arguments to the functions available to users

.check.options <- function(var, options){
  if(!var %in% options) stop(paste0(var, " must be one of ",
                                    paste(options, sep=", ")))
}

.check.car_scheme <- function(car_scheme){
  OPTIONS <- c("simple",
               "permuted-block",
               "pocock-simon",
               "biased-coin",
               "urn")
  .check.options(car_scheme, OPTIONS)
}

.check.adj_method.glm <- function(adj_method){
  OPTIONS <- c("heterogeneous",
               "homogeneous")
  .check.options(adj_method, OPTIONS)
}

.check.adj_method.linear <- function(adj_method){
  OPTIONS <- c("ANOVA",
               "ANCOVA",
               "ANHECOVA")
  .check.options(adj_method, OPTIONS)
}

.check.vcovHC <- function(vcovHC){
  OPTIONS <- c("HC0", 
               "HC1",
               "HC3")
  .check.options(vcovHC, OPTIONS)
}
