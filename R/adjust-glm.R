#' Perform ANCOVA adjustment with 
#' 
#' @inheritParams adjust
#' @param model Object of class ANCOVA_GLM
#' @exportS3Method RoboCar::adjust
adjust.ANCOVA_GLM <- function(model, data){
  
  # TODO: Integrate this with the ANCOVA
  # code so that it's only one function,
  # that includes info about the link function.
  
  return(list())
}

#' Perform AIPW-type ANCOVA adjustment
#' 
#' @inheritParams adjust
#' @param model Object of class ANCOVA_AIPW
#' @exportS3Method RoboCar::adjust
adjust.ANCOVA_AIPW <- function(model, data){
  
  return(list())
}

#' Perform AIPW-type ANHECOVA adjustment
#' 
#' @inheritParams adjust
#' @param model Object of class ANHECOVA_AIPW
#' @exportS3Method RoboCar::adjust
adjust.ANHECOVA_AIPW <- function(model, data){
  
  return(list())
}
