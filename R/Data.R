# Functions for validating data and creating data classes
# to be used across all analysis methods.

.check.response <- function(data){
  if(class(data$response) != "numeric"){
    return("Response column must be numeric.")
  }
}

.check.event <- function(data){
  if(!all(data$event %in% c(0, 1))){
    return("Event column must contain only 0 and 1.")
  }
}

.check.treat <- function(data){
  if(class(data$treat) != "factor"){
    return("Treatment column must be a factor variable.")
  }
}

.check.attributes <- function(x, ...){
  required <- c(...)
  existing <- names(x)
  missing <- which(!required %in% existing)
  
  if(length(missing > 0)){
    return(paste0("Missing data attributes ", paste(missing)))
  }
}

#' Generic function for data validation
#'
#' @param data Object of class Data
#' @export
validate <- function (data) {
  UseMethod("validate", data)
}

#' Validate linear model data
#'
#' @param data Object of class RoboDataLinear
#' @exportS3Method RoboCar::validate
validate.RoboDataLinear <- function(data){
  
  errors <- character()
  errors <- c(errors, .check.attributes(data, "treat", "response"))
  errors <- c(errors, .check.response(data))
  errors <- c(errors, .check.treat(data))
  
  .return.error(errors)
}

#' Validate GLM model data
#'
#' @param data Object of class RoboDataGLM
#' @exportS3Method RoboCar::validate
validate.RoboDataGLM <- function(data){
  
  errors <- character()
  .return.error(errors)
  
}

#' Validate time-to-event data
#'
#' @param data Object of class RoboDataTTE
#' @exportS3Method RoboCar::validate
validate.RoboDataTTE <- function(data){
  
  errors <- character()
  errors <- c(errors, .check.attributes(data, "treat", "response", "event"))
  errors <- c(errors, .check.response(data))
  errors <- c(errors, .check.event(data))
  errors <- c(errors, .check.treat(data))
  
  .return.error(errors)
}

#' Takes a data frame and converts it to a list with
#' the attributes as specified by the names passed to ...
#'
#' @param df data.frame with columns to extract
#' @param classname name of class
#' @param ... Additional names of columns to extract, or vector of names,
#'            e.g., treat_col="treatment", or strata_cols=c("s_1", "s_2")
.df.toclass <- function(df, classname, ...){
  data <- list()
  atts <- list(...)
  for(i in 1:length(atts)){
    
    att_name <- names(atts)[i]
    att <- atts[[i]]
    
    if(!grepl("col", att_name)) stop(paste0("Unrecognized column arguments ",
                                            att_name, ". All columns must have
                                            _col or _cols as a suffix."))
    att_name <- gsub("_cols", "", att_name)
    att_name <- gsub("_col", "", att_name)
    
    if(length(att) == 1){
      data[[att_name]] <- df[[att]]
    } else {
      data[[att_name]] <- as.matrix(df[att])
    }
    
  }
  class(data) <- classname
  
  return(data)
}

.make.data <- function(df, classname, ...){
  # Convert data frame to object
  data <- .df.toclass(df, classname, ...)
  
  # Add additional data attributes
  data$n <- nrow(df)
  
}