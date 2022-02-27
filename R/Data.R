# Functions for validating data and creating data classes
# to be used across all analysis methods.


#' Check response
#' 
#' @export
.check.response <- function(data){
  if(class(data$response) != "numeric"){
    return("Response column must be numeric.")
  }
}

#' Check event
#' 
#' @export
.check.event <- function(data){
  if(!all(data$event %in% c(0, 1))){
    return("Event column must contain only 0 and 1.")
  }
}

#' Helper to return errors
#' 
#' @export
.return.error <- function(err){
  if(length(err) > 0) stop(paste0(err, sep="\n"))
}

#' Helper to check attributes
#' 
#' @export
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
#' @exportS3Method
validate <- function (data) {
  UseMethod("validate", data)
}

#' Validate linear model data
#'
#' @exportS3Method
validate.RoboDataLinear <- function(data){

  errors <- character()
  errors <- c(errors, .check.attributes(data, "treat", "response"))
  errors <- c(errors, .check.response(data))

  .return.error(errors)
}

#' Validate GLM model data
#'
#' @exportS3Method
validate.RoboDataGLM <- function(data){

  errors <- character()
  .return.error(errors)

}

#' Validate time-to-event data
#'
#' @exportS3Method
validate.RoboDataTTE <- function(data){

  errors <- character()
  errors <- c(errors, .check.attributes(data, "treat", "response", "event"))
  errors <- c(errors, .check.response(data))
  errors <- c(errors, .check.event(data))

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
                                            colname, ". All columns must have
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
