# Functions for validating data and creating data classes
# to be used across all analysis methods.

.check.response <- function(x){
  if(class(x) != "numeric"){
    return("Response column must be numeric.")
  }
}

.check.event <- function(x){
  if(!all(x %in% c(0, 1))){
    return("Event column must contain only 0 and 1.")
  }
}

.return.error <- function(err){
  if(length(errors) > 0) stop(paste0(errors, sep="\n"))
}

.check.attributes <- function(x, ...){
  required <- c(...)
  existing <- names(x)
  missing <- which(!required %in% existing)

  if(length(missing > 0)){
    return(paste0("Missing data attributes ", paste(missing)))
  }
}

# Generic function for data validation
validate <- function (data) {
  UseMethod("validate", data)
}

# Validate linear model data
validate.RoboDataLinear <- function(data){

  errors <- character()
  errors <- c(errors, .check.attributes(data, "treat", "response"))
  errors <- c(errors, .check.response(data))

  .return.error(errors)
}

validate.RoboDataGLM <- function(data){

  errors <- character()
  .return.error(errors)

}

# Validate time-to-event data
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
  for(colname in ...){
    if(!grepl("col", colname)) stop(paste0("Unrecognized column arguments ",
                                           colname, ". All columns must have
                                           _col or _cols as a suffix."))
    colname <- gsub(colname, "_col", "")
    colname <- gsub(colname, ".")
    data[[colname]] <- df[[colname]]
  }
  class(data) <- class

  return(data)
}
