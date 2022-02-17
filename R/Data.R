#' Data Class
#'
#' @description Basic data class used to store data and data attributes
#'
#' @slot data A data.frame containing all of the data
#' @slot treat_col Column name of treatment variable
#' @slot response_col Column name of response variable
#' @slot strata_cols Column name(s) of strata variables
#' @slot covariate_cols Column name(s) of covariates to adjust for
#'
setClass("Data",

         slots = c(
           data = "data.frame",
           treat_col = "character",
           response_col = "character",
           strata_cols = "character",
           covariate_cols = "character"
         )
)
setValidity("Data", function(object){

  errors <- character()

  if(length(object@response_col) != 1) errors <- c(errors, "Must have exactly one response column.")
  if(length(object@treat_col) != 1) errors <- c(errors, "Must have exactly one treatment column.")
  if(length(errors > 0)) return(errors)

  df <- object@data
  response <- df[[object@response_col]]

  if(class(response) != "numeric"){
    msg <- "Response column must be numeric."
    errors <- c(errors, msg)
  }

  columns <- c(
    object@treat_col,
    object@response_col,
    object@strata_cols,
    object@covariate_cols
  )
  errors <- c(errors, unlist(sapply(columns, .check.colnames, data=df)))

  if(length(errors) > 0) return(errors)
})

#' DataTTE Class
#'
#' @description Data class used to store content and attributes for time-to-event data
#'
#' @inheritParams Data
#'
#' @slot response_col Column name of "time" variable
#' @slot event_col Column name of "event" variable (coded as 0: censored, 1: observed)
#'
setClass("DataTTE",

         contains="Data",

         slots=list(
           event_col = "character"
         )
)

setValidity("DataTTE", function(object){

  errors <- character()

  if(length(object@event_col) != 1) errors <- c(errors, "Must have exactly one event column.")
  if(length(errors) > 0) return(errors)

  errors <- c(errors, .check.colnames(object@event_col, data=df))

  df <- object@data
  events <- df[[object@event_col]]

  if(!all(events %in% c(0, 1))){
    msg <- "Event column must contain only 0 and 1."
    errors <- c(errors, msg)
  }

  if(length(errors) > 0) return(errors)
})
