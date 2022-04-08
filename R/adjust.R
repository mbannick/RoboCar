#' Generic function for adjustment
#'
#' @param model Object of class Model
#' @export
adjust <- function (model, ...) {
  UseMethod("adjust", model)
}