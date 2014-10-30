#' A runtime error condition
#' @export
#'  
Error = setRefClass("Error", fields = list(message = "character"))

#' An incorrect type
#' @export
#'
InvalidTypeError <- function(type = character(), value = character()) {
  Error$new(message = paste(value,"is not a valid",type))
}

#' A failed request to the API
#' @export
#' 
InvalidQueryError <- function(message = character(), code = character()) {
  msg <-  paste("Invalid query to API [", code, "].", sep = "")
  if (nchar(message) > 1) {
    msg <- paste(msg, " Response was: \"", message, "\".")
  }
  Error$new(message = msg)
}