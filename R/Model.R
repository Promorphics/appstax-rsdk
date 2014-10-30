#' The base class for all objects returned in a JSON response
#' 
#' 
setClass("Model",
         representation(
            "VIRTUAL",
            ID = "character"
        ))


#' A base collection of models
#' 
#'
setClass("ModelCollection",
         representation = representation(
            "VIRTUAL"
           ))