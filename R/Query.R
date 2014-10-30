.drill <- function(response, values) {
  
  for (i in values) {
    response <- response[[i]]
  }
  response
}

.Query = setRefClass("Query",
                     fields = list(
                       session = "Session"
                     ),
                     methods = list(
                       path = function(...) {
                         segments <- list(...)
                         paste("/",paste(segments,collapse="/"),sep="")
                       },
                       getResult = function(url, params=list(), drill=list()) {
                         res <- session$doGET(url, params)
                         if (class(res)[1] == "Error") {
                           res
                         }
                         else {
                           .drill(res, drill)
                         }
                       }
                    )
        )