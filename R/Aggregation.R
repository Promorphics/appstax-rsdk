.AggregationQuery = setRefClass("AggregationQuery", 
                          contains="Query",
                          methods  = list(  
                            get = function(id=character(), entity_type=character()) {
                                if(!missing(id)) {
                                  if (!.Validate()$isUUID(id)) {
                                    return(InvalidTypeError("UUID", id))
                                  }
                                  if (!missing(entity_type)) {
                                    entity_type <- tolower(c(entity_type))
                                    if (!.Validate()$isEntityType(entity_type)) {
                                      return(InvalidTypeError("EntityType", entity_type))
                                    }
                                    result <- getResult(path("aggregation","for",entity_type,id), list(), c("aggregation"))
                                  }
                                  else {
                                    result <- getResult(path("aggregation",id))
                                  }
                                }
                                else {
                                  result <- getResult(path("aggregation","all"), list(), c("aggregation"))
                                }
                                result
                            }
                          ))


#' Constructor for Aggregations request from the SDK session object
#' 
.Session$methods(Aggregation = function() {
  .AggregationQuery(session = .self)
})