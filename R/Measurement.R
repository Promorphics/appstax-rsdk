.MeasurementQuery = setRefClass("MeasurementQuery", 
                              contains="Query",
                              methods  = list(  
                                getAggregated = function(id) {
                                  if (!.Validate()$isUUID(id)) {
                                    return(InvalidTypeError("UUID", id))
                                  }
                                  getResult(path("measurement", "aggregated", id), list(), c("consolidations"))
                                },
                                get = function(src, type, asset_id, channel, timespan = Timespan(), limit=numeric()) {
                                  if (!.Validate()$isMeasurementSource(src)) {
                                    return(InvalidTypeError("MeasurementSource", src))
                                  }
                                  if (!.Validate()$isMeasurementType(type)) {
                                    return(InvalidTypeError("MeasurementType", type))
                                  }
                                  if (!.Validate()$isUUID(asset_id)) {
                                    return(InvalidTypeError("UUID", asset_id))
                                  }
                                  
                                  
                                  if (!missing(channel)) {
                                    p = path("measurement", tolower(c(src)), tolower(c(type)), asset_id, channel)
                                  }
                                  else {
                                    p = path("measurement", tolower(c(src)), tolower(c(type)), asset_id)
                                  }
                                  
                                  if (!missing(timespan)) {
                                    vals = timespan$asISOStrings()
                                  }
                                  else {
                                    vals = list()
                                  }
                                  
                                  if (!missing(limit)) {
                                    vals[["maxvals"]] = limit
                                  }

                                  getResult(p, vals, c("measurement"))
                                },
                                intersect = function(left, right, timespan, maxvals = numeric()) {
                                  if (!.Validate()$isUUID(left)) {
                                    return(InvalidTypeError("UUID", left))
                                  }
                                  if (!.Validate()$isUUID(right)) {
                                    return(InvalidTypeError("UUID", right))
                                  }
                                  ts = timespan$asISOStrings()
                                  vals = list(left = left, right = right, start = ts$start, end = ts$end)
                                  getResult(path("measurement", "aggregated", "intersect"), vals)
                                },
                                last = function(id, type, n) {
                                  if (!.Validate()$isUUID(id)) {
                                    return(InvalidTypeError("UUID", id))
                                  }
                                  if (!.Validate()$isMeasurementType(type)) {
                                    return(InvalidTypeError("MeasurementType", type))
                                  }
                                  if (!.Validate()$isInteger(n)) {
                                    return(InvalidTypeError("Integer", n))
                                  }
                                  getResult(path("measurement", "raw", tolower(c(type)), id, "last", n))
                                }
                              ))

#' Constructor for Measurement requests from the SDK session object
#' 
.Session$methods(Measurement = function() {
  .MeasurementQuery(session = .self)
})