.Validate = setRefClass("Validation",
                        methods = list(
                          isUUID = function(x) {
                            regexpr("^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$",x,ignore.case=T) != -1
                          },
                          isEntityType = function(x) {
                            x <- tolower(c(x))
                            return (x == "asset" || x == "label" || x == "labelgroup")
                          },
                          isMeasurementSource = function(x) {
                            x <- tolower(c(x))
                            return (x == "raw" || x == "processed")
                          },
                          isMeasurementType = function(x) {
                            x <- tolower(c(x))
                            return (x == "interval" || x == "register")
                          },
                          isInteger = function(x) {
                            regexpr("^[0-9]+$",x) != -1
                          }
            ))