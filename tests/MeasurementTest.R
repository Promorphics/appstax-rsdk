source("tests/Base.R")

test.measurements.get <- function() {
  sdk <- mockSDKFor("measurement")
  
  r <- sdk$Measurement()$get("notasource","notatype","blah", "use")
  checkEqualsV(InvalidTypeError("MeasurementSource", "notasource"),r)
  
  r <- sdk$Measurement()$get("raw","notatype","blah", "use")
  checkEqualsV(InvalidTypeError("MeasurementType", "notatype"),r)        
  
  r <- sdk$Measurement()$get("raw","interval","blah", "use")
  checkEqualsV(InvalidTypeError("UUID", "blah"),r)        
               
  r <- sdk$Measurement()$get("raw", "interval", "e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "use")
  checkEqualsV("https://u:p@domain:111/rest/measurement/raw/interval/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/use",r)
  
  r <- sdk$Measurement()$get("processed", "interval", "e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "use")
  checkEqualsV("https://u:p@domain:111/rest/measurement/processed/interval/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/use",r)
  
  r <- sdk$Measurement()$get("raw", "register", "e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "use")
  checkEqualsV("https://u:p@domain:111/rest/measurement/raw/register/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/use",r)
  
  r <- sdk$Measurement()$get("processed", "register", "e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "use")
  checkEqualsV("https://u:p@domain:111/rest/measurement/processed/register/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/use",r)
}

test.measurements.get.with.timespan <- function() {
  sdk <- mockSDKFor("measurement")
  r <- sdk$Measurement()$get("raw", "interval", "e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "channel", Timespan("1/1/2010","1/1/2014"))
  checkEqualsV("https://u:p@domain:111/rest/measurement/raw/interval/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel?start=2010-01-01T00%3A00%3A00Z&end=2014-01-01T00%3A00%3A00Z",r)
  
  r <- sdk$Measurement()$get("raw", "interval", "e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "channel", Timespan("1/1/2010"))
  checkEqualsV("https://u:p@domain:111/rest/measurement/raw/interval/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel?start=2010-01-01T00%3A00%3A00Z",r)
  
  r <- sdk$Measurement()$get("raw", "interval", "e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "channel", Timespan("","1/1/2014"))
  checkEqualsV("https://u:p@domain:111/rest/measurement/raw/interval/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel?end=2014-01-01T00%3A00%3A00Z",r)
  
  r <- sdk$Measurement()$get("raw", "interval", "e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "channel", Timespan("1/1/2014",NULL))
  checkEqualsV("https://u:p@domain:111/rest/measurement/raw/interval/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel?start=2014-01-01T00%3A00%3A00Z",r)
  
  r <- sdk$Measurement()$get("raw", "interval", "e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "channel", Timespan("1/1/2010","1/1/2014"), 500)
  checkEqualsV("https://u:p@domain:111/rest/measurement/raw/interval/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel?start=2010-01-01T00%3A00%3A00Z&end=2014-01-01T00%3A00%3A00Z&maxvals=500",r)
  
}

test.measurements.get.aggregated <- function() {
  sdk <- mockSDKFor("consolidations")
  
  r <- sdk$Measurement()$getAggregated("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", Timespan("1/1/2014",NULL))
  checkEqualsV("https://u:p@domain:111/rest/measurement/aggregated/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f?start=2014-01-01T00%3A00%3A00Z",r)
  
}

test.measurements.intersect <- function() {
  sdk <- mockSDKFor()
  
  r <- sdk$Measurement()$intersect("a", "b", Timespan("1/1/2010","1/1/2014"))
  checkEqualsV(InvalidTypeError("UUID", "a"),r)  
  
  r <- sdk$Measurement()$intersect("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "b", Timespan("1/1/2010","1/1/2014"))
  checkEqualsV(InvalidTypeError("UUID", "b"),r)        
  
  r <- sdk$Measurement()$intersect("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "5aede70d-27d0-4fdb-98bf-be6551d8122b", Timespan("1/1/2010","1/1/2014"))
  checkEqualsV("https://u:p@domain:111/rest/measurement/aggregated/intersect?left=e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f&right=5aede70d-27d0-4fdb-98bf-be6551d8122b&start=2010-01-01T00%3A00%3A00Z&end=2014-01-01T00%3A00%3A00Z",r)
}

test.measurements.last <- function() {
  sdk <- mockSDKFor()
  
  r <- sdk$Measurement()$last("a", "notatype", "lots")
  checkEqualsV(InvalidTypeError("UUID", "a"),r)  
  
  r <- sdk$Measurement()$last("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "notatype", "lots")
  checkEqualsV(InvalidTypeError("MeasurementType", "notatype"),r)    
  
  r <- sdk$Measurement()$last("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "interval", "lots")
  checkEqualsV(InvalidTypeError("Integer", "lots"),r)   
  
  r <- sdk$Measurement()$last("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "interval", "5")
  checkEqualsV("https://u:p@domain:111/rest/measurement/raw/interval/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/last/5",r) 
  
  r <- sdk$Measurement()$last("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "interval", 5)
  checkEqualsV("https://u:p@domain:111/rest/measurement/raw/interval/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/last/5",r) 
  
}