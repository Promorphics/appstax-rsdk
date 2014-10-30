source("tests/Base.R")

test.aggregation.get.multiple <- function() {
  sdk <- mockSDKFor("aggregation")
  r <- sdk$Aggregation()$get()
  checkEqualsV("https://u:p@domain:111/rest/aggregation/all",r)
  
  r <- sdk$Aggregation()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f","not_an_entity")
  checkEqualsV(InvalidTypeError("EntityType", "not_an_entity"),r)
  
  r <- sdk$Aggregation()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "asset")
  checkEqualsV("https://u:p@domain:111/rest/aggregation/for/asset/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
  
  r <- sdk$Aggregation()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "label")
  checkEqualsV("https://u:p@domain:111/rest/aggregation/for/label/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
  
  r <- sdk$Aggregation()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "labelgroup")
  checkEqualsV("https://u:p@domain:111/rest/aggregation/for/labelgroup/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
  
  r <- sdk$Aggregation()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", "ASset")
  checkEqualsV("https://u:p@domain:111/rest/aggregation/for/asset/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
}

test.aggregation.get.one <-function() {
  sdk <- mockSDKFor()
  
  r <- sdk$Aggregation()$get("blah")
  checkEqualsV(InvalidTypeError("UUID", "blah"),r)
  
  r <- sdk$Aggregation()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/aggregation/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
}