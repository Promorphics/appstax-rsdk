source("tests/Base.R")

test.asset.get.all <-function() {
  sdk <- mockSDKFor("asset")
  r <- sdk$Asset()$get()
  checkEqualsV("https://u:p@domain:111/rest/asset/all",r)
}

test.asset.get.one <-function() {
  sdk <- mockSDKFor()
  r <- sdk$Asset()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/asset/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
  
  r <- sdk$Asset()$get("blah")
  checkEqualsV(InvalidTypeError("UUID", "blah"),r)
}

test.asset.channel.get <- function() {
  sdk <- mockSDKFor("channel")
  
  r <- sdk$Asset()$Channel()$get("blah")
  checkEqualsV(InvalidTypeError("UUID", "blah"),r)
  
  r <- sdk$Asset()$Channel()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/asset/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel/all?includeDerived=TRUE",r)
  
  r <- sdk$Asset()$Channel()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", include_derived = T)
  checkEqualsV("https://u:p@domain:111/rest/asset/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel/all?includeDerived=TRUE",r)
  
  r <- sdk$Asset()$Channel()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", include_derived = F)
  checkEqualsV("https://u:p@domain:111/rest/asset/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel/all?includeDerived=FALSE",r)
  
  r <- sdk$Asset()$Channel()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", channel_key = "use")
  checkEqualsV("https://u:p@domain:111/rest/asset/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel/use",r)
}

test.asset.channel.blueprint.get <- function() {
  sdk <- mockSDKFor("blueprint")
  
  r <- sdk$Asset()$Channel()$Blueprint()$get("blah")
  checkEqualsV(InvalidTypeError("UUID", "blah"),r)
  
  r <- sdk$Asset()$Channel()$Blueprint()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/asset/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/channel/blueprints",r)
}
