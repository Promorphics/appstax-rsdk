source("tests/Base.R")

test.label.get.assets <- function() {
  sdk <- mockSDKFor("asset")
  
  r <- sdk$Label()$getAssets("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/assets",r)
  
  r <- sdk$Label()$getAssets("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", F)
  checkEqualsV("https://u:p@domain:111/rest/label/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/assets",r)
  
  r <- sdk$Label()$getAssets("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", T)
  checkEqualsV("https://u:p@domain:111/rest/label/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/assets/all",r)
}

test.label.get.multiple <- function() {
  sdk <- mockSDKFor("label")
  
  r <- sdk$Label()$get()
  checkEqualsV("https://u:p@domain:111/rest/label/all",r)
}

test.label.get.one <-function() {
  sdk <- mockSDKFor()
  
  r <- sdk$Label()$get("blah")
  checkEqualsV(InvalidTypeError("UUID", "blah"),r)
  
  r <- sdk$Label()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
}

test.label.get.children <- function() {
  sdk <- mockSDKFor("label")
  
  r <- sdk$Label()$getChildren("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/children",r)
}

test.label.get.roots <- function() {
  sdk <- mockSDKFor("label")
  
  r <- sdk$Label()$getRoots()
  checkEqualsV("https://u:p@domain:111/rest/label/roots",r)
}

test.label.get.subtree <- function() {
  sdk <- mockSDKFor()
  
  r <- sdk$Label()$getSubtree("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/subtree",r)
  
  r <- sdk$Label()$getSubtree("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f", 3)
  checkEqualsV("https://u:p@domain:111/rest/label/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/subtree?depth=3",r)
}

test.label.types.get.labels <- function() {
  sdk <- mockSDKFor("label")
  
  r <- sdk$Label()$Node()$getLabels("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/node/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/labels",r)
  
  r <- sdk$Label()$Tree()$getLabels("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/tree/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/labels",r)
  
  r <- sdk$Label()$Scenario()$getLabels("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/scenario/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f/labels",r)
  
}

test.label.types.get.multiple <- function() {
  sdk <- mockSDKFor("labeltype")
  
  r <- sdk$Label()$Node()$get()
  checkEqualsV("https://u:p@domain:111/rest/label/node/all",r)
  
  r <- sdk$Label()$Tree()$get()
  checkEqualsV("https://u:p@domain:111/rest/label/tree/all",r)
  
  r <- sdk$Label()$Scenario()$get()
  checkEqualsV("https://u:p@domain:111/rest/label/scenario/all",r)
  
  r <- sdk$Label()$Node()$get("somelabel")
  checkEqualsV("https://u:p@domain:111/rest/label/node/find?query=somelabel",r)
  
  r <- sdk$Label()$Tree()$get("somelabel")
  checkEqualsV("https://u:p@domain:111/rest/label/tree/find?query=somelabel",r)
  
  r <- sdk$Label()$Scenario()$get("somelabel")
  checkEqualsV("https://u:p@domain:111/rest/label/scenario/find?query=somelabel",r)
  
}

test.label.types.get.one <- function() {
  sdk <- mockSDKFor()
  
  r <- sdk$Label()$Node()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/node/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
  
  r <- sdk$Label()$Tree()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/tree/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
  
  r <- sdk$Label()$Scenario()$get("e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f")
  checkEqualsV("https://u:p@domain:111/rest/label/scenario/e9bfd82c-95ec-4a83-b3e9-fe433bf87d8f",r)
  
  
}