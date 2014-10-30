checkEqualsV <- function(a,b) {
  checkEquals(a,b,paste(b,"did not equal",a))
}

mockSDKFor <- function(prefix = character()) {
  if (!missing(prefix)) {
    .prf <<- prefix
    #mock GET method returns url
    .Session$methods(doGET = function(endpoint, params=list()) {
      r <- list()
      r[[.prf]] = .buildUrl(.self, endpoint, params)
      r
    })
    objs <- ls(pos = ".GlobalEnv")
    rm(list = objs[grep(".prf", objs)], pos=".GlobalEnv")
  }
  else {
    .Session$methods(doGET = function(endpoint, params=list()) {
      .buildUrl(.self, endpoint, params)
    })
  }
  sdk <- SDK("u","p","domain",111,T)
}