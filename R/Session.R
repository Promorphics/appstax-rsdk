.getCode = function(h) {
  vals <- strsplit(h$value(), "\r\n")
  res <- "200"
  check <- list("HTTP/1.1")[[1]]
  for (i in 2:length(vals[[1]])) { 
    val <- vals[[1]][i]
    tokens <-strsplit(val, " ")[[1]]
    if ("HTTP/1.1" %in% tokens[1]) { 
      res <- tokens[2]
    }
  }
  res
}

#' The base request object for making calls to TSAP's API
.Session <- setRefClass("Session",
                fields=list(
                  protocol = "character",
                  host = "character",
                  port = "character",
                  context = "character",
                  curl_session = "character",
                  username = "character",
                  password = "character"
                ),
                methods = list(
                  doGET = function(endpoint, params=list()) {
                    url <- .buildUrl(.self, endpoint, params)
                    h <- basicTextGatherer()
                    cat(url)
                    res <- tryCatch(
                      {
                        getURL(url, httpHeader = c(Accept = "application/json"), headerfunction = h$update) #, curl=.self$curl_session
                      },
                      error = function(e) {
                        cat(e)
                      })
                    if (nchar(res) > 1) {
                      response <- fromJSON(res)
                    }
                    else {
                      response = InvalidQueryError(code = .getCode(h), message = res)
                    }
                    response
                  }
                )
            )

#' Build a URL from an existing request object and an endpoint
.buildUrl <- function(req, endpoint, params = list()) {
  base <- paste(req$protocol, "://", req$username, ":", req$password, "@", req$host, ":", req$port, sep="")
  if (!is.null(req$context)) {
    base <- paste(base, req$context, sep="")
  }
  
  result <- paste(base, endpoint, sep="")
  if (length(params) > 0) {
    qs <- "?" 
    s <- ""
    keys <- attributes(params)$names
    for (i in 1 : length(keys)) {
      key <- keys[i]
      qs <- paste(qs, s, curlEscape(keys[i]), "=", curlEscape(params[[key]]), sep="")
      s <- "&"
    }
    result <- paste(result, qs, sep = "")
  }
  result
}


#' Constructor for a new SDK object
#'
#' @export
SDK <- function(username, password, host=character(), port=integer(), secure=T) {
  if (secure) {
    protocol = "https"
    default_port = "443"
  }
  else {
    protocol = "http"
    default_port = "80"
  }
  if (length(port) == 0) {
    port = default_port
  }
  if (length(host) == 0) {
    host = "surfcity.timeseriesgroup.com"
  }
  req <- .Session(protocol = protocol, host = host, port= paste(port,"", sep=""), username = username, password = password, curl_session = "TSAP", context = "/rest")
  req
}