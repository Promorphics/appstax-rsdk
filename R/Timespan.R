.Timespan <- setRefClass("Timespan",
                         fields = list(
                           start = "POSIXt",
                           end = "POSIXt"
                        ),
                        methods = list(
                          asISOStrings = function() {
                            asStrings()
                          },
                          asStrings = function(fmt = character()) {
                            if (missing(fmt)) {
                              fmt = "%Y-%m-%dT%H:%M:%SZ"
                            }
                            res <- list()
                            if (!is.na(.self$start)) {
                              res["start"] = format(.self$start, fmt)
                            }
                            if (!is.na(.self$end)) {
                              res["end"] = format(.self$end, fmt)
                            }
                            res
                          }
                        )
                    )

.datify <- function(date) {
  result <- date
  if (isTRUE(all.equal(date, NULL)) || typeof(date) == "character") {
    if (length(date) > 0 && nchar(date) > 0) {
      result <- .testFormats(date)
    }
    else {
      result <- strptime("0000-00-00", "%Y-%m-%d")
    }  
  }
  result
}

.empty = function(value) {
  length(value) == 0 || (!isTRUE(all.equal(value, NULL)) && is.na(value))
}

.getFormats <- function(x) { 
    .dformats <- c(
      "%m/%d/%y",
      "%m/%d/%Y",
      "%m/%y",
      "%m/%Y",
      "%y-%m-%d",
      "%Y-%m-%d",
      "%y-%m",
      "%Y-%m",     
      ""
    )
    .tformats <- c(
      "T%H:%M:%S",
      " %H:%M:%S",
      " %H:%M",
      " %I:%M:%S %p",
      " %I:%M:%S%p",
      " %I:%M %p",
      " %I:%M%p",
      ""
    )
    .zformats <- c(
      " %z",
      ""
    )
    result = c()
    c = 1
    for (i in 1: length(.dformats)) {
      for (j in 1: length(.tformats)) {
        for (k in 1: length(.zformats)) {
          val <- str_trim(paste(.dformats[i], .tformats[j], .zformats[k], sep=""))
          if (str_length(val)>0) {
            result[c] <- val
            c <- c + 1
          }
        }
      }
    }
    result <- result[order(1000 - nchar(result))]
    writeLines("THE FOLLOWING DATE TIME FORMATS ARE RECOGNIZED (IN THIS ORDER):")
    for (i in 1: length(result)) {
      writeLines(paste(i," ","\"",result[i],"\"",sep=""))
    }

  result
}

.formats <- eval(call(".getFormats"))

.compareDates <- function(dateStr1, dateStr2) {
  nums1 <- as.numeric(str_extract_all(dateStr1,"([0-9]+)?")[[1]])
  ampm1 <-str_extract_all(dateStr1,"AM|PM")[[1]]
  nums2 <- as.numeric(str_extract_all(dateStr2,"([0-9]+)?")[[1]])
  ampm2 <-str_extract_all(dateStr2,"AM|PM")[[1]]

  isTRUE(all.equal(nums1, nums2)) && isTRUE(all.equal(ampm1, ampm2))
}

.testFormats <- function(dateStr) {
  f <- .formats
  for (i in 1:length(f)) {
    fmt = f[i]
    d <- strptime(dateStr,fmt)
    if (length(d) > 0 && !is.na(d)) {
      s <- format(d,fmt)
      #writeLines(paste("COMPARING",dateStr,s))
      if (.compareDates(s, dateStr)) {
        #writeLines(paste("FOUND",f[i],s,d,dateStr))
        return(d)
      }
    }
  }
  stop(paste("Date format not found for",dateStr))
}

#' Constructor for a Timespan object
#' 
#' @export
Timespan <- function(start = character(), end = character()) {
  if (.empty(start)) {
    start = NULL
  }
  if (.empty(end)) {
    end = NULL
  }
  .Timespan$new(start = .datify(start), end = .datify(end))
}