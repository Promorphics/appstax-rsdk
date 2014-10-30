source("tests/Base.R")

test.test.compare <- function() {
  checkTrue(.compareDates("1/3/2011 2:45 PM","01/3/2011 2:45 PM"))
  checkTrue(.compareDates("1/3/2011 2:45 PM","01/03/2011 2:45 PM"))
  checkTrue(.compareDates("1/3/2011 2:45 PM","01/3/2011 02:45 PM"))
  checkTrue(.compareDates("1/3/2011 2:45 PM","01/03/2011 02:45 PM"))
  checkTrue(!(.compareDates("1/3/2011 2:45 PM","01/3/2011 2:45 AM")))
}

test.timespan.recognizes.formats <- function() {
  ts <- Timespan("1/3/2011 2:45 PM","2011-02-24T03:34:23")$asISOStrings()
  checkEqualsV("2011-01-03T14:45:00Z",ts$start)
  checkEqualsV("2011-02-24T03:34:23Z",ts$end)
  
  ts <- Timespan("1/3/11 2:45 PM","11-02-24T03:34:23")$asISOStrings()
  checkEqualsV("2011-01-03T14:45:00Z",ts$start)
  checkEqualsV("2011-02-24T03:34:23Z",ts$end)
  
  ts <- Timespan("1/1/2014","2014-3-2")$asISOStrings()
  checkEqualsV("2014-01-01T00:00:00Z",ts$start)
  checkEqualsV("2014-03-02T00:00:00Z",ts$end)
  
  ts <- Timespan("1/1/14","14-3-2")$asISOStrings()
  checkEqualsV("2014-01-01T00:00:00Z",ts$start)
  checkEqualsV("2014-03-02T00:00:00Z",ts$end)
  
  ts <- Timespan("1/1/14 3:13")$asISOStrings()
  checkEqualsV("2014-01-01T03:13:00Z",ts$start)
}

test.timespan.handles.empty <- function() {
  ts <- Timespan("", "2014-01-01")$asISOStrings()
  checkEqualsV("2014-01-01T00:00:00Z",ts$end)
  checkEqualsV(NULL,ts$start)
  
  ts <- Timespan("2014-01-01", "")$asISOStrings()
  checkEqualsV("2014-01-01T00:00:00Z",ts$start)
  checkEqualsV(NULL,ts$end)
  
  ts <- Timespan(NULL, "2014-01-01")$asISOStrings()
  checkEqualsV("2014-01-01T00:00:00Z",ts$end)
  checkEqualsV(NULL,ts$start)
  
  ts <- Timespan("2014-01-01", NA)$asISOStrings()
  checkEqualsV("2014-01-01T00:00:00Z",ts$start)
  checkEqualsV(NULL,ts$end)
  
  ts <- Timespan("2014-01-01")$asISOStrings()
  checkEqualsV("2014-01-01T00:00:00Z",ts$start)
  checkEqualsV(NULL,ts$end)
}
