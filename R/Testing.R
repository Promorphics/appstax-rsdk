source('R/Error.R') 
source('R/Validation.R') 
source('R/Session.R') 
source('R/Model.R') 
source('R/Query.R') 
source('R/Aggregation.R')
source('R/Asset.R')
source('R/Label.R')
source('R/Measurement.R')
source('R/Timespan.R')

test.suite <- defineTestSuite("all-tests",
                              dirs = file.path("tests"),
                              testFileRegexp = '*Test\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)