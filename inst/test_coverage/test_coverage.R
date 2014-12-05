
# Test coverage

library(testthat)
library("testCoverage")

reportCoverage("rinca", unittestdir = "tests/testthat", 
               reportfile = "inst/test_coverage/test.html",
               outputfile = "inst/test_coverage/traceOutput.txt")
