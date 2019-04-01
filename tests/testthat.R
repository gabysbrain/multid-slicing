library(testthat)
library(slicing)

test_check("slicing",
           reporter = JunitReporter$new(file = "junit_result.xml"))
