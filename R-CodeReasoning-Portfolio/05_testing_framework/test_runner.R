# Unified test runner
suppressMessages({
  library(testthat)
  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    message("Install 'microbenchmark' for benchmark tests: install.packages('microbenchmark')")
  }
})
test_dir("05_testing_framework/tests", reporter = "summary")
