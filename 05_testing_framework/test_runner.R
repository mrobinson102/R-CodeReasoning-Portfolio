# -----------------------------------------------------------------------------
# © 2025 Michelle Goulbourne Robinson. All rights reserved.
# Licensed for non-commercial evaluation only. See LICENSE in the repo root.
# Contact: MichelleGRobinson1@gmail.com for other licensing.
# -----------------------------------------------------------------------------
# Unified test runner
suppressMessages({
  library(testthat)
  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    message("Install 'microbenchmark' for benchmark tests: install.packages('microbenchmark')")
  }
})
test_dir("05_testing_framework/tests", reporter = "summary")
