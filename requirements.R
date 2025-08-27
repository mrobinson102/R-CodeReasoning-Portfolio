# Install minimal deps for testing and benchmarking
pkgs <- c("testthat", "microbenchmark")
to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(to_install)) install.packages(to_install, repos="https://cloud.r-project.org")
message("Dependencies ensured: ", paste(pkgs, collapse=", "))
