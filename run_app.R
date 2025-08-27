lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
shiny::runApp("interface", launch.browser = TRUE)
