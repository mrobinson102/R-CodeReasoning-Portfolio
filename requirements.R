# -----------------------------------------------------------------------------
# © 2025 Michelle Goulbourne Robinson. All rights reserved.
# Licensed for non-commercial evaluation only. See LICENSE in the repo root.
# Contact: MichelleGRobinson1@gmail.com for other licensing.
# -----------------------------------------------------------------------------
# Windows-safe requirements installer: always use user library
lib <- Sys.getenv("R_LIBS_USER")
if (lib == "") {
  user <- Sys.getenv(ifelse(.Platform$OS.type == "windows","USERPROFILE","HOME"))
  ver  <- paste(R.version$major, R.version$minor, sep=".")
  lib  <- if (.Platform$OS.type == "windows")
            file.path(user, "AppData/Local/R/win-library", ver)
          else
            file.path(user, "R", ver)
}
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))

pkgs <- c("testthat","microbenchmark","yaml")
to_install <- setdiff(pkgs, rownames(installed.packages(lib.loc = .libPaths())))
if (length(to_install))
  install.packages(to_install, lib = lib, repos = "https://cloud.r-project.org")

message("Using libs: ", paste(.libPaths(), collapse = " | "))

# --- UI deps ---
add_pkgs <- c("shiny","DT")
missing2 <- setdiff(add_pkgs, rownames(installed.packages(lib.loc = .libPaths())))
if (length(missing2)) {
  install.packages(missing2, lib = lib, repos = "https://cloud.r-project.org")
}
