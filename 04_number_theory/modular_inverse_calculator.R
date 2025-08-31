# -----------------------------------------------------------------------------
# © 2025 Michelle Goulbourne Robinson. All rights reserved.
# Licensed for non-commercial evaluation only. See LICENSE in the repo root.
# Contact: MichelleGRobinson1@gmail.com for other licensing.
# -----------------------------------------------------------------------------
#' Modular Inverse using Extended Euclidean Algorithm
#' @param a integer
#' @param m modulus (> 1)
#' @return integer inverse in [0, m-1] or NULL if not invertible
mod_inverse <- function(a, m) {
  a <- as.integer(a); m <- as.integer(m)
  stopifnot(m > 1L)
  egcd <- function(a, b) {
    if (b == 0L) return(list(g=a, x=1L, y=0L))
    res <- egcd(b, a %% b)
    list(g=res$g, x=res$y, y=res$x - (a %/% b) * res$y)
  }
  r <- egcd(a %% m, m)
  if (r$g != 1L) return(NULL)
  ((r$x %% m) + m) %% m
}
