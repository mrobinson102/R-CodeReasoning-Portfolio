#' Minimum Path Sum on Grid (Right/Down moves)
#' @param grid numeric matrix
#' @return minimal path sum from (1,1) to (n,m)
dp_min_path_sum <- function(grid) {
  stopifnot(is.matrix(grid))
  n <- nrow(grid); m <- ncol(grid)
  dp <- matrix(Inf, n, m)
  dp[1,1] <- grid[1,1]
  for (i in 1:n) {
    for (j in 1:m) {
      if (i > 1) dp[i,j] <- min(dp[i,j], dp[i-1,j] + grid[i,j])
      if (j > 1) dp[i,j] <- min(dp[i,j], dp[i,j-1] + grid[i,j])
    }
  }
  dp[n,m]
}
