#' Segment Tree for Range Sum with Point Updates
#' @param arr numeric vector
#' @return list containing tree and size with query/update functions
build_segment_tree <- function(arr) {
  stopifnot(is.numeric(arr))
  n <- length(arr)
  size <- 1
  while (size < n) size <- size * 2
  tree <- numeric(2 * size)
  # build
  tree[(size):(size + n - 1)] <- arr
  for (i in (size - 1):1) {
    tree[i] <- tree[2 * i] + tree[2 * i + 1]
  }
  list(
    size = size,
    tree = tree,
    update = function(idx, val) {
      i <- size + idx - 1
      tree[i] <<- val
      i <- i %/% 2
      while (i >= 1) {
        tree[i] <<- tree[2 * i] + tree[2 * i + 1]
        i <- i %/% 2
      }
    },
    query = function(l, r) { # inclusive 1-based
      l <- l + size - 1
      r <- r + size - 1
      res <- 0
      while (l <= r) {
        if (l %% 2 == 1) { res <- res + tree[l]; l <- l + 1 }
        if (r %% 2 == 0) { res <- res + tree[r]; r <- r - 1 }
        l <- l %/% 2; r <- r %/% 2
      }
      res
    }
  )
}
