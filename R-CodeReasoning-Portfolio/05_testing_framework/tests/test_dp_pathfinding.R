context('DP Min Path Sum')
source('../../02_algorithms/dp_pathfinding.R')
test_that('minimal path sum is correct', {
  g <- matrix(c(1,3,1,1,5,1), nrow=2, byrow=TRUE)
  # paths: 1-3-1 or 1-1-1-1-?  minimal sum should be 1+1+1 = 3 across first row/col adjustments?
  # Proper minimal: right, right, down -> 1+3+1 = 5; right, down, right -> 1+3+5? no.
  # Construct a clearer grid
  g <- matrix(c(1,3,1,
                1,5,1,
                4,2,1), nrow=3, byrow=TRUE)
  expect_equal(dp_min_path_sum(g), 7)  # classic example
})
