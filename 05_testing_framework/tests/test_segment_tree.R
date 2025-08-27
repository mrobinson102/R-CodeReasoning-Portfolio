context('Segment Tree')
source('../../01_data_structures/segment_tree.R')
test_that('range sums and updates work', {
  st <- build_segment_tree(c(1,2,3,4,5))
  expect_equal(st$query(1,3), 6)
  st$update(3, 10)
  expect_equal(st$query(1,3), 13)
  expect_equal(st$query(3,5), 10+4+5)
})
