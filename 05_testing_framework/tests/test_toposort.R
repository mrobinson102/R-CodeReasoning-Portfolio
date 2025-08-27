context('Topological Sort')
source('../../03_graphs/topological_sort.R')

test_that('topological order exists for DAG', {
  edges <- data.frame(
    from = c('A','A','B','C'),
    to   = c('B','C','D','D'),
    stringsAsFactors = FALSE
  )
  order <- topological_sort(edges)
  expect_true(!is.null(order))
  # In a valid topo order, A must come before B and C; B,C before D
  pos <- function(x) match(x, order)
  expect_true(pos('A') < pos('B'))
  expect_true(pos('A') < pos('C'))
  expect_true(pos('B') < pos('D'))
  expect_true(pos('C') < pos('D'))
})

test_that('cycle returns NULL', {
  edges <- data.frame(
    from = c('X','Y','Z'),
    to   = c('Y','Z','X'),
    stringsAsFactors = FALSE
  )
  expect_null(topological_sort(edges))
})
