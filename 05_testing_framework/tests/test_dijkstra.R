# -----------------------------------------------------------------------------
# © 2025 Michelle Goulbourne Robinson. All rights reserved.
# Licensed for non-commercial evaluation only. See LICENSE in the repo root.
# Contact: MichelleGRobinson1@gmail.com for other licensing.
# -----------------------------------------------------------------------------
context('Dijkstra SSSP')
source('../../03_graphs/dijkstra_graph_solver.R')
test_that('shortest paths computed', {
  edges <- data.frame(
    from=c('A','A','B','B','C','D'),
    to  =c('B','C','C','D','D','E'),
    weight=c(1,4,2,5,1,3)
  )
  dist <- dijkstra_sssp(edges, 'A')
  expect_equal(unname(dist['E']), 1+2+1+3) # A->B->C->D->E
})
