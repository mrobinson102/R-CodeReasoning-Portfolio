# -----------------------------------------------------------------------------
# © 2025 Michelle Goulbourne Robinson. All rights reserved.
# Licensed for non-commercial evaluation only. See LICENSE in the repo root.
# Contact: MichelleGRobinson1@gmail.com for other licensing.
# -----------------------------------------------------------------------------
#' Disjoint Set Union (Union-Find) with Path Compression & Union by Rank
#' @param n number of elements
#' @return list with find/union/connected
uf_create <- function(n) {
  parent <- seq_len(n)
  rank <- integer(n)
  find <- function(x) {
    if (parent[x] != x) parent[x] <<- find(parent[x])
    parent[x]
  }
  union <- function(a, b) {
    ra <- find(a); rb <- find(b)
    if (ra == rb) return(invisible(FALSE))
    if (rank[ra] < rank[rb]) parent[ra] <<- rb
    else if (rank[ra] > rank[rb]) parent[rb] <<- ra
    else { parent[rb] <<- ra; rank[ra] <<- rank[ra] + 1 }
    invisible(TRUE)
  }
  connected <- function(a, b) find(a) == find(b)
  list(find=find, union=union, connected=connected)
}
