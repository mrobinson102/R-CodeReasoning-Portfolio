#' Dijkstra Single-Source Shortest Paths
#' @param edges data.frame with columns: from, to, weight
#' @param source starting node id (character or numeric)
#' @return named numeric vector of distances
dijkstra_sssp <- function(edges, source) {
  stopifnot(all(c("from","to","weight") %in% names(edges)))
  nodes <- unique(c(as.character(edges$from), as.character(edges$to)))
  dist <- setNames(rep(Inf, length(nodes)), nodes)
  visited <- setNames(rep(FALSE, length(nodes)), nodes)
  src <- as.character(source)
  dist[src] <- 0

  repeat {
    # pick unvisited node with smallest distance
    candidates <- names(dist)[!visited & is.finite(dist)]
    if (!length(candidates)) break
    u <- candidates[which.min(dist[candidates])]
    visited[u] <- TRUE

    # relax neighbors of u
    nbrs <- edges[as.character(edges$from) == u, ]
    if (nrow(nbrs)) {
      for (k in seq_len(nrow(nbrs))) {
        v <- as.character(nbrs$to[k])
        w <- as.numeric(nbrs$weight[k])
        if (!visited[v] && dist[u] + w < dist[v]) {
          dist[v] <- dist[u] + w
        }
      }
    }
  }
  dist
}
