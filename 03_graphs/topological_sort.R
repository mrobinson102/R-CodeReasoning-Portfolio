# -----------------------------------------------------------------------------
# © 2025 Michelle Goulbourne Robinson. All rights reserved.
# Licensed for non-commercial evaluation only. See LICENSE in the repo root.
# Contact: MichelleGRobinson1@gmail.com for other licensing.
# -----------------------------------------------------------------------------
topological_sort <- function(edges) {
  stopifnot(all(c("from","to") %in% names(edges)))
  nodes <- unique(c(as.character(edges$from), as.character(edges$to)))
  indeg <- setNames(integer(length(nodes)), nodes)
  adj <- setNames(vector("list", length(nodes)), nodes)
  for (i in seq_len(nrow(edges))) {
    u <- as.character(edges$from[i]); v <- as.character(edges$to[i])
    adj[[u]] <- c(adj[[u]], v); indeg[v] <- indeg[v] + 1L
    if (is.null(adj[[v]])) adj[[v]] <- character(0)
  }
  q <- nodes[indeg == 0L]; order <- character(0)
  while (length(q)) {
    u <- q[1]; q <- q[-1]; order <- c(order, u)
    for (v in adj[[u]]) { indeg[v] <- indeg[v] - 1L; if (indeg[v] == 0L) q <- c(q, v) }
  }
  if (length(order) != length(nodes)) return(NULL)
  order
}
