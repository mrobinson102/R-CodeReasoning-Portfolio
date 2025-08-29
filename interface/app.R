# interface/app.R  (with About + Help)
suppressPackageStartupMessages({
  library(shiny)
  library(DT)
})

# --------------------------- Helpers ------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

parse_num_vec <- function(txt) {
  s <- gsub("\\s+", "", txt %||% "")
  if (!nzchar(s)) return(numeric(0))
  as.numeric(strsplit(s, ",", fixed = TRUE)[[1]])
}
parse_edges_weighted <- function(txt) {
  txt <- txt %||% ""
  lines <- strsplit(trimws(txt), "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) {
    return(data.frame(from=character(), to=character(), weight=numeric(), stringsAsFactors = FALSE))
  }
  parts <- strsplit(lines, "\\s+")
  d <- do.call(rbind, lapply(parts, function(p) {
    if (length(p) < 2) return(c(NA, NA, NA))
    c(p[1], p[2], ifelse(length(p) >= 3, p[3], "1"))
  }))
  data.frame(from = d[,1], to = d[,2], weight = as.numeric(d[,3]), stringsAsFactors = FALSE)
}
parse_edges_unweighted <- function(txt) {
  txt <- txt %||% ""
  lines <- strsplit(trimws(txt), "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(data.frame(from=character(), to=character(), stringsAsFactors = FALSE))
  parts <- strsplit(lines, "\\s+")
  d <- do.call(rbind, lapply(parts, function(p) if (length(p) >= 2) c(p[1], p[2]) else c(NA, NA)))
  data.frame(from = d[,1], to = d[,2], stringsAsFactors = FALSE)
}

# ------------------------ Segment Tree (sum) -----------------------------
st_build <- function(vec) {
  n <- length(vec); if (n == 0) return(list(n=0, tree=numeric(0)))
  tree <- numeric(4*n)
  build <- function(node, l, r) {
    if (l == r) { tree[node] <<- vec[l]; return(invisible()) }
    mid <- (l + r) %/% 2
    build(node*2, l, mid); build(node*2+1, mid+1, r)
    tree[node] <<- tree[node*2] + tree[node*2+1]
  }
  build(1, 1, n); list(n=n, tree=tree, arr=vec)
}
st_update <- function(st, idx, val) {
  if (st$n == 0) return(st)
  update <- function(node, l, r) {
    if (l == r) { st$tree[node] <<- val; st$arr[idx] <<- val; return(invisible()) }
    mid <- (l + r) %/% 2
    if (idx <= mid) update(node*2, l, mid) else update(node*2+1, mid+1, r)
    st$tree[node] <<- st$tree[node*2] + st$tree[node*2+1]
  }
  update(1,1,st$n); st
}
st_query <- function(st, ql, qr) {
  if (st$n == 0 || ql > qr) return(0)
  q <- function(node, l, r) {
    if (qr < l || r < ql) return(0)
    if (ql <= l && r <= qr) return(st$tree[node])
    mid <- (l + r) %/% 2
    q(node*2, l, mid) + q(node*2+1, mid+1, r)
  }
  q(1,1,st$n)
}

# --------------------------- Union-Find ----------------------------------
make_dsu <- function(n) {
  parent <- seq_len(n); rank <- integer(n)
  find <- function(x) { while (parent[x] != x) { parent[x] <<- parent[parent[x]]; x <- parent[x] }; x }
  unite <- function(a,b) {
    ra <- find(a); rb <- find(b); if (ra == rb) return(invisible())
    if (rank[ra] < rank[rb]) parent[ra] <<- rb
    else if (rank[ra] > rank[rb]) parent[rb] <<- ra
    else { parent[rb] <<- ra; rank[ra] <<- rank[ra] + 1 }
  }
  list(find=find, unite=unite, parent=function() parent)
}

# ---------------------------- Dijkstra -----------------------------------
dijkstra_sssp <- function(edges, source) {
  stopifnot(all(c("from","to","weight") %in% names(edges)))
  nodes <- unique(c(as.character(edges$from), as.character(edges$to)))
  src <- as.character(source); if (!(src %in% nodes)) nodes <- unique(c(nodes, src))
  dist <- setNames(rep(Inf, length(nodes)), nodes); visited <- setNames(rep(FALSE, length(nodes)), nodes)
  dist[src] <- 0
  repeat {
    candidates <- names(dist)[!visited & is.finite(dist)]; if (!length(candidates)) break
    u <- candidates[which.min(dist[candidates])]; visited[u] <- TRUE
    nbrs <- edges[as.character(edges$from) == u,]
    if (nrow(nbrs)) for (k in seq_len(nrow(nbrs))) {
      v <- as.character(nbrs$to[k]); w <- as.numeric(nbrs$weight[k])
      if (!visited[v] && dist[u] + w < dist[v]) dist[v] <- dist[u] + w
    }
  }
  dist
}

# ------------------------- DP Min Path -----------------------------------
dp_min_path <- function(mat) {
  if (!length(mat)) return(0)
  m <- nrow(mat); n <- ncol(mat); dp <- matrix(Inf, m, n); dp[1,1] <- mat[1,1]
  for (i in 1:m) for (j in 1:n) {
    if (i > 1) dp[i,j] <- min(dp[i,j], dp[i-1,j] + mat[i,j])
    if (j > 1) dp[i,j] <- min(dp[i,j], dp[i, j-1] + mat[i,j])
  }
  list(cost=dp[m,n], dp=dp)
}

# ------------------------- Modular Inverse --------------------------------
mod_inv <- function(a, m) {
  a <- as.integer(a); m <- as.integer(m); if (m <= 1) return(NA_integer_)
  egcd <- function(a,b) if (b == 0) c(a,1,0) else { gxy <- Recall(b, a %% b); c(gxy[1], gxy[3], gxy[2] - (a %/% b)*gxy[3]) }
  gxy <- egcd(a %% m, m); g <- gxy[1]; x <- gxy[2]; if (g != 1) NA_integer_ else (x %% m)
}

# -------------------------- Topological Sort ------------------------------
topo_kahn <- function(edges) {
  if (!nrow(edges)) return(character(0))
  nodes <- unique(c(as.character(edges$from), as.character(edges$to)))
  indeg <- setNames(integer(length(nodes)), nodes)
  for (i in seq_len(nrow(edges))) indeg[as.character(edges$to[i])] <- indeg[as.character(edges$to[i])] + 1
  q <- nodes[indeg == 0]; order <- character(0)
  while (length(q)) {
    u <- q[1]; q <- q[-1]; order <- c(order, u)
    out <- edges[as.character(edges$from) == u, "to", drop=TRUE]
    for (v in out) { indeg[v] <- indeg[v] - 1; if (indeg[v] == 0) q <- c(q, v) }
  }
  if (length(order) != length(nodes)) NULL else order
}

# ----------------------- About/Help Content -------------------------------
about_html <- HTML("
<h3>About this demo</h3>
<p>This is an interactive portfolio showcasing algorithm design and code reasoning in R.
It includes <b>data structures</b>, <b>graph algorithms</b>, <b>dynamic programming</b>, <b>number theory</b>,
and <b>test/benchmark mindset</b>. Use the tabs to run examples.</p>
<hr/>
<h4>Quick start</h4>
<ul>
  <li><b>Segment Tree</b> – Build from a comma-separated vector, then run range-sum queries or point updates.</li>
  <li><b>Union-Find</b> – Enter unions like <code>1-2, 2-3, 4-5</code>, then click <b>Apply</b> to see connected components.</li>
  <li><b>Dijkstra</b> – Paste one edge per line (<code>from to weight</code>), pick a source, click <b>Run</b>.</li>
  <li><b>DP Min Path</b> – Generate a random grid and compute the minimum path sum (right or down moves).</li>
  <li><b>Modular Inverse</b> – Compute <code>a^(-1) mod m</code> when it exists (coprime <code>a, m</code>).</li>
  <li><b>Topological Sort</b> – Paste <code>from to</code> lines for a DAG. Reports a cycle if one exists.</li>
</ul>
<hr/>
<h4>No installation for reviewers</h4>
<p>If this app is hosted on a Shiny server (e.g., shinyapps.io), anyone can use it from a browser
— no R required. If you're running locally from the repo, see the README's <code>run_app.R</code> snippet.</p>
")

help_modal <- function(id_title, bullets) {
  showModal(modalDialog(
    title = id_title, easyClose = TRUE, footer = modalButton('Close'),
    HTML(sprintf('<ul>%s</ul>', paste(sprintf('<li>%s</li>', bullets), collapse='')))
  ))
}

# ================================ UI ======================================
ui <- fluidPage(
  titlePanel("R Code Reasoning & Benchmarks — Interactive Demos"),
  tabsetPanel(
    tabPanel("About & Help", div(style="max-width:900px;", about_html)),

    tabPanel("Segment Tree",
      fluidRow(
        column(8, helpText("Build a sum tree, then query ranges or update a point.")),
        column(4, div(style="text-align:right;margin-top:5px;",
                      actionButton("help_seg", "Help", icon = icon("question-circle"), class="btn btn-link")))
      ),
      fluidRow(
        column(4, textInput("st_vec", "Initial vector", value="1,2,3,4,5,6,7,8"),
               actionButton("st_build","Build")),
        column(4, numericInput("st_q_l","Query L",1,min=1),
               numericInput("st_q_r","Query R",4,min=1),
               actionButton("st_query","Range Sum")),
        column(4, numericInput("st_u_idx","Update idx",3,min=1),
               numericInput("st_u_val","New value",10),
               actionButton("st_update","Update"))
      ),
      verbatimTextOutput("st_status")
    ),

    tabPanel("Union-Find",
      fluidRow(
        column(8, helpText("Enter unions like 1-2, 2-3, 4-5; nodes are labeled 1..N.")),
        column(4, div(style="text-align:right;margin-top:5px;",
                      actionButton("help_uf", "Help", icon = icon("question-circle"), class="btn btn-link")))
      ),
      fluidRow(
        column(4, numericInput("uf_n","Nodes",6,1)),
        column(8, textInput("uf_ops","Unions a-b (comma-separated)","1-2, 2-3, 4-5"))
      ),
      actionButton("uf_run","Apply"),
      DTOutput("uf_tbl")
    ),

    tabPanel("Dijkstra",
      fluidRow(
        column(8, helpText("One edge per line: from to weight (e.g., A B 7). Source must exist (adds if missing).")),
        column(4, div(style="text-align:right;margin-top:5px;",
                      actionButton("help_dj", "Help", icon = icon("question-circle"), class="btn btn-link")))
      ),
      fluidRow(
        column(8, textAreaInput("dj_edges","Edges (from to w)", rows=8,
"A B 7
A C 9
A D 14
B C 10
B E 15
C D 2
C F 11
D F 9
E F 6")),
        column(4, textInput("dj_src","Source","A"), actionButton("dj_run","Run"))
      ),
      DTOutput("dj_tbl")
    ),

    tabPanel("DP Min Path",
      fluidRow(
        column(8, helpText("Moves allowed: right or down. Values 1..9 by default when generating.")),
        column(4, div(style="text-align:right;margin-top:5px;",
                      actionButton("help_dp", "Help", icon = icon("question-circle"), class="btn btn-link")))
      ),
      fluidRow(
        column(3, numericInput("dp_m","Rows",3,1)),
        column(3, numericInput("dp_n","Cols",3,1)),
        column(3, actionButton("dp_gen","Random Grid")),
        column(3, actionButton("dp_run","Compute"))
      ),
      DTOutput("dp_grid"),
      verbatimTextOutput("dp_out")
    ),

    tabPanel("Modular Inverse",
      fluidRow(
        column(8, helpText("Computes a^(-1) mod m if gcd(a, m) = 1; otherwise reports none.")),
        column(4, div(style="text-align:right;margin-top:5px;",
                      actionButton("help_mi", "Help", icon = icon("question-circle"), class="btn btn-link")))
      ),
      fluidRow(
        column(4, numericInput("mi_a","a",3)),
        column(4, numericInput("mi_m","m",11, min=2)),
        column(4, actionButton("mi_run","Compute"))
      ),
      verbatimTextOutput("mi_out")
    ),

    tabPanel("Topological Sort",
      fluidRow(
        column(8, helpText("One edge per line: from to (unweighted). Reports a cycle if present.")),
        column(4, div(style="text-align:right;margin-top:5px;",
                      actionButton("help_ts", "Help", icon = icon("question-circle"), class="btn btn-link")))
      ),
      textAreaInput("ts_edges","Edges (from to)", rows=6,
"A B
B C
A D
D E"),
      actionButton("ts_run","Sort"),
      verbatimTextOutput("ts_out")
    )
  ),

  # --- Footer notice (inside fluidPage(...), after your tabs) ---
  tags$hr(),
  tags$div(
    style = "font-size:12px;color:#6b7280;margin-top:8px;",
    list(
      HTML("&copy; 2025 Michelle Goulbourne Robinson — Portfolio Evaluation Use Only. 
            No redistribution or commercial use without permission. "),
      tags$a(
        href = "https://github.com/mrobinson102/R-CodeReasoning-Portfolio/blob/main/LICENSE",
        target = "_blank",
        style  = "color:#1f6feb;text-decoration:none;",
        "See LICENSE"
      ),
      HTML(".")
    )
  )
)

# =============================== SERVER ===================================
server <- function(input, output, session) {
  rv <- reactiveValues(st=NULL, grid=NULL)

  # About/Help modals
  observeEvent(input$help_seg, { help_modal("Segment Tree – How to use", c(
    "Enter a comma-separated vector and click Build.",
    "Range Sum: set L and R (1-based, inclusive) and click Range Sum.",
    "Update: set index and value to update a single point."
  ))})
  observeEvent(input$help_uf, { help_modal("Union-Find – How to use", c(
    "Pick number of nodes (1..N).",
    "Enter unions like 1-2, 2-3, 4-5 and click Apply.",
    "Table shows each node and its component representative (root)."
  ))})
  observeEvent(input$help_dj, { help_modal("Dijkstra – How to use", c(
    "Paste one edge per line: 'from to weight' (e.g., A B 7).",
    "Set the Source and click Run.",
    "Output shows shortest distance from source to every node."
  ))})
  observeEvent(input$help_dp, { help_modal("DP Min Path – How to use", c(
    "Click Random Grid to generate values (1..9).",
    "Click Compute to get minimal path sum using right/down moves.",
    "Demonstrates classic dynamic programming over a grid."
  ))})
  observeEvent(input$help_mi, { help_modal("Modular Inverse – How to use", c(
    "Enter a and m (m ≥ 2).",
    "Returns a^(-1) mod m if gcd(a, m) = 1; otherwise 'no inverse'."
  ))})
  observeEvent(input$help_ts, { help_modal("Topological Sort – How to use", c(
    "Paste edges as 'from to' (one per line) for a DAG.",
    "Returns an ordering if acyclic; otherwise reports a cycle."
  ))})

  # Segment Tree
  observeEvent(input$st_build, {
    vec <- parse_num_vec(input$st_vec)
    if (!length(vec)) { output$st_status <- renderText("Provide at least one number, e.g., 1,2,3,4"); return() }
    rv$st <- st_build(vec)
    output$st_status <- renderText(sprintf("Built Segment Tree for n=%d. Array: %s", rv$st$n, paste(rv$st$arr, collapse=", ")))
  })
  observeEvent(input$st_query, {
    req(rv$st); l <- max(1, as.integer(input$st_q_l)); r <- min(rv$st$n, as.integer(input$st_q_r))
    if (l > r) { output$st_status <- renderText("Query invalid: L > R"); return() }
    s <- st_query(rv$st, l, r); output$st_status <- renderText(sprintf("Sum[%d..%d] = %s", l, r, s))
  })
  observeEvent(input$st_update, {
    req(rv$st); i <- as.integer(input$st_u_idx); v <- as.numeric(input$st_u_val)
    if (i < 1 || i > rv$st$n) { output$st_status <- renderText("Update index out of range"); return() }
    rv$st <- st_update(rv$st, i, v)
    output$st_status <- renderText(sprintf("Updated a[%d] = %s. Array: %s", i, v, paste(rv$st$arr, collapse=", ")))
  })

  # Union-Find
  observeEvent(input$uf_run, {
    req(input$uf_n)
    tryCatch({
      n <- as.integer(input$uf_n); validate(need(is.finite(n) && n > 0, "Nodes must be a positive integer."))
      dsu <- make_dsu(n)
      ops <- trimws(unlist(strsplit(input$uf_ops %||% "", ","))); ops <- ops[nchar(ops) > 0]
      for (o in ops) {
        ab <- as.integer(unlist(strsplit(gsub("\\s","",o), "-")))
        if (length(ab) == 2 && all(ab >= 1 & ab <= n)) dsu$unite(ab[1], ab[2])
      }
      comp <- vapply(1:n, dsu$find, integer(1))
      output$uf_tbl <- DT::renderDT(DT::datatable(data.frame(node=1:n, root=comp), rownames=FALSE, options=list(dom='t', paging=FALSE)))
    }, error = function(e) { showNotification(paste("Union-Find error:", e$message), type="error") })
  })

  # Dijkstra
  observeEvent(input$dj_run, {
    edges <- parse_edges_weighted(input$dj_edges); dist <- dijkstra_sssp(edges, input$dj_src %||% "")
    if (!length(dist)) { output$dj_tbl <- renderDT(DT::datatable(data.frame(info="No edges / nodes"), options=list(dom='t', paging=FALSE))); return() }
    output$dj_tbl <- renderDT(DT::datatable(data.frame(node=names(dist), distance=as.numeric(dist)), rownames=FALSE, options=list(dom='t', paging=FALSE)))
  })

  # DP Min Path
  observeEvent(input$dp_gen, {
    m <- max(1, as.integer(input$dp_m)); n <- max(1, as.integer(input$dp_n))
    rv$grid <- matrix(sample(1:9, m*n, replace=TRUE), m, n)
    output$dp_grid <- renderDT(DT::datatable(as.data.frame(rv$grid), options=list(dom='t')))
    output$dp_out  <- renderText("Random grid generated.")
  })
  observeEvent(input$dp_run, {
    req(rv$grid); res <- dp_min_path(rv$grid)
    output$dp_out <- renderText(sprintf("Min path sum = %s", res$cost))
    output$dp_grid <- renderDT(DT::datatable(as.data.frame(rv$grid), options=list(dom='t')))
  })

  # Modular Inverse
  observeEvent(input$mi_run, {
    a <- as.integer(input$mi_a); m <- as.integer(input$mi_m); inv <- mod_inv(a, m)
    if (is.na(inv)) output$mi_out <- renderText("No modular inverse (numbers not coprime).")
    else output$mi_out <- renderText(sprintf("%d^(-1) mod %d = %d", a, m, inv))
  })

  # Topological Sort
  observeEvent(input$ts_run, {
    e <- parse_edges_unweighted(input$ts_edges); ord <- topo_kahn(e)
    output$ts_out <- renderText(if (is.null(ord)) "Cycle detected (no topological order)." else paste(ord, collapse = " -> "))
  })
}

shinyApp(ui, server)
