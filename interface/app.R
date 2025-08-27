# Minimal Shiny UI for portfolio algorithms
library(shiny); library(DT)

root <- normalizePath("..", winslash = "/")
source(file.path(root,"01_data_structures/segment_tree.R"))
source(file.path(root,"01_data_structures/union_find.R"))
source(file.path(root,"02_algorithms/dp_pathfinding.R"))
source(file.path(root,"03_graphs/dijkstra_graph_solver.R"), local=TRUE)
if (file.exists(file.path(root,"03_graphs/topological_sort.R")))
  source(file.path(root,"03_graphs/topological_sort.R"))
source(file.path(root,"04_number_theory/modular_inverse_calculator.R"), local=TRUE)

parse_num_vec <- function(txt){
  nums <- gsub("\\s+","",txt)
  if (nzchar(nums)) as.numeric(strsplit(nums,",")[[1]]) else numeric(0)
}
parse_edges <- function(txt){
  lines <- strsplit(trimws(txt), "\\n")[[1]]
  if (!length(lines) || (length(lines) == 1L && lines == ""))
    return(data.frame(from=character(),to=character(),weight=numeric(),stringsAsFactors=FALSE))
  parts <- strsplit(lines, "\\s+")
  d <- do.call(rbind, lapply(parts, function(p){
    if (length(p) < 2) return(c(NA,NA,NA))
    c(p[1], p[2], ifelse(length(p) >= 3, p[3], "1"))
  }))
  data.frame(from=d[,1], to=d[,2], weight=as.numeric(d[,3]), stringsAsFactors=FALSE)
}

ui <- fluidPage(
  titlePanel("R Code Reasoning & Benchmarks — Interactive Demos"),
  tabsetPanel(
    tabPanel("Segment Tree",
      fluidRow(
        column(4, textInput("st_vec","Initial vector", value="1,2,3,4,5,6,7,8"), actionButton("st_build","Build")),
        column(4, numericInput("st_q_l","Query L",1,1), numericInput("st_q_r","Query R",4,1), actionButton("st_query","Range Sum")),
        column(4, numericInput("st_u_idx","Update idx",3,1), numericInput("st_u_val","New value",10), actionButton("st_update","Update"))
      ),
      verbatimTextOutput("st_status")
    ),
    tabPanel("Union-Find",
      fluidRow(column(4, numericInput("uf_n","Nodes",6,1)),
               column(8, textInput("uf_ops","Unions a-b (comma-separated)","1-2, 2-3, 4-5"))),
      actionButton("uf_run","Apply"), DTOutput("uf_tbl")
    ),
    tabPanel("Dijkstra",
      fluidRow(
        column(8, textAreaInput("dj_edges","Edges (from to w)", rows=6,
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
      fluidRow(column(3, numericInput("dp_m","Rows",3,1)),
               column(3, numericInput("dp_n","Cols",3,1)),
               column(3, actionButton("dp_gen","Random Grid")),
               column(3, actionButton("dp_run","Compute"))),
      DTOutput("dp_grid"), verbatimTextOutput("dp_out")
    ),
    tabPanel("Modular Inverse",
      fluidRow(column(4, numericInput("mi_a","a",3)),
               column(4, numericInput("mi_m","m",11)),
               column(4, actionButton("mi_run","Compute"))),
      verbatimTextOutput("mi_out")
    ),
    tabPanel("Topological Sort",
      fluidRow(column(8, textAreaInput("ts_edges","Edges (from to)", rows=5,
"A B
A C
B D
C D")),
               column(4, actionButton("ts_run","Run"))),
      verbatimTextOutput("ts_out")
    )
  )
)

server <- function(input, output, session){
  # Segment tree
  st <- reactiveVal(NULL)
  observeEvent(input$st_build,{
    v <- parse_num_vec(input$st_vec); req(length(v)>0)
    st(build_segment_tree(v))
    output$st_status <- renderText("Built")
  })
  observeEvent(input$st_query,{
    req(!is.null(st()))
    res <- st()$query(as.integer(input$st_q_l), as.integer(input$st_q_r))
    output$st_status <- renderText(paste("Sum =",res))
  })
  observeEvent(input$st_update,{
    req(!is.null(st()))
    st()$update(as.integer(input$st_u_idx), as.numeric(input$st_u_val))
    output$st_status <- renderText("Updated")
  })

  # Union-Find
  observeEvent(input$uf_run,{
    n <- as.integer(input$uf_n); uf <- union_find_new(n)
    ops <- gsub("\\s+","",input$uf_ops)
    if (nzchar(ops)) for (p in strsplit(ops,",")[[1]]) {
      ab <- strsplit(p,"-")[[1]]
      if (length(ab)==2) uf$union(as.integer(ab[1]), as.integer(ab[2]))
    }
    comp <- data.frame(node=1:n, parent=sapply(1:n, uf$find))
    output$uf_tbl <- renderDT(datatable(comp, options=list(pageLength=10)))
  })

  # Dijkstra
  observeEvent(input$dj_run,{
    edges <- parse_edges(input$dj_edges); req(nrow(edges)>0)
    d <- dijkstra_sssp(edges, input$dj_src)
    output$dj_tbl <- renderDT(datatable(data.frame(node=names(d), dist=as.numeric(d))))
  })

  # DP Min Path
  grid <- reactiveVal(matrix(sample(1:9,9,TRUE),3,3))
  output$dp_grid <- renderDT(datatable(as.data.frame(grid()), options=list(dom='t'), rownames=FALSE))
  observeEvent(input$dp_gen,{
    m <- as.integer(input$dp_m); n <- as.integer(input$dp_n)
    grid(matrix(sample(1:9, m*n, TRUE), m, n))
    output$dp_grid <- renderDT(datatable(as.data.frame(grid()), options=list(dom='t'), rownames=FALSE))
  })
  observeEvent(input$dp_run,{
    g <- grid(); f <- if (exists("min_path_sum")) min_path_sum else NULL; req(!is.null(f))
    output$dp_out <- renderText(paste("Min path sum:", f(g)))
  })

  # Modular inverse
  observeEvent(input$mi_run,{
    f <- if (exists("modinv")) modinv else if (exists("modular_inverse")) modular_inverse else NULL
    req(!is.null(f))
    output$mi_out <- renderText(paste("Inverse:", f(as.integer(input$mi_a), as.integer(input$mi_m))))
  })

  # Topological sort
  observeEvent(input$ts_run,{
    if (!exists("topological_sort")) { output$ts_out <- renderText("topological_sort.R not found."); return(NULL) }
    edges <- parse_edges(input$ts_edges)
    ord <- topological_sort(edges)
    output$ts_out <- renderText(if (is.null(ord)) "Cycle detected" else paste(ord, collapse=" -> "))
  })
}

shinyApp(ui, server)
