source("R/utils.R")

# NB fix for pcalg R/isValidGraph.R
noCycles <- function(amat) {
  ok <- TRUE
  for (i in 1:length(amat[1, ])) {
    pa.i <- which(amat[i, ] != 0 & amat[, i] == 0)
    if (length(pa.i) != 0) {
      for (j in 1:length(pa.i)) {
        pos.anc <- setdiff(possAn(
          m = amat, x = pa.i[j],
          ds = FALSE, possible = TRUE
        ), pa.i[j])
        if (i %in% pos.anc) {
          ok <- FALSE
        }
      }
    }
  }
  ok
}

find_valid_dags <- function(allow_biarr = FALSE) {
  library(pbapply)
  library(pcalg)

  opts <- expand.grid(data.frame(replicate(16, c(0, 1))))

  valid <- pbsapply(1:nrow(opts), function(i) {
    amat <- vector_to_amat(opts[i, ])
    if (!allow_biarr && !all(amat + t(amat) <= 1)) {
      return(FALSE)
    }
    if (!isValidGraph(amat, type = "dag")) {
      return(FALSE)
    }
    g <- as(t(amat), "graphNEL")
    john.pairs <- RBGL::johnson.all.pairs.sp(g)
    return(!dsep("X", "Y", g = g, john.pairs = john.pairs) &&
      !dsep("Y", "S", "X", g = g, john.pairs = john.pairs) &&
      dsep("Y", "S", c("X", "Z"), g = g, john.pairs = john.pairs))
  })

  return(opts[valid, ])
}

find_valid_admgs <- function() {
  valid_graphs <- find_valid_dags(allow_biarr = TRUE)
  results <- data.frame()
  vars <- c("X", "Y", "Z", "S")
  combs <- get_all_confounders(vars)
  ord <- c(vars, combs)
  pbsapply(1:nrow(valid_graphs), function(i) {
    amat <- admg_to_dag(vector_to_amat(valid_graphs[i, ]))
    colns <- colnames(amat)
    confs <- colns[sapply(colns, function(s) startsWith(s, "C"))]
    confs_to_check <- setdiff(combs, confs)
    amat <- cbind(amat, replicate(length(confs_to_check), numeric(nrow(amat))))
    amat <- rbind(amat, t(replicate(length(confs_to_check), numeric(ncol(amat)))))
    colnames(amat) <- rownames(amat) <- c(colns, confs_to_check)
    amat <- amat[ord, ord]
    results <<- rbind(results, as.vector(amat))
    opts_cnf <- do.call(data.table::CJ, replicate(length(confs_to_check), c(0, 1), simplify = FALSE))[-1, ]
    colnames(opts_cnf) <- confs_to_check
    for (j in 1:nrow(opts_cnf)) {
      amat_ <- amat
      for (cnf in confs_to_check[as.logical(opts_cnf[j, ])]) {
        amat_ <- activate_confounder(cnf, amat_)
      }
      g <- as(t(amat_), "graphNEL")
      john.pairs <- RBGL::johnson.all.pairs.sp(g)
      if (!dsep("X", "Y", g = g, john.pairs = john.pairs) &&
        !dsep("Y", "S", "X", g = g, john.pairs = john.pairs) &&
        dsep("Y", "S", c("X", "Z"), g = g, john.pairs = john.pairs)) {
        results <<- rbind(results, as.vector(amat_))
      }
    }
  })

  return(results)
}

dag_to_edgelist <- function(amat) {
  colns <- colnames(amat)
  confs <- colns[startsWith(colns, "C")]
  edges <- NA
  for (var in setdiff(colns, confs)) {
    edges <- rbind(edges, unname(as.matrix(expand.grid(var, colns[as.logical(amat[, var])]))))
  }
  edges <- unname(cbind(edges[-1, ], F))
  for (conf in confs) {
    if (sum(amat[, conf]) != 0) {
      confounded <- colns[as.logical(amat[, conf])]
      edges <- rbind(
        edges,
        c(sort(confounded, T), T),
        c(sort(confounded, F), T)
      )
    }
  }
  edges
}

# plot_graphs <- function(graphs) {
#   library(qgraph)
#   for (i in 1:nrow(graphs)) {
#     amat <- matrix(as.numeric(graphs[i, ]), nrow = 4)
#     colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
#     qgraph(t(amat),
#       vsize = 30, label.cex = 1.4, esize = 4, asize = 14,
#       layout = matrix(c(0, 1, 1, 0, 0, 0, -1, -1), nrow = 4),
#       mar = c(8, 8, 8, 8), edge.color = "black",
#       bidirectional = TRUE
#     )
#   }
# }

plot_graphs <- function(graphs) {
  library(qgraph)
  for (i in 1:length(graphs)) {
    amat <- graphs[[i]]
    edgelist <- dag_to_edgelist(amat)
    ord <- order(edgelist[, 3], decreasing = T)
    edgelist <- edgelist[ord, ]
    layt <- matrix(c(0, 1, 1, 0, 0, 0, -1, -1), nrow = 4)
    rownames(layt) <- c("X", "Y", "Z", "S")
    qgraph(edgelist[, c(1, 2)],
      directed = T, bidirectional = as.logical(edgelist[, 3]),
      # vsize = 30, label.cex = 1.4, esize = 4, asize = 14,
      layout = layt[unique(as.vector(edgelist[, c(1, 2)])), ],
      # mar = c(4, 4, 4, 4),
      edge.color = "black"
    )
  }
}

start <- Sys.time()
cat("\nStarting exp1a_find_valid_graphs.R", "at", format(start), "\n")

cat("\nFinding valid ADMGs:", "\n")
if (!file.exists("data/exp1/valid_admgs.RData")) {
  valid_graphs <- find_valid_admgs()
  save(valid_graphs, file = "data/exp1/valid_admgs.RData")
} else {
  load("data/exp1/valid_admgs.RData")
}

# graph_ranges <- get_graph_ranges()
# graph_ranges[["x_to_s"]] <- NULL
#
# filenames <- sprintf("output/figures/exp1/valid_graphs/pmar_%s.pdf", names(graph_ranges))
#
# for (i in 1:length(graph_ranges)) {
#   graph_range <- graph_ranges[[i]]
#   if(!length(graph_range)) next
#   width <- 8
#   height <- ceiling(length(graph_range) / width)
#   filename <- filenames[i]
#   if (!file.exists(filename)) {
#     pdf(filename, width = width, height = height)
#     par(mfrow = c(height, width))
#     plot_graphs(valid_graphs[graph_range, ])
#     dev.off()
#   }
# }

end <- Sys.time()
cat("\nFinished exp1a_find_valid_graphs.R", "at", format(end), "in", format(end - start), "\n")
