source("R/utils.R")

# NB directly copied from pcalg R/isValidGraph.R
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

find_valid_graphs <- function() {
  library(pbapply)
  library(pcalg)

  opts <- expand.grid(data.frame(replicate(16, c(0, 1))))

  valid <- pbsapply(1:nrow(opts), function(i) {
    amat <- matrix(as.numeric(opts[i, ]), nrow = 4)
    colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
    amat <- admg_to_dag(amat)
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

plot_graphs <- function(graphs) {
  library(qgraph)
  for (i in 1:nrow(graphs)) {
    amat <- matrix(as.numeric(graphs[i, ]), nrow = 4)
    colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
    qgraph(t(amat),
      vsize = 30, label.cex = 1.4, esize = 4, asize = 14,
      layout = matrix(c(0, 1, 1, 0, 0, 0, -1, -1), nrow = 4),
      mar = c(8, 8, 8, 8), edge.color = "black",
      bidirectional = TRUE
    )
  }
}

start <- Sys.time()
cat("\nStarting exp1a_find_valid_graphs.R", "at", format(start), "\n")

if (!file.exists("data/exp1/valid_graphs.RData")) {
  valid_graphs <- find_valid_graphs()
  save(valid_graphs, file = "data/exp1/valid_graphs.RData")
} else {
  load("data/exp1/valid_graphs.RData")
}

graph_ranges <- get_graph_ranges()
graph_ranges[['x_to_s']] <- NULL

filenames <- sprintf("output/figures/exp1/pmar_%s.pdf", names(graph_ranges))

for (i in 1:length(graph_ranges)) {
  graph_range <- graph_ranges[[i]]
  width <- 8
  height <- ceiling(length(graph_range) / width)
  filename <- filenames[i]
  if (!file.exists(filename)) {
    pdf(filename, width = width, height = height)
    par(mfrow = c(height, width))
    plot_graphs(valid_graphs[graph_range, ])
    dev.off()
  }
}

end <- Sys.time()
cat("\nFinished exp1a_find_valid_graphs.R", "at", format(end), "in", format(end - start), "\n")
