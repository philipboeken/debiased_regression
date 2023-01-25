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
  valid_graphs <- data.frame()

  opts <- expand.grid(data.frame(replicate(16, c(0, 1))))

  pbsapply(1:nrow(opts), function(i) {
    amat <- matrix(as.numeric(opts[i, ]), nrow = 4)
    colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
    g <- as(t(amat), "graphNEL")
    if (!isValidGraph(amat, type = "dag")) {
      return()
    }
    # Uncomment this to only find graphs where S is a sink-node
    # if (sum(amat[, "S"]) > 0) return()
    john.pairs <- RBGL::johnson.all.pairs.sp(g)
    if (!dsep("X", "Y", g = g, john.pairs = john.pairs) &&
      !dsep("Y", "S", g = g, john.pairs = john.pairs) &&
      !dsep("Y", "S", "X", g = g, john.pairs = john.pairs) &&
      dsep("Y", "S", c("X", "Z"), g = g, john.pairs = john.pairs)) {
      valid_graphs <<- rbind(valid_graphs, data.frame(opts[i, ]))
    }
  })

  valid_graphs
}

plot_graphs <- function(graphs) {
  library(qgraph)
  for (i in 1:nrow(graphs)) {
    amat <- matrix(as.numeric(graphs[i, ]), nrow = 4)
    colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
    qgraph(t(amat),
      vsize = 30, label.cex = 1.4, esize = 4, asize = 14,
      layout = matrix(c(0, 1, 1, 0, 0, 0, -1, -1), nrow = 4),
      mar = c(8, 8, 8, 8), edge.color = "black"
    )
  }
}

if (!file.exists("data/exp1/valid_graphs.RData")) {
  valid_graphs <- find_valid_graphs()
  save(valid_graphs, file = "data/exp1/valid_graphs.RData")
} else {
  load("data/exp1/valid_graphs.RData")
}


filename <- "output/figures/exp1/all_valid_graphs.pdf"
if (!file.exists(filename)) {
  pdf(filename, width = 7, height = 4)
  par(mfrow = c(4, 7))
  # In graphs 1:27 S is a sink node, in graphs 28:51 S is not a sink node
  for (graphs_range in list(1:27, 28:51)) {
    plot_graphs(valid_graphs[graphs_range, ])
    par(mfrow = c(4, 7))
  }
  dev.off()
}
