library(pcalg)
library(pbapply)
library(qgraph)

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


if (file.exists("data/valid_graphs.RData")) {
  load("data/valid_graphs.RData")
} else {
  valid_graphs <- data.frame()

  opts <- expand.grid(data.frame(replicate(16, c(0, 1))))

  pbsapply(1:nrow(opts), function(i) {
    amat <- matrix(as.numeric(opts[i, ]), nrow = 4)
    colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
    g <- as(t(amat), "graphNEL")
    john.pairs <- RBGL::johnson.all.pairs.sp(g)
    if (!isValidGraph(amat, type = "dag")) {
      return()
    }
    if (sum(amat[, "S"]) > 0) {
      return()
    }
    if (dsep("X", "Y", g = g, john.pairs = john.pairs) ||
      dsep("Y", "S", g = g, john.pairs = john.pairs) ||
      dsep("Y", "S", "X", g = g, john.pairs = john.pairs) ||
      !dsep("Y", "S", c("X", "Z"), g = g, john.pairs = john.pairs)) {
      return()
    }
    valid_graphs <<- rbind(valid_graphs, data.frame(opts[i, ]))
  })
  save(valid_graphs, file = "data/valid_graphs.RData")
}


pdf("./output/figures/all_dags.pdf", width = 7, height = 4)
par(mfrow = c(4, 7))
for (i in 1:nrow(valid_graphs)) {
  amat <- matrix(as.numeric(valid_graphs[i, ]), nrow = 4)
  colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
  qgraph(t(amat),
    vsize = 30, label.cex = 1.4, esize = 4, asize = 14,
    layout = matrix(c(0, 1, 1, 0, 0, 0, -1, -1), nrow = 4),
    mar = c(8, 8, 8, 8), edge.color = "black"
  )
}
dev.off()
