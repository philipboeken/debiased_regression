# Run as: Rscript plot_pos_vs_indep.R <graph_nr> <iter> <n_obs>

load("experiment.R")

args <- commandArgs(trailingOnly = TRUE)

graph_nr <- as.numeric(args[1])
iter <- as.numeric(args[2])
n <- as.numeric(args[3])

pdf(sprintf("output/figures/pos_indep_grid/figure_%s_%s_%s.pdf", graph_nr, iter, n), width = 15, height = 10)
par(mfrow = c(3, 3))
for (pos_mode in c("pos", "wpos", "npos")) {
  for (indep_mode in c("indep", "wdep", "dep")) {
    print(experiment(graph_nr = graph_nr, iter = iter, n = n, pos_mode = pos_mode, indep_mode = indep_mode, plot_flag = TRUE))
  }
}
dev.off()
