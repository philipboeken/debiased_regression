# Run as: Rscript plot_pos_vs_indep.R <graph_nr> <iter> <n_obs>

source("experiment.R")

args <- commandArgs(trailingOnly = TRUE)

graph_nr <- as.numeric(args[1])
iter <- as.numeric(args[2])
n <- as.numeric(args[3])
graph_known <- as.integer(args[4])
graph_known <- as.logical(if(is.na(graph_known)) 0 else graph_known)

pdf(sprintf("output/figures/pos_indep_grid/figure_%s_%s_%s_%s.pdf", graph_nr, iter, n, graph_known), width = 15, height = 10)
par(mfrow = c(3, 3))
for (pos_mode in c("pos", "wpos", "npos")) {
  for (indep_mode in c("indep", "wdep", "dep")) {
    experiment(graph_nr = graph_nr, iter = iter, n = n, 
               pos_mode = pos_mode, indep_mode = indep_mode, 
               graph_known = graph_known, plot_flag = TRUE)
  }
}
dev.off()
