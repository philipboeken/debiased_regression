source("R/utils.R")

graph_nr <- get_arg_numeric(1)
iter <- get_arg_numeric(2)
n <- get_arg_numeric(3)
graph_known <- get_arg_logical(4, FALSE)

start <- Sys.time()
cat("\nStarting exp1e_compare_pos_indep.R", c(graph_nr, iter, n, graph_known), "at", format(start), "\n")

pdf(
  sprintf(
    "output/figures/exp1/compare_pos_indep/compare_%s_%s_%s_%s.pdf",
    graph_nr, iter, n, graph_known
  ),
  width = 15, height = 10
)
par(mfrow = c(3, 3))
for (pos_mode in c("pos", "wpos", "npos")) {
  for (indep_mode in c("indep", "wdep", "dep")) {
    experiment(
      graph_nr = graph_nr, iter = iter, n = n,
      pos_mode = pos_mode, indep_mode = indep_mode,
      graph_known = graph_known, plot_flag = TRUE
    )
  }
}
dev.off()

end <- Sys.time()
cat(
    "\nFinished exp1e_compare_pos_indep.R", c(graph_nr, iter, n, graph_known), "at", format(end),
    "in", format(end - start), "\n"
)
