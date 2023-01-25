source("R/utils.R")

graph_nr <- get_arg_numeric(1)
n_iter <- get_arg_numeric(2)
n <- get_arg_numeric(3)
pos_mode <- get_arg_character(4, "pos")
indep_mode <- get_arg_character(5, "indep")

start <- Sys.time()
cat("\nStarting exp1d_plot_iterations.R", c(graph_nr, n_iter, n, pos_mode, indep_mode), "at", format(start), "\n")

fig_outfolder <- sprintf("output/figures/exp1/graphs_plots", n_iter, n, pos_mode, indep_mode)

pdf(sprintf("%s/graph_%s_%s_%s_%s_%s.pdf", fig_outfolder, graph_nr, n_iter, n, pos_mode, indep_mode), width = 7, height = 4)
for (iter in 1:n_iter) {
    experiment(graph_nr, iter, n, pos_mode = pos_mode, indep_mode = indep_mode, plot_flag = TRUE)
}
dev.off()

end <- Sys.time()
cat(
    "\nFinished exp1d_plot_iterations.R", c(graph_nr, n_iter, n, pos_mode, indep_mode), "at", format(end),
    "in", format(end - start), "\n"
)
