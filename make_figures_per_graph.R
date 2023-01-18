# Run as: Rscript run_experiment.R <graph_nr> <iter> <n_obs> <pos_mode> <indep_mode>

args <- commandArgs(trailingOnly = TRUE)

graph_nr <- as.numeric(args[1])
n_iter <- as.numeric(args[2])
n <- as.numeric(args[3])
pos_mode <- as.character(args[4])
indep_mode <- as.character(args[5])

start <- Sys.time()

cat("\nStarting make_figures_per_graph.R", args, "at", format(start), "\n")

source("experiment.R")

fig_outfolder <- sprintf("./output/figures/graph_%s_%s_%s", n, pos_mode, indep_mode)
dir.create(fig_outfolder, showWarnings = FALSE)
file.copy("output/figures/.gitignore", fig_outfolder)

pdf(sprintf("%s/graph_%s_%s.pdf", fig_outfolder, graph_nr, n_iter), width = 7, height = 4)
for (iter in 1:n_iter) {
    experiment(graph_nr, iter, n, pos_mode = pos_mode, indep_mode = indep_mode, plot_flag = TRUE)
}
dev.off()

end <- Sys.time()

cat(
    "\nFinished make_figures_per_graph.R", args, "at", format(end),
    "in", format(end - start), "\n"
)
