# Run as: Rscript run_iter.R <iter> <n_obs> <pos_mode> <indep_mode>

args <- commandArgs(trailingOnly = TRUE)

iter <- as.numeric(args[1])
n <- as.numeric(args[2])
pos_mode <- as.character(args[3])
indep_mode <- as.character(args[4])

start <- Sys.time()

cat("\nStarting run_iter.R", args, "at", format(start), "\n")

source("experiment.R")

mse_outfolder <- sprintf("./output/mse_results_%s_%s_%s", n, pos_mode, indep_mode)
dir.create(mse_outfolder, showWarnings = FALSE)
file.copy("output/figures/.gitignore", mse_outfolder)

for (graph_nr in 1:27) {
    mse_result <- experiment(graph_nr, iter, n, pos_mode, indep_mode)
    outfile <- sprintf("%s/mse_result_%s_%s_%s", mse_outfolder, graph_nr, iter, n)
    save(mse_result, file = sprintf("%s.RData", outfile))
}

end <- Sys.time()

cat(
    "\nFinished run_iter.R", args, "at", format(end),
    "in", format(end - start), "\n"
)
