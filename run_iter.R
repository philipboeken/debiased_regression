
# Run as: Rscript run_iter.R <iter> <n_obs>

args <- commandArgs(trailingOnly = TRUE)

iter <- as.numeric(args[1])
n <- as.numeric(args[2])

start <- Sys.time()

cat("\nStarting run_iter.R", args, "at", format(start), "\n")

source("experiment.R")

for (graph_nr in 1:27) {
    mse_result <- experiment(graph_nr, iter, n)
    outfile <- sprintf("output/mse_results/mse_result_%s_%s_%s", graph_nr, iter, n)
    save(mse_result, file = sprintf("%s.RData", outfile))
}

end <- Sys.time()

cat(
    "\nFinished run_iter.R", args, "at", format(end),
    "in", format(end - start), "\n"
)