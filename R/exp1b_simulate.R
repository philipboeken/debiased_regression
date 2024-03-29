# Run as: Rscript exp1b_simulate.R <iter> <n_obs> <pos_mode=pos> <indep_mode=indep> <graph_known=0>

source("R/utils.R")
source("R/experiment1.R")

iter <- get_arg_numeric(1, 1)
n_iter <- get_arg_numeric(2, 2)
n <- get_arg_numeric(3, 500)
pos_mode <- get_arg_character(4, "npos")
indep_mode <- get_arg_character(5, "indep")
graph_known <- get_arg_logical(6, FALSE)

start <- Sys.time()
cat("\nStarting expb1_simulate.R", c(iter, n, pos_mode, indep_mode, graph_known), "at", format(start), "\n")

mse_outfolder <- sprintf("data/exp1/results_%s_%s_%s_%s_%s", n_iter, n, pos_mode, indep_mode, graph_known)
dir.create(mse_outfolder, showWarnings = FALSE)

load("data/exp1/valid_admgs.RData")
for (graph_nr in 1:nrow(valid_graphs)) {
  mse_result <- experiment1(graph_nr, iter, n, pos_mode, indep_mode, graph_known)
  outfile <- sprintf("%s/mse_result_%s_%s", mse_outfolder, graph_nr, iter)
  save(mse_result, file = sprintf("%s.RData", outfile))
}

end <- Sys.time()
cat(
  "\nFinished expb1_simulate.R", c(iter, n, pos_mode, indep_mode, graph_known), "at", format(end),
  "in", format(end - start), "\n"
)
