# Run as: Rscript exp1b_simulate.R <iter> <n_obs> <pos_mode=pos> <indep_mode=indep> <graph_known=0>

# TODO:
# - Scale simulated vars to make the MSE have a better scale? Other parameters that make it more general?
# - Check which graphs have X -> S, and for these graphs calculate MSE results for
#   interpolation and extrapolation part. Especially wiht no positivity, this should matter.
# - Also use LightGBM regression, this should extrapolate better for IW regression than GAM.
# - Grid calculation of MSE (MSE-t) for pos and indep in range(0,1)
# - ??? Misspecification of imputation m_{imp} and pi-model m_{pi}, grid calculation of
#               MSE (MSE-t) of DR over these parameters.
#       Also plot m_{imp} vs MSE of RR, and plot m_{pi} vs MSE of IW
# - bidirected edges?

# TODO:
# v Add file for making figures for the main story:
#     v 3d plot waarom imputatie zo goed lukt.
# - Improve naive method using causal vs anticausal, or ssl kernel regression.
# v Pick the best IW clipping method and apply this to Doubly Robust.
#     Still, what direct method do we use for DR?
#       - Use trans_05 as this works best in mse_results_combined_500_1000_pos_indep_FALSE,
#         which is the only setting where IW works better than naive.
# - Test whether one method is better than the other:
#       https://dl.acm.org/doi/pdf/10.1145/1143844.1143862 section 5
# - Lijst maken van conclusies die ik wil trekken
#     - Identify for which graphs any method fails, or where naive has much bias.
#     - Perhaps select only datasets where naive fails.
# v Better tuning of IW (out of the box package,
#     or better clipping: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3069059/)
# v Check if GP regression depends on size of weights
#       It does... There's a threshold, and for
#       every value above this threshold it responds the same, but different as
#       to any value below this threshold.
#     So, mutliply with P(S=1) for completeness sake.
# v We don't simulate e.g. Z -> S by passing Z through a GP and then a sigmoid,
#            as we can then not properly tune positivity

source("R/utils.R")
source("R/experiment1.R")

iter <- get_arg_numeric(1, 1)
n_iter <- get_arg_numeric(2, 20)
n <- get_arg_numeric(3, 500)
pos_mode <- get_arg_character(4, "pos")
indep_mode <- get_arg_character(5, "indep")
graph_known <- get_arg_logical(6, FALSE)

start <- Sys.time()
cat("\nStarting expb1_simulate.R", c(iter, n, pos_mode, indep_mode, graph_known), "at", format(start), "\n")

mse_outfolder <- sprintf("data/exp1/results_%s_%s_%s_%s_%s", n_iter, n, pos_mode, indep_mode, graph_known)
dir.create(mse_outfolder, showWarnings = FALSE)

for (graph_nr in 1:126) {
  mse_result <- experiment1(graph_nr, iter, n, pos_mode, indep_mode, graph_known)
  outfile <- sprintf("%s/mse_result_%s_%s", mse_outfolder, graph_nr, iter)
  save(mse_result, file = sprintf("%s.RData", outfile))
}

end <- Sys.time()
cat(
  "\nFinished expb1_simulate.R", c(iter, n, pos_mode, indep_mode, graph_known), "at", format(end),
  "in", format(end - start), "\n"
)
