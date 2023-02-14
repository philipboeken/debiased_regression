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

simulate_discr <- function(all_data, var, amat, n, pos_mode, indep_mode) {
  parents <- get_parents(var, amat)

  if (length(parents) == 0) {
    all_data$pi <- 1 / 3
  } else {
    all_data$pi <- apply(sapply(parents, function(parent) {
      sigmoid(scale(all_data[, parent]) * 10)
    }), 1, prod)
    # Scale selection probabilities between a lower bound and 1, to control positivity.
    min_prob <- c("pos" = 1 / 20, "wpos" = 1 / 100, "npos" = 0)[pos_mode]
    all_data$pi <- trans_linear(all_data$pi, min_prob, 1)
  }

  if (var == "S" && smaller_top_order("Y", "S", amat)) {
    # Let selection probabilities depend on Y (allowing for no positivity in the Y-direction)
    dep <- c("indep" = 0, "wdep" = 1 / 2, "dep" = 1)[indep_mode]
    all_data$pi <- as.numeric(all_data$pi * sigmoid(scale(all_data$Y) * 10, (1 - dep), 1))
  }

  all_data$S <- runif(n) < all_data$pi

  all_data
}

simulate_cont <- function(all_data, var, amat, n, pos_mode, indep_mode) {
  parents <- get_parents(var, amat)
  eps <- scale(draw_gp(runif(n), kernel_fn = se_kernel, length = 3 / 2))
  if (length(parents) == 0) {
    mu <- numeric(n)
  } else {
    mu <- scale(draw_gp(all_data[, setdiff(parents, "S")], kernel_fn = matern_kernel, nu = 2.5))
    eps <- eps / 2
  }
  all_data[, var] <- as.numeric(mu + eps)

  if ("S" %in% parents) {
    shift <- c("pos" = 1, "wpos" = 2, "npos" = 3)[pos_mode]
    all_data[all_data$S, var] <- all_data[all_data$S, var] - sd(all_data[, var]) * shift
  } else if (var == "Y" && smaller_top_order("S", "Y", amat)) {
    dep <- c("indep" = 0, "wdep" = 1 / 2, "dep" = 1)[indep_mode]
    all_data[all_data$S, var] <- all_data[all_data$S, var] - sd(all_data[, var]) * dep
  }

  all_data
}

simulate_nonlinear <- function(amat, n, seed, pos_mode = "pos", indep_mode = "indep") {
  stopifnot(pos_mode %in% c("pos", "wpos", "npos"))
  stopifnot(indep_mode %in% c("indep", "wdep", "dep"))
  set.seed(seed)
  top_order <- get_top_order(amat)

  all_data <- data.frame(
    X = numeric(n),
    Y = numeric(n),
    Z = numeric(n),
    S = numeric(n)
  )

  while (sum(all_data$S) < 50) {
    for (var in top_order) {
      if (var == "S") {
        all_data <- simulate_discr(all_data, var, amat, n, pos_mode, indep_mode)
      } else {
        all_data <- simulate_cont(all_data, var, amat, n, pos_mode, indep_mode)
      }
    }

    if (sum(all_data$S) < 50) {
      warning("Less than 50 observations selected, we're going to resample.")
    }
  }

  return(all_data)
}

experiment1 <- function(
    graph_nr, iter, n = 900,
    pos_mode = "pos", indep_mode = "indep",
    graph_known = FALSE, plot_flag = FALSE) {
  seed <- 100000 * graph_nr + iter

  amat <- get_graph(graph_nr)
  amat <- admg_to_dag(amat)

  all_data <- simulate_nonlinear(amat, round((3 / 2) * n), seed, pos_mode, indep_mode)
  train_idx <- (1:(2 * n)) %in% sample(1:(2 * n), round((2 / 3) * n))
  train_data <- all_data[train_idx, ]
  test_data <- all_data[!train_idx, ]
  test_data <- cbind_predictions(
    test_data = test_data, train_data = train_data,
    pi_model_data = all_data, imputation_model_data = all_data,
    graph_known = graph_known, amat = amat
  )

  if (plot_flag) {
    plot_results(test_data)
  }

  mse_result <- get_mse_result(test_data)

  return(mse_result)
}

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
