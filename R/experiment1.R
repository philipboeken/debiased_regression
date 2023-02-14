source("R/utils.R")

simulate_discr <- function(all_data, var, amat, n, pos_mode, indep_mode) {
  parents <- get_parents(var, amat)

  if (length(parents) == 0) {
    all_data$pi <- 1 / 3
  } else {
    all_data$pi <- apply(sapply(parents, function(parent) {
      sigmoid(scale(all_data[, parent]) * 20)
    }), 1, prod)
    # Scale selection probabilities between a lower bound and 1, to control positivity.
    min_prob <- c("pos" = 1 / 20, "wpos" = 1 / 100, "npos" = 0)[pos_mode]
    all_data$pi <- trans_linear(all_data$pi, min_prob, 1)
  }

  if (var == "S" && smaller_top_order("Y", "S", amat)) {
    # Let selection probabilities depend on Y (allowing for no positivity in the Y-direction)
    dep <- c("indep" = 0, "wdep" = 1 / 2, "dep" = 1)[indep_mode]
    all_data$pi <- as.numeric(all_data$pi * sigmoid(scale(all_data$Y) * 20, (1 - dep), 1))
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

  all_data <- simulate_nonlinear(amat, 2*n, seed, pos_mode, indep_mode)
  train_idx <- (1:(2*n)) %in% sample(1:(2*n), n)
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