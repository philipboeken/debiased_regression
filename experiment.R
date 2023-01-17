source("gp_draw.R")
library(mgcv)

# TODO:
# v Better tuning of IPW (out of the box package, or better clipping: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3069059/)
# v Check if GP regression depends on size of weights
#       It does... There's a threshold, and for
#       every value above this threshold it responds the same, but different as
#       to any value below this threshold.
#     So, mutliply with P(S=1) for completeness sake.
# v Regress only on dependent vars for P(S=1|X, Z) and E[Y| X, Z, S=1]
# - Identify for which graphs any method fails, or where naive has much bias.
# - Perhaps select only datasets where naive fails.
# - Improve naive method using causal vs anticausal, or ssl kernel regression.
# v Find other non-parametric weighted regression methods

sigmoid <- function(x, ymin = 0, ymax = 1) {
  1 / (1 + exp(x)) * (ymax - ymin) + ymin
}

mse <- function(y, yhat, weights = replicate(length(y), 1)) {
  stopifnot(length(y) == length(yhat))
  stopifnot(length(yhat) == length(weights))
  sum(weights * (y - yhat)^2) / sum(weights)
}

clip_lower_quantile <- function(probs, q) {
  min_value <- quantile(probs, q)
  probs[probs <= min_value] <- min_value
  probs
}

translate_between_values <- function(probs, min_value, max_value = 1) {
  (probs / max(probs)) * (max_value - min_value) + min_value
}

get_graph <- function(graph_nr) {
  load("data/valid_graphs.RData")
  amat <- matrix(as.numeric(valid_graphs[graph_nr, ]), nrow = 4)
  colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
  amat
}

plot_graph <- function(amat) {
  qgraph::qgraph(t(amat),
    vsize = 30, label.cex = 1.4, esize = 4, asize = 14,
    layout = matrix(c(0, 1, 1, 0, 0, 0, -1, -1), nrow = 4),
    mar = c(8, 8, 8, 8), edge.color = "black"
  )
}

get_parents <- function(var, amat) {
  colnames(amat)[amat[var, ] == 1]
}

get_roots <- function(amat) {
  is_root <- Reduce(
    function(a, b) a & b,
    lapply(colnames(amat), function(var) amat[, var] == 0)
  )
  names(amat)[is_root]
}

get_top_order <- function(amat) {
  amat <- data.frame(amat)
  top_order <- c()
  while (nrow(amat) > 0) {
    roots <- get_roots(amat)
    top_order <- c(top_order, roots)
    remain <- setdiff(colnames(amat), roots)
    if (length(remain) == 1) {
      top_order <- c(top_order, remain)
      remain <- c()
    }
    amat <- amat[remain, remain]
  }
  return(top_order)
}

simulate_nonlinear <- function(amat, n, seed) {
  set.seed(seed)
  top_order <- get_top_order(amat)
  top_order <- top_order[top_order != "S"]

  all_data <- data.frame(
    X = numeric(n),
    Y = numeric(n),
    Z = numeric(n),
    S = numeric(n)
  )

  while (sum(all_data$S) < 50) {
    for (var in top_order) {
      parents <- get_parents(var, amat)
      if (length(parents) == 0) {
        mu <- numeric(n)
        eps <- runif(n, -2, 2)
        eps <- eps / (2 * sd(eps))
      } else {
        mu <- draw_gp(all_data[, parents], kernel_fn = matern_kernel, nu = 2.5)
        eps <- 4 * draw_gp(matrix(runif(n)), kernel_fn = se_kernel, length = 3 / 2)
        eps <- sd(mu) * eps / (2 * sd(eps))
      }
      all_data[, var] <- mu + eps
    }

    S_parents <- get_parents("S", amat)
    all_data$pi <- apply(sapply(S_parents, function(parent) {
      # sigmoid(scale(all_data[, parent]) * 10, 1 / 20, 1)
      sigmoid(scale(all_data[, parent]) * 10, 0, 1)
    }), 1, prod)
    all_data$S <- runif(n) < all_data$pi

    if (sum(all_data$S) < 50) {
      warning("Less than 50 observations selected, we're going to resample.")
    }
  }

  return(all_data)
}

cbind_true <- function(all_data) {
    # 'True' model as if we have observed all data
    true_model <- gam(Y ~ s(X, bs = "tp"), data = all_data)
    all_data$yhat_true <- predict(true_model, data.frame(X=all_data$X))
    
    return(all_data)
}

cbind_naive <- function(all_data) {
  selected_data <- all_data[all_data$S, ]

  # Naive model directly trained on observed data
  naive_model <- gam(Y ~ s(X, bs = "tp"), data = selected_data)
  all_data$yhat_naive <- predict(naive_model, data.frame(X=all_data$X))
  
  return(all_data)
}

cbind_recursive <- function(all_data, amat) {
  selected_data <- all_data[all_data$S, ]

  # Direct recursive (imputed) with gam
  if ("Y" %in% get_parents("X", amat) || "X" %in% get_parents("Y", amat)) {
    imputation_model <- gam(Y ~ s(X, Z, bs = "tp"), data = selected_data)
  } else {
    imputation_model <- gam(Y ~ s(Z, bs = "tp"), data = selected_data)
  }
  all_data$y_imputed <- predict(imputation_model, data.frame(X=all_data$X, Z=all_data$Z))
  all_data$y_mix <- all_data$y_imputed
  all_data$y_mix[all_data$S] <- selected_data$Y

  recursive_model <- gam(y_imputed ~ s(X, bs = "tp"), data = all_data)
  all_data$yhat_recursive <- predict(recursive_model, data.frame(X=all_data$X))

  recursive_model_mix <- gam(y_mix ~ s(X, bs = "tp"), data = all_data)
  all_data$yhat_recursive_mix <- predict(recursive_model_mix, data.frame(X=all_data$X))

  return(all_data)
}

cbind_ipw <- function(all_data, amat) {
  selected_data <- all_data[all_data$S, ]

  # Estimate pi and calculate weights
  if ("X" %in% get_parents("S", amat)) {
    pi_model <- gam(S ~ s(X, Z, bs = "tp"), family = binomial(link = "logit"), data = all_data)
  } else {
    pi_model <- gam(S ~ s(Z, bs = "tp"), family = binomial(link = "logit"), data = all_data)
  }
  all_data$pi_hat <- pi_model$fitted.values
  p_s <- sum(all_data$S) / n
  all_data$weights_est <- p_s / all_data$pi_hat
  all_data$weights_est_clip_05 <- p_s / clip_lower_quantile(all_data$pi_hat, 0.05)
  all_data$weights_est_clip_1 <- p_s / clip_lower_quantile(all_data$pi_hat, 0.1)
  all_data$weights_est_clip_25 <- p_s / clip_lower_quantile(all_data$pi_hat, 0.25)
  all_data$weights_est_trans_05 <- p_s / translate_between_values(all_data$pi_hat, 0.05, 1)
  all_data$weights_est_trans_1 <- p_s / translate_between_values(all_data$pi_hat, 0.1, 1)
  all_data$weights_est_trans_25 <- p_s / translate_between_values(all_data$pi_hat, 0.25, 1)
  all_data$weights_true <- p_s / all_data$pi
  all_data$weights_true_clip_05 <- p_s / clip_lower_quantile(all_data$pi, 0.05)
  all_data$weights_true_clip_1 <- p_s / clip_lower_quantile(all_data$pi, 0.1)
  all_data$weights_true_clip_25 <- p_s / clip_lower_quantile(all_data$pi, 0.25)
  all_data$weights_true_trans_05 <- p_s / translate_between_values(all_data$pi, 0.05, 1)
  all_data$weights_true_trans_1 <- p_s / translate_between_values(all_data$pi, 0.1, 1)
  all_data$weights_true_trans_25 <- p_s / translate_between_values(all_data$pi, 0.25, 1)
  selected_data <- all_data[all_data$S, ]

  # IPW with estimated weights
  ipw_model_est <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_est)
  ipw_model_est_clip_05 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_est_clip_05)
  ipw_model_est_clip_1 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_est_clip_1)
  ipw_model_est_clip_25 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_est_clip_25)
  ipw_model_est_trans_05 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_est_trans_05)
  ipw_model_est_trans_1 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_est_trans_1)
  ipw_model_est_trans_25 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_est_trans_25)
  all_data$yhat_ipw_est <- predict(ipw_model_est, data.frame(X=all_data$X))
  all_data$yhat_ipw_est_clip_05 <- predict(ipw_model_est_clip_05, data.frame(X=all_data$X))
  all_data$yhat_ipw_est_clip_1 <- predict(ipw_model_est_clip_1, data.frame(X=all_data$X))
  all_data$yhat_ipw_est_clip_25 <- predict(ipw_model_est_clip_25, data.frame(X=all_data$X))
  all_data$yhat_ipw_est_trans_05 <- predict(ipw_model_est_trans_05, data.frame(X=all_data$X))
  all_data$yhat_ipw_est_trans_1 <- predict(ipw_model_est_trans_1, data.frame(X=all_data$X))
  all_data$yhat_ipw_est_trans_25 <- predict(ipw_model_est_trans_25, data.frame(X=all_data$X))

  # IPW with true weights
  ipw_model_true <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_true)
  ipw_model_true_clip_05 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_true_clip_05)
  ipw_model_true_clip_1 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_true_clip_1)
  ipw_model_true_clip_25 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_true_clip_25)
  ipw_model_true_trans_05 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_true_trans_05)
  ipw_model_true_trans_1 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_true_trans_1)
  ipw_model_true_trans_25 <- gam(Y ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_true_trans_25)
  all_data$yhat_ipw_true <- predict(ipw_model_true, data.frame(X=all_data$X))
  all_data$yhat_ipw_true_clip_05 <- predict(ipw_model_true_clip_05, data.frame(X=all_data$X))
  all_data$yhat_ipw_true_clip_1 <- predict(ipw_model_true_clip_1, data.frame(X=all_data$X))
  all_data$yhat_ipw_true_clip_25 <- predict(ipw_model_true_clip_25, data.frame(X=all_data$X))
  all_data$yhat_ipw_true_trans_05 <- predict(ipw_model_true_trans_05, data.frame(X=all_data$X))
  all_data$yhat_ipw_true_trans_1 <- predict(ipw_model_true_trans_1, data.frame(X=all_data$X))
  all_data$yhat_ipw_true_trans_25 <- predict(ipw_model_true_trans_25, data.frame(X=all_data$X))

  return(all_data)
}

cbind_doubly_robust <- function(all_data) {
  # Doubly Robust
  all_data$resid_naive <- all_data$Y - all_data$yhat_naive
  selected_data <- all_data[all_data$S, ]

  resid_ipw_model_est <- gam(resid_naive ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_est)
  all_data$residhat_ipw_est <- predict(resid_ipw_model_est, data.frame(X=all_data$X))
  all_data$yhat_dr_est <- all_data$yhat_naive + all_data$residhat_ipw_est

  resid_ipw_model_true <- gam(resid_naive ~ s(X, bs = "tp"), data = selected_data, weights = selected_data$weights_true)
  all_data$residhat_ipw_true <- predict(resid_ipw_model_true, data.frame(X=all_data$X))
  all_data$yhat_dr_true <- all_data$yhat_naive + all_data$residhat_ipw_true

  return(all_data)
}

cbind_predictions <- function(all_data, amat) {
  all_data <- cbind_true(all_data)
  all_data <- cbind_naive(all_data)
  all_data <- cbind_recursive(all_data, amat)
  all_data <- cbind_ipw(all_data, amat)
  all_data <- cbind_doubly_robust(all_data)
  
  return(all_data)
}

get_mse_result <- function(all_data) {
  vars <- colnames(all_data)
  estimators <- vars[sapply(vars, function(name) startsWith(name, "yhat"))]

  selected_data <- all_data[all_data$S, ]

  mse_all_estimators <- function(y, df, ...) {
    sapply(estimators, function(estimator) mse(y, df[, estimator], ...))
  }

  data.frame(
    "y_selected" = mse_all_estimators(selected_data$Y, selected_data),
    "y" = mse_all_estimators(all_data$Y, all_data),
    "yhat_true" = mse_all_estimators(all_data$yhat_true, all_data),
    "yhat_imputed" = mse_all_estimators(all_data$y_imputed, all_data),
    "y_mix" = mse_all_estimators(all_data$y_mix, all_data),
    "y_weighted_true" = mse_all_estimators(selected_data$Y, selected_data,
      weights = selected_data$weights_true
    ),
    "y_weighted_est" = mse_all_estimators(selected_data$Y, selected_data,
      weights = selected_data$weights_est
    )
  )
}

plot_results <- function(all_data) {
  selected_data <- all_data[all_data$S, ]
  rejected_data <- all_data[!all_data$S, ]

  ord <- order(all_data$X)
  par(mar = c(1, 1, 1, 1))
  plot(range(all_data$X), range(all_data$Y),
    type = "n",
    xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE
  )
  points(rejected_data$X, rejected_data$Y, pch = 16, cex = .75, col = "grey")
  points(selected_data$X, selected_data$Y, pch = 16, cex = .75)
  points(rejected_data$X, rejected_data$y_imputed,
    pch = 3, cex = .75, col = "#D55E00"
  )
  lines(all_data$X[ord], all_data$yhat_true[ord],
    pch = 16, col = "#009E73", lwd = 2.5
  )
  lines(all_data$X[ord], all_data$yhat_naive[ord],
    pch = 16, col = "#000000", lwd = 2.5
  )
  lines(all_data$X[ord], all_data$yhat_recursive[ord],
    pch = 16, col = "#D55E00", lwd = 2.5
  )
  lines(all_data$X[ord], all_data$yhat_ipw_true[ord],
    pch = 16, col = "#0072B2", lwd = 2.5
  )
   lines(all_data$X[ord], all_data$yhat_ipw_true_trans_25[ord],
    pch = 16, col = "#0072B2", lwd = 2.5, lty=2
  )
  lines(all_data$X[ord], all_data$yhat_ipw_est[ord],
    pch = 16, col = "#56B4E9", lwd = 2.5
  )
   lines(all_data$X[ord], all_data$yhat_ipw_est_trans_25[ord],
    pch = 16, col = "#56B4E9", lwd = 2.5, lty=2
  )
  lines(all_data$X[ord], all_data$yhat_dr_true[ord],
    pch = 16, col = "#F0E442", lwd = 2.5
  )
  lines(all_data$X[ord], all_data$yhat_dr_est[ord],
    pch = 16, col = "#E69F00", lwd = 2.5
  )
}

experiment <- function(graph_nr, iter, n = 900, plot_flag = FALSE) {
  seed <- 100000 * graph_nr + iter

  amat <- get_graph(graph_nr)

  all_data <- simulate_nonlinear(amat, n, seed)

  all_data <- cbind_predictions(all_data, amat)

  if (plot_flag) {
    plot_results(all_data)
  }

  mse_result <- get_mse_result(all_data)

  return(mse_result)
}

# print(experiment(7, 1, n = 1000))