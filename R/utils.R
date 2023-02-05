library(mgcv)

get_arg <- function(idx, default_value = NA) {
  args <- commandArgs(trailingOnly = TRUE)
  arg <- args[idx]
  arg <- if (is.na(arg)) default_value else arg
  arg
}

get_arg_numeric <- function(idx, default_value = NA) {
  as.numeric(get_arg(idx, default_value))
}

get_arg_logical <- function(idx, default_value = NA) {
  as.logical(get_arg(idx, default_value))
}

get_arg_character <- function(idx, default_value = NA) {
  as.character(get_arg(idx, default_value))
}

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

trans_linear <- function(probs, min_value, max_value = 1) {
  if (max(probs) == min(probs)) {
    return(probs)
  }
  probs <- probs - min(probs)
  (probs / max(probs)) * (max_value - min_value) + min_value
}

get_graph <- function(graph_nr) {
  load("data/exp1/valid_graphs.RData")
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

smaller_top_order <- function(v, w, amat) {
  top_order <- get_top_order(amat)
  which(top_order == v) < which(top_order == w)
}

# Taken from:
# https://www.r-bloggers.com/2019/07/sampling-paths-from-a-gaussian-process/

# SQUARED EXPONENTIAL KERNEL (RBF)
se_kernel <- function(x, y, sigma = 1 / 2, length = 1) {
  sigma^2 * exp(-sum((x - y)^2) / (2 * length^2))
}

# MATERN COVARIANCE
matern_kernel <- function(x, y, nu = 1.5, sigma = 1, l = 1) {
  if (!(nu %in% c(0.5, 1.5, 2.5))) {
    stop("p must be equal to 0.5, 1.5 or 2.5")
  }
  p <- nu - 0.5
  d <- sqrt(sum(abs(x - y)^2))
  if (p == 0) {
    sigma^2 * exp(-d / l)
  } else if (p == 1) {
    sigma^2 * (1 + sqrt(3) * d / l) * exp(-sqrt(3) * d / l)
  } else {
    sigma^2 * (1 + sqrt(5) * d / l + 5 * d^2 / (3 * l^2)) * exp(-sqrt(5) * d / l)
  }
}

kernel_matrix <- function(x, kernel_fn, ...) {
  x <- as.matrix(x)
  n <- nrow(x)
  cov_matrix <- matrix(rep(0, n^2), nrow = n)
  for (i1 in 1:n) {
    for (i2 in i1:n) {
      cov_matrix[i1, i2] <- kernel_fn(x[i1, ], x[i2, ], ...)
    }
  }
  cov_matrix <- cov_matrix + t(cov_matrix)
  diag(cov_matrix) <- diag(cov_matrix) / 2

  return(cov_matrix)
}

# given x coordinates, take draw from kernel function at those points
draw_gp <- function(x, kernel_fn, ...) {
  x <- as.matrix(x)
  cov_matrix <- kernel_matrix(x, kernel_fn, ...)
  MASS::mvrnorm(1, mu = rep(0, times = nrow(x)), Sigma = cov_matrix)
}

cbind_true <- function(all_data) {
  # 'True' model as if we have observed all data
  true_model <- gam(Y ~ s(X, bs = "tp"), data = all_data)
  all_data$yhat_true <- predict(true_model, data.frame(X = all_data$X))

  return(all_data)
}

cbind_naive <- function(all_data) {
  selected_data <- all_data[all_data$S, ]

  # Naive model directly trained on observed data
  naive_model <- gam(Y ~ s(X, bs = "tp"), data = selected_data)
  all_data$yhat_naive <- predict(naive_model, data.frame(X = all_data$X))

  return(all_data)
}

cbind_recursive <- function(all_data, graph_known = FALSE, amat = NULL, impute_linear = FALSE) {
  stopifnot(!(graph_known && is.null(amat)))
  selected_data <- all_data[all_data$S, ]

  # Direct recursive (imputed) with gam
  if (graph_known && !"Y" %in% get_parents("X", amat) && !"X" %in% get_parents("Y", amat)) {
    if (impute_linear) {
      imputation_model <- lm(Y ~ Z, data = selected_data)
    } else {
      imputation_model <- gam(Y ~ s(Z, bs = "tp"), data = selected_data)
    }
  } else {
    if (impute_linear) {
      imputation_model <- lm(Y ~ Z + X, data = selected_data)
    } else {
      imputation_model <- gam(Y ~ s(X, Z, bs = "tp"), data = selected_data)
    }
  }
  all_data$y_imputed <- predict(imputation_model, data.frame(X = all_data$X, Z = all_data$Z))
  all_data$y_mix <- all_data$y_imputed
  all_data$y_mix[all_data$S] <- selected_data$Y

  recursive_model <- gam(y_imputed ~ s(X, bs = "tp"), data = all_data)
  all_data$yhat_recursive <- predict(recursive_model, data.frame(X = all_data$X))

  recursive_model_mix <- gam(y_mix ~ s(X, bs = "tp"), data = all_data)
  all_data$yhat_recursive_mix <- predict(recursive_model_mix, data.frame(X = all_data$X))

  return(all_data)
}

cbind_ipw <- function(all_data, graph_known = FALSE, amat = NULL) {
  stopifnot(!(graph_known && is.null(amat)))
  selected_data <- all_data[all_data$S, ]

  # Estimate pi and calculate weights
  if (graph_known && !"X" %in% get_parents("S", amat)) {
    pi_model <- gam(S ~ s(Z, bs = "tp"), family = binomial(link = "logit"), data = all_data)
  } else {
    pi_model <- gam(S ~ s(X, Z, bs = "tp"), family = binomial(link = "logit"), data = all_data)
  }
  all_data$pi_hat <- pi_model$fitted.values
  p_s <- sum(all_data$S) / length(all_data$S)
  all_data$weights_est <- p_s / all_data$pi_hat
  all_data$weights_est_clip_05 <- p_s / clip_lower_quantile(all_data$pi_hat, 0.05)
  all_data$weights_est_clip_1 <- p_s / clip_lower_quantile(all_data$pi_hat, 0.1)
  all_data$weights_est_clip_25 <- p_s / clip_lower_quantile(all_data$pi_hat, 0.25)
  all_data$weights_est_trans_05 <- p_s / trans_linear(all_data$pi_hat, 0.05, 1)
  all_data$weights_est_trans_1 <- p_s / trans_linear(all_data$pi_hat, 0.1, 1)
  all_data$weights_est_trans_25 <- p_s / trans_linear(all_data$pi_hat, 0.25, 1)
  all_data$weights_true <- p_s / all_data$pi
  all_data$weights_true_clip_05 <- p_s / clip_lower_quantile(all_data$pi, 0.05)
  all_data$weights_true_clip_1 <- p_s / clip_lower_quantile(all_data$pi, 0.1)
  all_data$weights_true_clip_25 <- p_s / clip_lower_quantile(all_data$pi, 0.25)
  all_data$weights_true_trans_05 <- p_s / trans_linear(all_data$pi, 0.05, 1)
  all_data$weights_true_trans_1 <- p_s / trans_linear(all_data$pi, 0.1, 1)
  all_data$weights_true_trans_25 <- p_s / trans_linear(all_data$pi, 0.25, 1)
  selected_data <- all_data[all_data$S, ]

  # IPW with estimated weights
  ipw_model_est <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_est
  )
  ipw_model_est_clip_05 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_est_clip_05
  )
  ipw_model_est_clip_1 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_est_clip_1
  )
  ipw_model_est_clip_25 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_est_clip_25
  )
  ipw_model_est_trans_05 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_est_trans_05
  )
  ipw_model_est_trans_1 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_est_trans_1
  )
  ipw_model_est_trans_25 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_est_trans_25
  )
  all_data$yhat_ipw_est <- predict(ipw_model_est, data.frame(X = all_data$X))
  all_data$yhat_ipw_est_clip_05 <- predict(ipw_model_est_clip_05, data.frame(X = all_data$X))
  all_data$yhat_ipw_est_clip_1 <- predict(ipw_model_est_clip_1, data.frame(X = all_data$X))
  all_data$yhat_ipw_est_clip_25 <- predict(ipw_model_est_clip_25, data.frame(X = all_data$X))
  all_data$yhat_ipw_est_trans_05 <- predict(ipw_model_est_trans_05, data.frame(X = all_data$X))
  all_data$yhat_ipw_est_trans_1 <- predict(ipw_model_est_trans_1, data.frame(X = all_data$X))
  all_data$yhat_ipw_est_trans_25 <- predict(ipw_model_est_trans_25, data.frame(X = all_data$X))

  # IPW with true weights
  ipw_model_true <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_true
  )
  ipw_model_true_clip_05 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_true_clip_05
  )
  ipw_model_true_clip_1 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_true_clip_1
  )
  ipw_model_true_clip_25 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_true_clip_25
  )
  ipw_model_true_trans_05 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_true_trans_05
  )
  ipw_model_true_trans_1 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_true_trans_1
  )
  ipw_model_true_trans_25 <- gam(Y ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_true_trans_25
  )
  all_data$yhat_ipw_true <- predict(ipw_model_true, data.frame(X = all_data$X))
  all_data$yhat_ipw_true_clip_05 <- predict(ipw_model_true_clip_05, data.frame(X = all_data$X))
  all_data$yhat_ipw_true_clip_1 <- predict(ipw_model_true_clip_1, data.frame(X = all_data$X))
  all_data$yhat_ipw_true_clip_25 <- predict(ipw_model_true_clip_25, data.frame(X = all_data$X))
  all_data$yhat_ipw_true_trans_05 <- predict(ipw_model_true_trans_05, data.frame(X = all_data$X))
  all_data$yhat_ipw_true_trans_1 <- predict(ipw_model_true_trans_1, data.frame(X = all_data$X))
  all_data$yhat_ipw_true_trans_25 <- predict(ipw_model_true_trans_25, data.frame(X = all_data$X))

  all_data$weights_true_clipped <- all_data$weights_true_trans_05
  all_data$yhat_ipw_true_clipped <- all_data$yhat_ipw_true_trans_05
  all_data$weights_est_clipped <- all_data$weights_est_trans_05
  all_data$yhat_ipw_est_clipped <- all_data$yhat_ipw_est_trans_05

  return(all_data)
}

cbind_doubly_robust <- function(all_data, direct_method = "yhat_recursive_mix") {
  stopifnot(all(c(direct_method, c(
    "weights_est", "weights_est_clipped", "weights_true", "weights_true_clipped"
  )) %in% colnames(all_data)))

  all_data$yhat_dr_direct <- all_data[, direct_method]
  all_data$dr_resid <- all_data$Y - all_data$yhat_dr_direct
  selected_data <- all_data[all_data$S, ]

  resid_ipw_model_est <- gam(dr_resid ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_est
  )
  all_data$residhat_ipw_est <- predict(resid_ipw_model_est, data.frame(X = all_data$X))
  all_data$yhat_dr_est <- all_data$yhat_dr_direct + all_data$residhat_ipw_est

  resid_ipw_model_est_clipped <- gam(dr_resid ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_est_clipped
  )
  all_data$residhat_ipw_est_clipped <- predict(resid_ipw_model_est_clipped, data.frame(X = all_data$X))
  all_data$yhat_dr_est_clipped <- all_data$yhat_dr_direct + all_data$residhat_ipw_est_clipped

  resid_ipw_model_true <- gam(dr_resid ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_true
  )
  all_data$residhat_ipw_true <- predict(resid_ipw_model_true, data.frame(X = all_data$X))
  all_data$yhat_dr_true <- all_data$yhat_dr_direct + all_data$residhat_ipw_true

  resid_ipw_model_true_clipped <- gam(dr_resid ~ s(X, bs = "tp"),
    data = selected_data,
    weights = selected_data$weights_true_clipped
  )
  all_data$residhat_ipw_true_clipped <- predict(resid_ipw_model_true_clipped, data.frame(X = all_data$X))
  all_data$yhat_dr_true_clipped <- all_data$yhat_dr_direct + all_data$residhat_ipw_true_clipped

  return(all_data)
}

cbind_predictions <- function(all_data, graph_known = FALSE, amat = NULL) {
  stopifnot(!(graph_known && is.null(amat)))

  all_data <- cbind_true(all_data)
  all_data <- cbind_naive(all_data)
  all_data <- cbind_recursive(all_data, graph_known, amat)
  all_data <- cbind_ipw(all_data, graph_known, amat)
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

get_mse_stats <- function(list_of_mse_results) {
  means <- list_of_mse_results[[1]]
  means[, ] <- NA
  vars <- means
  for (i in rownames(means)) {
    for (j in colnames(means)) {
      mse_for_type <- sapply(list_of_mse_results, function(e) e[i, j])
      means[i, j] <- mean(mse_for_type)
      vars[i, j] <- var(mse_for_type)
    }
  }
  list(means = means, vars = vars)
}

get_mse_formatted <- function(list_of_mse_results, bold = NA) {
  mse_stats <- get_mse_stats(list_of_mse_results)
  idx_order <- order(mse_stats$means$y)
  formatted_results <- list_of_mse_results[[1]][idx_order, ]
  formatted_results[, ] <- NA
  for (i in rownames(formatted_results)) {
    for (j in colnames(formatted_results)) {
      if (!all(is.na(bold)) && bold[i, j]) {
        formatted_results[i, j] <- sprintf("\\textbf{%.3e} (%.0e)", mse_stats$means[i, j], mse_stats$vars[i, j])
      } else {
        formatted_results[i, j] <- sprintf("%.3e (%.0e)", mse_stats$means[i, j], mse_stats$vars[i, j])
      }
    }
  }
  formatted_results
}

write_table <- function(table, file, append = FALSE) {
  out_temp <- capture.output(table)
  keep <- 1 + nrow(table)
  out <- out_temp[1:keep]
  for (i in 2:(length(out_temp) / keep)) {
    length_skip <- max(nchar(rownames(table))) + 1
    out <- paste(out, substring(out_temp[((i - 1) * keep + 1):(i * keep)], length_skip), sep = "")
  }
  cat(out, "\n", file = file, sep = "\n", append = append)
}

table_to_tex <- function(table, bold = NA) {
  if (!is.na(bold)) {
    table[bold] <- sprintf("\\textbf{%s}", table[bold])
  }
  apply(table, 1, function(row) paste(row, collapse = " & "))
}

palette <- c(
  "yhat_true" = "#009E73",
  "yhat_naive" = "#000000",
  "yhat_missp" = "#000000",
  "yhat_recursive_mix" = "#D55E00",
  "yhat_ipw_true" = "#0072B2",
  # "yhat_ipw_true_clipped" = "#0072B2",
  "yhat_ipw_est" = "#56B4E9",
  # "yhat_ipw_est_clipped" = "#56B4E9",
  "yhat_dr_true" = "#E69F00",
  # "yhat_dr_true_clipped" = "#E69F00",
  "yhat_dr_est" = "#F0E442"
  # "yhat_dr_est_clipped" = "#F0E442"
)

legend_labels <- c(
  "yhat_true" = "True",
  "yhat_naive" = "Naive",
  "yhat_missp" = "Misspecified",
  "yhat_recursive_mix" = "Recursive",
  "yhat_ipw_true" = "IPW (true)",
  "yhat_ipw_true_clipped" = "IPW (true, clipped)",
  "yhat_ipw_est" = "IPW (est.)",
  "yhat_ipw_est_clipped" = "IPW (est., clipped)",
  "yhat_dr_true" = "Doubly Robust (true)",
  "yhat_dr_true_clipped" = "Doubly Robust (true, clipped)",
  "yhat_dr_est" = "Doubly Robust (est.)",
  "yhat_dr_est_clipped" = "Doubly Robust (est., clipped)"
)

plot_results <- function(all_data, xlim = range(all_data$X), ylim = range(all_data$Y),
                         weights_obs = .75, legend_flag = FALSE) {
  selected_data <- all_data[all_data$S, ]
  rejected_data <- all_data[!all_data$S, ]

  ord <- order(all_data$X)
  par(mar = c(0, 0, 0, 0))
  plot(rejected_data$X, rejected_data$Y,
    pch = 16, cex = .75, col = "grey",
    xlim = xlim, ylim = ylim,
    xaxt = "n", yaxt = "n",
    ann = FALSE, frame.plot = FALSE
  )
  points(selected_data$X, selected_data$Y, pch = 16, cex = trans_linear(weights_obs, .75, max(weights_obs)))
  if ("y_imputed" %in% colnames(rejected_data)) {
    points(rejected_data$X, rejected_data$y_imputed, pch = 3, cex = .75, col = "#D55E00")
  }

  for (method in names(palette)) {
    if (method %in% colnames(all_data)) {
      lty <- if (endsWith(method, "_clipped")) 2 else 1
      lines(all_data$X[ord], all_data[ord, method],
        pch = 16, col = palette[method], lwd = 2.5, lty = lty
      )
    }
  }

  if (legend_flag) {
    items <- intersect(names(palette), colnames(all_data))
    lty <- sapply(items, function(method) if (endsWith(method, "_clipped")) 2 else 1)
    legend("bottomright",
      legend = legend_labels[items], col = palette[items],
      lty = lty, lwd = 2.5, inset = 0.01, bg = "white", cex = 0.9,
      y.intersp = 0.9, x.intersp = 0.9
    )
  }
}
