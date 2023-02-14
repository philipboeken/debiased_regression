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
  if (length(weights) == 0) {
    return(NA)
  }
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

# Returns a list with the indices of:
# DAGs with S a sink node
# DAGs with S not a sink node
# ADMGs with S a sink node
# ADMGs with S not a sink node
get_graph_ranges <- function(graph_nr) {
  load("data/exp1/valid_graphs.RData")
  dags_idx <- sapply(1:nrow(valid_graphs), function(i) {
    amat <- matrix(as.numeric(valid_graphs[i, ]), nrow = 4)
    colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
    biarrs <- amat * t(amat)
    biarrs[lower.tri(biarrs, diag = TRUE)] <- 0
    return(sum(biarrs) == 0)
  })

  s_sink_idx <- sapply(1:nrow(valid_graphs), function(i) {
    amat <- matrix(as.numeric(valid_graphs[i, ]), nrow = 4)
    colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
    return(sum(amat[, "S"]) == 0)
  })

  x_to_s_idx <- sapply(1:nrow(valid_graphs), function(i) {
    amat <- matrix(as.numeric(valid_graphs[i, ]), nrow = 4)
    colnames(amat) <- rownames(amat) <- c("X", "Y", "Z", "S")
    return(amat["S", "X"] == 1 && amat["X", "S"] == 0)
  })

  return(list(
    "graphs_all" = (1:length(dags_idx)),
    "dags_s_sink" = (1:length(dags_idx))[dags_idx & s_sink_idx],
    "dags_s_not_sink" = (1:length(dags_idx))[dags_idx & !s_sink_idx],
    "admgs_s_sink" = (1:length(dags_idx))[!dags_idx & s_sink_idx],
    "admgs_s_not_sink" = (1:length(dags_idx))[!dags_idx & !s_sink_idx],
    "x_to_s" = (1:length(dags_idx))[x_to_s_idx]
  ))
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

get_confounder_name <- function(var1, var2) {
  sprintf("C_%s", paste(sort(c(var1, var2)), collapse = ""))
}

admg_to_dag <- function(amat) {
  biarrs <- amat * t(amat)
  biarrs[lower.tri(biarrs, diag = TRUE)] <- 0
  for (var1 in colnames(biarrs)) {
    connected <- rownames(biarrs)[biarrs[, var1]]
    for (var2 in connected) {
      amat[var1, var2] <- amat[var2, var1] <- 0
      conf_name <- get_confounder_name(var1, var2)
      if (!conf_name %in% colnames(amat)) {
        amat <- cbind(amat, numeric(nrow(amat)))
        amat <- rbind(amat, numeric(ncol(amat)))
        colnames(amat)[ncol(amat)] <- rownames(amat)[nrow(amat)] <- conf_name
      }
      amat[var1, conf_name] <- 1
      amat[var2, conf_name] <- 1
    }
  }
  amat
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

lgbm <- function(formula, data, weights = NULL) {
  if (is.null(weights)) {
    weights <- replicate(nrow(data), 1)
  }
  response <- all.vars(formula[[2]])
  covariates <- all.vars(formula[[3]])
  model <- suppressWarnings(suppressMessages(lightgbm::lightgbm(
    data = as.matrix(data[, covariates]),
    label = as.matrix(data[, response]),
    params = list(objective = "regression", metric = "l2"),
    weight = weights,
    verbose = -1
  )))
  unlockBinding("predict", model)
  pred <- model$predict
  model$predict <- function(data, ...) {
    pred(as.matrix(data), ...)
  }
  lockBinding("predict", model)
  model
}

gam_wrapper <- function(formula, data, weights = NULL) {
  if (is.null(weights)) {
    weights <- replicate(nrow(data), 1)
  }
  response <- all.vars(formula[[2]])
  covariates <- all.vars(formula[[3]])
  formula_2 <- as.formula(sprintf(
    "%s ~ s(%s, bs=\"tp\")", response,
    paste(covariates, collapse = ", ")
  ))
  gam(formula = formula_2, data = data, weights = weights)
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

get_imputation_model <- function(selected_data, graph_known = FALSE,
                                 amat = NULL, impute_linear = FALSE) {
  stopifnot(!(graph_known && is.null(amat)))
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

  imputation_model
}

get_rr_model <- function(data, imputation_model) {
  data$yhat_imputed <- predict(imputation_model, data)
  gam(yhat_imputed ~ s(X, bs = "tp"), data = data)
}

cbind_imputations <- function(data, imputation_model) {
  data$yhat_imputed <- predict(imputation_model, data.frame(X = data$X, Z = data$Z))
  data$y_mix <- data$yhat_imputed
  data$y_mix[data$S] <- data$Y[data$S]

  data
}

cbind_repeated <- function(test_data, train_data = NULL, imp_model_data = NULL,
                           graph_known = FALSE, amat = NULL, impute_linear = FALSE) {
  stopifnot(!(graph_known && is.null(amat)))

  if (is.null(train_data)) train_data <- test_data
  if (is.null(imp_model_data)) imp_model_data <- train_data

  # Direct repeated (imputed) with gam
  selected_data <- imp_model_data[imp_model_data$S, ]
  imputation_model <- get_imputation_model(selected_data, graph_known, amat, impute_linear)

  train_data <- cbind_imputations(train_data, imputation_model)
  test_data <- cbind_imputations(test_data, imputation_model)

  mu_rr <- gam(yhat_imputed ~ s(X, bs = "tp"), data = train_data)
  test_data$yhat_repeated <- predict(mu_rr, data.frame(X = test_data$X))

  mu_rr_mix <- gam(y_mix ~ s(X, bs = "tp"), data = train_data)
  test_data$yhat_repeated_mix <- predict(mu_rr_mix, data.frame(X = test_data$X))

  return(test_data)
}

get_pi_model <- function(data, graph_known = FALSE, amat = NULL) {
  stopifnot(!(graph_known && is.null(amat)))
  if (graph_known && !"X" %in% get_parents("S", amat)) {
    pi_model <- gam(S ~ s(Z, bs = "tp"), family = binomial(link = "logit"), data = data)
  } else {
    pi_model <- gam(S ~ s(X, Z, bs = "tp"), family = binomial(link = "logit"), data = data)
  }

  pi_model
}

cbind_weights <- function(data, pi_model) {
  stopifnot(all(c("pi", "pi_hat") %in% colnames(data)))

  p_s <- mean(pi_model$model$S)
  data$weights_est <- p_s / data$pi_hat
  data$weights_est_clip_05 <- p_s / clip_lower_quantile(data$pi_hat, 0.05)
  data$weights_est_clip_1 <- p_s / clip_lower_quantile(data$pi_hat, 0.1)
  data$weights_est_clip_25 <- p_s / clip_lower_quantile(data$pi_hat, 0.25)
  data$weights_est_trans_05 <- p_s / trans_linear(data$pi_hat, 0.05, 1)
  data$weights_est_trans_1 <- p_s / trans_linear(data$pi_hat, 0.1, 1)
  data$weights_est_trans_25 <- p_s / trans_linear(data$pi_hat, 0.25, 1)
  data$weights_true <- p_s / data$pi
  data$weights_true_clip_05 <- p_s / clip_lower_quantile(data$pi, 0.05)
  data$weights_true_clip_1 <- p_s / clip_lower_quantile(data$pi, 0.1)
  data$weights_true_clip_25 <- p_s / clip_lower_quantile(data$pi, 0.25)
  data$weights_true_trans_05 <- p_s / trans_linear(data$pi, 0.05, 1)
  data$weights_true_trans_1 <- p_s / trans_linear(data$pi, 0.1, 1)
  data$weights_true_trans_25 <- p_s / trans_linear(data$pi, 0.25, 1)

  data$weights_true_clipped <- data$weights_true_trans_05
  data$yhat_iw_true_clipped <- data$yhat_iw_true_trans_05
  data$weights_est_clipped <- data$weights_est_trans_05
  data$yhat_iw_est_clipped <- data$yhat_iw_est_trans_05

  data
}

cbind_iw <- function(test_data, train_data = NULL, pi_model_data = NULL, model = gam_wrapper,
                     graph_known = FALSE, amat = NULL) {
  stopifnot(!(graph_known && is.null(amat)))

  if (is.null(train_data)) train_data <- test_data
  if (is.null(pi_model_data)) pi_model_data <- train_data

  # Estimate pi and calculate weights
  pi_model <- get_pi_model(pi_model_data, graph_known, amat)

  train_data$pi_hat <- predict(pi_model, train_data[, c("X", "Z")], type = "response")
  test_data$pi_hat <- predict(pi_model, test_data[, c("X", "Z")], type = "response")
  train_data <- cbind_weights(train_data, pi_model)
  test_data <- cbind_weights(test_data, pi_model)

  # IW with estimated weights
  selected_data <- train_data[train_data$S, ]
  types <- colnames(test_data)[sapply(colnames(test_data), function(s) startsWith(s, "weights_"))]
  for (type in types) {
    name <- sprintf("yhat_iw_%s", substr(type, nchar("weights_") + 1, nchar(type)))
    mu_iw <- model(Y ~ X, data = selected_data, weights = selected_data[, type])
    test_data[, name] <- predict(mu_iw, test_data)
  }

  return(test_data)
}

cbind_doubly_robust <- function(test_data, train_data = NULL, direct_method = "yhat_repeated") {
  if (is.null(train_data)) train_data <- test_data

  weight_types <- c("weights_est", "weights_est_clipped", "weights_true", "weights_true_clipped")
  stopifnot(all(c(direct_method, weight_types) %in% colnames(train_data)))

  test_data$yhat_dr_direct <- test_data[, direct_method]
  train_data$yhat_dr_direct <- train_data[, direct_method]
  train_data$dr_resid <- train_data$Y - train_data$yhat_dr_direct
  test_data$dr_resid <- test_data$Y - test_data$yhat_dr_direct
  selected_data <- train_data[train_data$S, ]

  for (weight_type in weight_types) {
    r_iw <- gam(dr_resid ~ s(X, bs = "tp"),
      data = selected_data,
      weights = selected_data[, weight_type]
    )
    weight_name <- substr(weight_type, nchar("weights_") + 1, nchar(weight_type))
    residhat_name <- sprintf("residhat_iw_%s", weight_name)
    yhat_dr_name <- sprintf("yhat_dr_%s", weight_name)
    test_data[, residhat_name] <- predict(r_iw, data.frame(X = test_data$X))
    test_data[, yhat_dr_name] <- test_data$yhat_dr_direct + test_data[, residhat_name]
  }

  return(test_data)
}

cbind_predictions <- function(all_data, graph_known = FALSE, amat = NULL) {
  stopifnot(!(graph_known && is.null(amat)))

  all_data <- cbind_true(all_data)
  all_data <- cbind_naive(all_data)
  all_data <- cbind_repeated(all_data, graph_known = graph_known, amat = amat)
  all_data <- cbind_iw(all_data, graph_known = graph_known, amat = amat)
  all_data <- cbind_doubly_robust(all_data)

  return(all_data)
}

get_mse_result <- function(data) {
  vars <- colnames(data)
  estimators <- vars[sapply(vars, function(name) startsWith(name, "yhat"))]

  mse_all_estimators <- function(y, df, ...) {
    sapply(estimators, function(estimator) mse(y, df[, estimator], ...))
  }

  results <- data.frame(y = mse_all_estimators(data$Y, data))
  interp_idx <- min(data$X[data$S]) <= data$X & data$X <= max(data$X[data$S])
  dfs <- list(data, data[interp_idx, ], data[!interp_idx, ])
  types <- c("", "_interp", "_extrap")
  for (i in 1:length(dfs)) {
    df <- dfs[[i]]
    results[, paste0("y", types[i])] <- mse_all_estimators(df$Y, df)
    results[, paste0("yhat_true", types[i])] <- mse_all_estimators(df$yhat_true, df)
    results[, paste0("yhat_imputed", types[i])] <- mse_all_estimators(df$yhat_imputed, df)
    results[, paste0("y_mix", types[i])] <- mse_all_estimators(df$y_mix, df)
  }

  selected_data <- data[data$S, ]
  if (nrow(selected_data) > 0) {
    results <- cbind(results, data.frame(
      "y_selected" = mse_all_estimators(selected_data$Y, selected_data),
      "y_weighted_true" = mse_all_estimators(selected_data$Y, selected_data,
        weights = selected_data$weights_true
      ),
      "y_weighted_true_clipped" = mse_all_estimators(selected_data$Y, selected_data,
        weights = selected_data$weights_true_clipped
      ),
      "y_weighted_est" = mse_all_estimators(selected_data$Y, selected_data,
        weights = selected_data$weights_est
      ),
      "y_weighted_est_clipped" = mse_all_estimators(selected_data$Y, selected_data,
        weights = selected_data$weights_est_clipped
      )
    ))
  }
  results
}

get_mse_stats <- function(mse_results) {
  means <- as.matrix(sapply(mse_results, mean))
  sds <- as.matrix(sapply(mse_results, sd))
  dim(means) <- dim(sds) <- dim(mse_results)
  dimnames(means) <- dimnames(sds) <- dimnames(mse_results)
  list(means = data.frame(means), sds = data.frame(sds))
}

get_mse_formatted <- function(mse_results, bold = NA) {
  mse_stats <- get_mse_stats(mse_results)
  idx_order <- order(mse_stats$means$y)
  formatted_results <- mse_stats$means[idx_order, ]
  formatted_results[, ] <- NA
  for (i in rownames(formatted_results)) {
    for (j in colnames(formatted_results)) {
      if (!all(is.na(bold)) && bold[i, j]) {
        formatted_results[i, j] <- sprintf(
          "\\textbf{%.3e} (%.0e)",
          mse_stats$means[i, j],
          mse_stats$sds[i, j]
        )
      } else {
        formatted_results[i, j] <- sprintf(
          "%.3e (%.0e)",
          mse_stats$means[i, j],
          mse_stats$sds[i, j]
        )
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
    out <- paste(out, substring(out_temp[((i - 1) * keep + 1):(i * keep)], length_skip),
      sep = ""
    )
  }
  cat(out, "\n", file = file, sep = "\n", append = append)
}

table_to_tex <- function(table, bold = NA) {
  if (!all(is.na(bold))) {
    table[bold] <- sprintf("\\textbf{%s}", table[bold])
  }
  apply(table, 1, function(row) paste(row, collapse = " & "))
}

palette <- c(
  "yhat_true" = "#009E73",
  "yhat_naive" = "#000000",
  "yhat_missp" = "#000000",
  "yhat_repeated" = "#D55E00",
  "yhat_iw_true" = "#0072B2",
  # "yhat_iw_true_clipped" = "#0072B2",
  "yhat_iw_est" = "#56B4E9",
  # "yhat_iw_est_clipped" = "#56B4E9",
  "yhat_dr_true" = "#E69F00",
  # "yhat_dr_true_clipped" = "#E69F00",
  "yhat_dr_est" = "#F0E442"
  # "yhat_dr_est_clipped" = "#F0E442"
)

legend_labels <- c(
  "yhat_true" = "True",
  "yhat_naive" = "Naive",
  "yhat_missp" = "Missp.",
  "yhat_repeated" = "RR",
  "yhat_iw_true" = "IW",
  "yhat_iw_true_clipped" = "IW-tc",
  "yhat_iw_est" = "IW-e",
  "yhat_iw_est_clipped" = "IW-ec",
  "yhat_dr_true" = "DR",
  "yhat_dr_true_clipped" = "DR-tc",
  "yhat_dr_est" = "DR-e",
  "yhat_dr_est_clipped" = "DR-ec"
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
  points(selected_data$X, selected_data$Y,
    pch = 16,
    cex = trans_linear(weights_obs, .75, max(weights_obs))
  )
  if ("yhat_imputed" %in% colnames(all_data)) {
    points(all_data$X, all_data$yhat_imputed, pch = 3, cex = .75, col = "#D55E00")
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
