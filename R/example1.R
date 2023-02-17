source("R/utils.R")
library(pbapply)

simulate_example1 <- function(n, f_Z = function(x) 3 * sin(x), f_Y = function(x, z) z + x / 2) {
  sd_X <- 3
  sd_Z <- 2
  sd_Y <- 2

  train_data <- data.frame(X = rnorm(n, 0, sd_X))
  train_data$Z <- f_Z(train_data$X) + rnorm(n, 0, sd_Z)
  train_data$eps_Y <- rnorm(n, 0, sd_Y)
  train_data$Y <- f_Y(train_data$X, train_data$Z) + train_data$eps_Y

  train_data$pi <- sigmoid(as.numeric(scale(train_data$X) * 4)) *
    trans_linear(sigmoid(as.numeric(scale(train_data$Z) * 4)), 1 / 20, 1)
  train_data$S <- runif(n) < train_data$pi
  train_data
}

print_mse_table <- function(data, cols = c("y", "y_selected", "yhat_imputed", "y_weighted_true", "y_weighted_est")) {
  mse_result <- get_mse_result(data)
  cols <- intersect(cols, colnames(mse_result))
  mse_result <- mse_result[
    c("yhat_naive", "yhat_repeated", "yhat_iw_true", "yhat_iw_est", "yhat_dr_true", "yhat_dr_est"),
    cols
  ]
  print(mse_result)
  maxes <- apply(mse_result, 2, function(col) col == min(col))
  mse_results <- table_to_tex(round(mse_result, 2), bold = maxes)
  labels <- c("Naive", "RR", "IW-t", "IW-e", "DR-t", "DR-e")
  cat(" & ", paste(cols, collapse = " & "), "\n")
  for (i in 1:length(mse_results)) {
    cat(labels[i], " & ", mse_results[[i]], "\\\\ \n")
  }
  cat("\n")
}

example1_single <- function(n = 400, seed = 1, save_figs = FALSE) {
  set.seed(seed)

  ################################################
  # Simulate data
  f_Z <- function(x) 3 * sin(x)
  f_Y <- function(x, z) z + x / 2
  ftrue <- function(x) f_Y(x, f_Z(x))
  train_data <- simulate_example1(n, f_Z, f_Y)
  selected_data <- train_data[train_data$S, ]

  cat("Train #{S=1}: ", nrow(selected_data), "\n\n")

  ################################################
  # Plot true and naive fit
  train_data$yhat_true <- ftrue(train_data$X)
  train_data <- cbind_naive(train_data)
  if (save_figs) pdf("output/figures/example1/1_true_and_naive.pdf", width = 5, height = 5 * 2 / 3)
  plot_results(train_data, legend_flag = TRUE)
  title(xlab = "X", ylab = "Y", line = -1, cex.lab = 1.2, las = 3)
  if (save_figs) dev.off()

  ################################################
  # Plot imputed values
  train_data <- cbind_repeated(train_data, impute_linear = FALSE)
  if (save_figs) pdf("output/figures/example1/2a_imputed.pdf", width = 5, height = 5 * 2 / 3)
  offsets_y <- c(25, 45, 62)
  loc_x <- min(train_data$X) - 3
  rejected_data <- train_data[!train_data$S, ]
  plot_results(train_data[, c("X", "Y", "S")],
    xlim = c(loc_x, max(train_data$X)),
    ylim = c(min(train_data$Y), max(train_data$Y) + max(offsets_y))
  )

  text(loc_x, mean(train_data$eps_Y) + offsets_y[3] - 1, latex2exp::TeX("$\\epsilon_Y$"),
    cex = 1.8, pos = 3, col = "black"
  )
  points(rejected_data$X, rejected_data$eps_Y + offsets_y[3],
    cex = .75, pch = 16, col = "gray"
  )
  points(selected_data$X, selected_data$eps_Y + offsets_y[3],
    cex = .75, pch = 16, col = "black"
  )
  text(mean(train_data$X), 48, latex2exp::TeX("$+$"),
    cex = 2, pos = 3, col = "black"
  )

  text(loc_x, mean(train_data$X / 2) + offsets_y[2] - 5, latex2exp::TeX("$\\frac{1}{2}X$"),
    cex = 1.5, pos = 3, col = "black"
  )
  points(train_data$X, train_data$X / 2 + offsets_y[2], cex = .75, pch = 16)
  text(mean(train_data$X), 34, latex2exp::TeX("$+$"),
    cex = 2, pos = 3, col = "black"
  )

  text(loc_x, mean(train_data$Z) + offsets_y[1] - 1, latex2exp::TeX("$Z$"),
    cex = 1.5, pos = 3, col = "black"
  )
  points(train_data$X, train_data$Z + offsets_y[1], cex = .75, pch = 16)
  text(mean(train_data$X), 10, latex2exp::TeX("$=$"),
    cex = 2, pos = 3, col = "black"
  )
  text(loc_x, mean(train_data$Y) - 1, latex2exp::TeX("$Y$"),
    cex = 1.5, pos = 3, col = "black"
  )
  if (save_figs) dev.off()

  print(lm(Y ~ X + Z, data = selected_data)$coefficients)

  ################################################
  # Plot repeated method
  if (save_figs) pdf("output/figures/example1/2b_imputed.pdf", width = 5, height = 5 * 2 / 3)
  plot_results(train_data, legend_flag = TRUE)
  title(xlab = "X", ylab = "Y", line = -1, cex.lab = 1.2, las = 3)
  if (save_figs) dev.off()

  ################################################
  # Plot IW estimator with estimated probabilities
  train_data <- cbind_iw(train_data)
  selected_data <- train_data[train_data$S, ]
  if (save_figs) pdf("output/figures/example1/3_iw_estimated_weights.pdf", width = 5, height = 5 * 2 / 3)
  plot_results(train_data[, c("X", "S", "Y", "yhat_true", "yhat_naive", "yhat_iw_est")],
    weights_obs = trans_linear(selected_data$weights_est, 0.75, max(selected_data$weights_est) * 3 / 5),
    legend_flag = TRUE
  )
  title(xlab = "X", ylab = "Y", line = -1, cex.lab = 1.2, las = 3)
  if (save_figs) dev.off()

  ################################################
  # Plot IW estimator with known probabilities
  if (save_figs) pdf("output/figures/example1/4_iw_true_weights.pdf", width = 5, height = 5 * 2 / 3)
  plot_results(train_data[, c("X", "S", "Y", "yhat_true", "yhat_naive", "yhat_iw_true")],
    weights_obs = trans_linear(selected_data$weights_true, 0.75, max(selected_data$weights_true) * 3 / 5),
    legend_flag = TRUE
  )
  title(xlab = "X", ylab = "Y", line = -1, cex.lab = 1.2, las = 3)
  if (save_figs) dev.off()

  ################################################
  # Plot naive, the IW estimated residuals, and the doubly robust estimator
  train_data$yhat_missp <- lm(yhat_imputed ~ poly(X, degree = 5), data = train_data)$fitted.values
  train_data <- cbind_doubly_robust(train_data, direct_method = "yhat_missp")
  selected_data <- train_data[train_data$S, ]
  offset <- max(selected_data$dr_resid) - min(train_data$Y) + 5
  ord <- order(train_data$X)
  if (save_figs) pdf("output/figures/example1/5_dr_true_weights.pdf", width = 5, height = 5 * 2 / 3)
  plot_results(train_data[, c("X", "S", "Y", "yhat_true", "yhat_missp", "yhat_dr_true")],
    ylim = c(min(train_data$Y) - offset, max(train_data$Y)), legend_flag = FALSE
  )
  points(selected_data$X, selected_data$dr_resid - offset,
    pch = 16, col = "black",
    cex = trans_linear(selected_data$weights_true, 0.75, max(selected_data$weights_true) * 3 / 5)
  )
  lines(train_data$X[ord], train_data$residhat_iw_true[ord] - offset, col = "#F0E442", lwd = 2.5)
  items <- c("yhat_true", "yhat_missp", "yhat_dr_true")
  legend_labels_temp <- legend_labels
  legend_labels_temp["yhat_dr_true"] <- "DR"
  legend("bottomright",
    legend = append(legend_labels_temp[items], "Resid.", 2), col = append(palette[items], "#F0E442", 2),
    lty = 1, lwd = 2.5, inset = 0.01, bg = "white",
    y.intersp = 0.9, x.intersp = 0.9
  )
  title(xlab = "X", ylab = "Y", line = -1, cex.lab = 1.2, las = 3)
  if (save_figs) dev.off()

  ################################################
  # Plot overview of naive, imputed, IW and DR estimates on train set
  train_data <- cbind_doubly_robust(train_data, direct_method = "yhat_repeated")
  if (save_figs) pdf("output/figures/example1/6a_overview_train.pdf", width = 5, height = 5 * 2 / 3)
  plot_results(train_data[, c(
    "X", "S", "Y", "yhat_true", "yhat_naive",
    "yhat_repeated", "yhat_iw_true", "yhat_iw_est", "yhat_dr_true"
  )], legend_flag = TRUE)
  title(xlab = "X", ylab = "Y", line = -1, cex.lab = 1.2, las = 3)
  if (save_figs) dev.off()


  ################################################
  # Evaluation on train set
  cat("Evaluation on train set: \n")
  selected_data <- train_data[train_data$S, ]
  print_mse_table(train_data)

  ################################################
  # Generate test set and add estimations
  test_data <- simulate_example1(n, f_Z, f_Y)
  cat("Test #{S=1}: ", sum(test_data$S), "\n\n")
  test_data$yhat_true <- ftrue(test_data$X)
  test_data <- cbind_naive(test_data, train_data)

  all_data <- rbind(
    train_data[, c("Y", "X", "Z", "S", "pi")],
    test_data[, c("Y", "X", "Z", "S", "pi")]
  )

  test_data <- cbind_repeated(test_data, train_data, imputation_model_data = train_data)
  test_data <- cbind_iw(test_data, train_data, pi_model_data = train_data)
  test_data <- cbind_doubly_robust(test_data, train_data)

  ################################################
  # Plot overview of naive, imputed, IW and DR estimates on test set
  if (save_figs) pdf("output/figures/example1/6b_overview_test.pdf", width = 5, height = 5 * 2 / 3)
  plot_results(test_data[, c(
    "X", "S", "Y", "yhat_true", "yhat_naive",
    "yhat_repeated", "yhat_iw_true", "yhat_iw_est", "yhat_dr_true"
  )], legend_flag = TRUE)
  title(xlab = "X", ylab = "Y", line = -1, cex.lab = 1.2, las = 3)
  if (save_figs) dev.off()

  ################################################
  # Evaluation on entire test set
  cat("Evaluation on test set: \n")
  print_mse_table(test_data)

  cat("\nInterpolation extrapolation: \n")
  print_mse_table(test_data, cols = c("y", "y_interp", "y_extrap"))
}

example1_repeated <- function(n = 400, m = 500, seed = 1) {
  set.seed(seed)

  pblapply(1:m, function(i) {
    f_Z <- function(x) 3 * sin(x)
    f_Y <- function(x, z) z + x / 2
    ftrue <- function(x) f_Y(x, f_Z(x))
    train_data <- simulate_example1(n, f_Z, f_Y)
    selected_data <- train_data[train_data$S, ]

    train_data$yhat_true <- ftrue(train_data$X)
    train_data <- cbind_naive(train_data)
    train_data <- cbind_repeated(train_data, impute_linear = FALSE)
    train_data <- cbind_iw(train_data)
    train_data$yhat_missp <- lm(yhat_imputed ~ poly(X, degree = 5), data = train_data)$fitted.values
    train_data <- cbind_doubly_robust(train_data, direct_method = "yhat_missp")
    train_data <- cbind_doubly_robust(train_data, direct_method = "yhat_repeated")

    ################################################
    # Generate test set and add estimations
    test_data <- simulate_example1(n, f_Z, f_Y)
    test_data$yhat_true <- ftrue(test_data$X)
    test_data <- cbind_naive(test_data, train_data)

    all_data <- rbind(
      train_data[, c("Y", "X", "Z", "S", "pi")],
      test_data[, c("Y", "X", "Z", "S", "pi")]
    )

    test_data <- cbind_repeated(test_data, train_data, imputation_model_data = train_data)
    test_data <- cbind_iw(test_data, train_data, pi_model_data = train_data)
    test_data <- cbind_doubly_robust(test_data, train_data)

    mse_result <- get_mse_result(test_data)

    return(mse_result)
  })
}

n <- get_arg_numeric(1, 400)
seed <- get_arg_numeric(2, 7)
save_figs <- get_arg_logical(3, TRUE)
m <- get_arg_numeric(4, 500)

start <- Sys.time()
cat("\nStarting example1.R", c(n, seed, save_figs), "at", format(start), "\n")

example1_single(n, seed, save_figs)

data_filename <- sprintf("output/tables/example1/results_%s.RData", m)
if (!file.exists(data_filename)) {
  cat("Running", m, "iterations: \n")
  all_mse_results <- example1_repeated(n, m = m, seed)
  all_mse_results <- transform_mse_results(all_mse_results)
  save(all_mse_results, file = data_filename)
} else {
  load(data_filename)
}

output_table(all_mse_results,
  rows = c(
    "yhat_true", "yhat_naive", "yhat_repeated", "yhat_iw_true_clipped",
    "yhat_iw_est_clipped", "yhat_dr_true_clipped", "yhat_dr_est_clipped"
  ),
  columns = c(
    "y", "y_selected", "yhat_imputed",
    "y_weighted_true", "y_weighted_est"
  ), print_sds = 0
)

cat("\nInterpolation vs extrapolation:\n")
output_table(all_mse_results,
  columns = c("y", "y_interp", "y_extrap"),
  rows = c("yhat_repeated", "yhat_iw_true_clipped", "yhat_iw_est_clipped"),
  row_labels = c("RR", "IW-t", "IW-e"),
  print_sds = 0
)

end <- Sys.time()
cat("\nFinished example1.R", c(n, seed, save_figs), "at", format(end), "in", format(end - start), "\n")
