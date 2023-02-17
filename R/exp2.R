library(RCIT)
library(pbapply)

source("R/utils.R")

library(mlbench)
data(BostonHousing2)
# https://cran.r-project.org/web/packages/mlbench/mlbench.pdf

simulate_boston_housing <- function() {
  all_data <- data.frame(
    X = scale(BostonHousing2$rm), # average number of rooms per dwelling
    Z = scale(BostonHousing2$lstat), # percentage of lower status of the population
    Y = scale(BostonHousing2$cmedv), # median value of owner-occupied homes in USD 1000’s
    S = numeric(length(BostonHousing2$rm))
  )

  n <- nrow(all_data)

  while (sum(all_data$S) < 120) {
    all_data$pi <- numeric(n) + 1
    all_data$pi <- all_data$pi * sigmoid(scale(draw_gp(all_data$X, kernel_fn = se_kernel, length = 3 / 2)) * 5)
    all_data$pi <- all_data$pi * sigmoid(scale(draw_gp(all_data$Z, kernel_fn = se_kernel, length = 3 / 2)) * 5)
    all_data$pi <- as.numeric(all_data$pi)
    all_data$S <- runif(n) < all_data$pi
  }

  all_data
}

experiment2 <- function(m = 100, seed = 1) {
  set.seed(seed)

  pblapply(1:m, function(i) {
    all_data <- simulate_boston_housing()
    n <- nrow(all_data)
    train_idx <- (1:n) %in% sample(1:n, round(n / 2))
    train_data <- all_data[train_idx, ]
    test_data <- all_data[!train_idx, ]

    test_data <- cbind_predictions(test_data, train_data,
      pi_model_data = train_data,
      imputation_model_data = train_data
    )
    all_data <- cbind_predictions(all_data)

    if (i == 1) {
      pdf("output/figures/exp2/selection_prob.pdf", width = 7, height = 4)
      plot(all_data$X, all_data$pi)
      plot(all_data$Z, all_data$pi)
      dev.off()

      alpha <- 0.01
      cat(
        RCoT(all_data$X, all_data$Y)$p < alpha,
        RCoT(all_data$S, all_data$Y)$p < alpha,
        RCoT(all_data$S, all_data$Y, all_data$X)$p < alpha,
        RCoT(all_data$S, all_data$Y, as.matrix(all_data[, c("X", "Z")]))$p > alpha,
        "\n"
      )

      selected_data <- all_data[all_data$S, ]
      pdf("output/figures/exp2/exp2_plot.pdf", width = 7, height = 4)
      plot_results(all_data, weights_obs = selected_data$weights_true_clipped, legend_flag = TRUE)
      title(xlab = "X", ylab = "Y", line = -1, cex.lab = 1.2, las = 3)
      dev.off()
    }

    mse_result <- get_mse_result(test_data)

    return(mse_result)
  })
}

m <- get_arg_numeric(1, 500)
seed <- get_arg_numeric(2, 1)

start <- Sys.time()
cat("\nStarting exp2.R", c(m, seed), "at", format(start), "\n")
data_filename <- sprintf("output/tables/exp2/results_%s.RData", m)
if (!file.exists(data_filename)) {
  all_mse_results <- experiment2(m, seed)
  all_mse_results <- transform_mse_results(all_mse_results)
  save(all_mse_results, file = data_filename)
} else {
  load(data_filename)
}

output_table(all_mse_results)
output_table(all_mse_results,
  columns = c("y", "y_interp", "y_extrap")
  # rows = c("yhat_repeated", "yhat_iw_true_clipped", "yhat_iw_est_clipped")
)

end <- Sys.time()
cat("\nFinished exp2.R", c(m, seed), "at", format(end), "in", format(end - start), "\n")
