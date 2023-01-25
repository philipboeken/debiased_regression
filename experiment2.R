library(RCIT)
library(pbapply)

source("experiment.R")

library(mlbench)
data(BostonHousing2)
# https://cran.r-project.org/web/packages/mlbench/mlbench.pdf

all_mse_results <- pblapply(1:100, function(i) {
  # all_mse_results <- pblapply(1:2, function(i) {
  all_data <- data.frame(
    X = BostonHousing2$rm, # average number of rooms per dwelling
    Z = BostonHousing2$lstat, # percentage of lower status of the population
    Y = BostonHousing2$cmedv # median value of owner-occupied homes in USD 1000’s
  )

  n <- nrow(all_data)

  all_data$pi <- numeric(n) + 1
  all_data$pi <- all_data$pi * sigmoid(scale(draw_gp(all_data$X, kernel_fn = se_kernel, length = 3 / 2)) * 3)
  all_data$pi <- all_data$pi * sigmoid(scale(draw_gp(all_data$Z, kernel_fn = se_kernel, length = 3 / 2)) * 3)
  all_data$pi <- as.numeric(all_data$pi)
  all_data$S <- runif(n) < all_data$pi

  all_data <- cbind_true(all_data)
  all_data <- cbind_naive(all_data)
  all_data <- cbind_recursive(all_data)
  all_data <- cbind_ipw(all_data)
  all_data <- cbind_doubly_robust(all_data)

  if (i == 1) {
    plot(all_data$X, all_data$pi)
    plot(all_data$Z, all_data$pi)

    alpha <- 0.01
    print(RCoT(all_data$X, all_data$Y)$p < alpha)
    print(RCoT(all_data$S, all_data$Y)$p < alpha)
    print(RCoT(all_data$S, all_data$Y, all_data$X)$p < alpha)
    print(RCoT(all_data$S, all_data$Y, as.matrix(all_data[, c("X", "Z")]))$p > alpha)

    selected_data <- all_data[all_data$S, ]
    plot_results(all_data, weights_obs = selected_data$weights_true_trans_05)
  }

  combined_mse_results <- get_mse_formatted(all_mse_results)

  mse_result <- get_mse_result(all_data)
  idx_order <- order(mse_result$y)
  mse_result <- mse_result[idx_order, ]
  mse_result
})

print(combined_mse_results)
