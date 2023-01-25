library(RCIT)
library(pbapply)

source("R/utils.R")

library(mlbench)
data(BostonHousing2)
# https://cran.r-project.org/web/packages/mlbench/mlbench.pdf

simulate_boston_housing <- function() {
  all_data <- data.frame(
    X = BostonHousing2$rm, # average number of rooms per dwelling
    Z = BostonHousing2$lstat, # percentage of lower status of the population
    Y = BostonHousing2$cmedv # median value of owner-occupied homes in USD 1000’s
  )

  n <- nrow(all_data)

  all_data$pi <- numeric(n) + 1
  all_data$pi <- all_data$pi * sigmoid(scale(draw_gp(all_data$X, kernel_fn = se_kernel, length = 3 / 2)) * 5)
  all_data$pi <- all_data$pi * sigmoid(scale(draw_gp(all_data$Z, kernel_fn = se_kernel, length = 3 / 2)) * 5)
  all_data$pi <- as.numeric(all_data$pi)
  all_data$S <- runif(n) < all_data$pi

  all_data
}

experiment2 <- function(m = 100, seed = 1) {
  set.seed(seed)

  all_mse_results <- pblapply(1:m, function(i) {
    all_data <- simulate_boston_housing()
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
      pdf("output/figures/exp2/plot.pdf", width = 7, height = 4)
      plot_results(all_data, weights_obs = selected_data$weights_true_trans_05)
      dev.off()
    }

    mse_result <- get_mse_result(all_data)
    idx_order <- order(mse_result$y)
    mse_result <- mse_result[idx_order, ]
    mse_result
  })

  all_mse_results
}

m <- get_arg_numeric(1, 100)
seed <- get_arg_numeric(2, 1)

all_mse_results <- experiment2(m, seed)

formatted <- get_mse_formatted(all_mse_results)
write_table(formatted, file = sprintf("output/tables/exp2/results_formatted_%s.txt", m), append = FALSE)