library(plotly)
source("experiment.R")

set.seed(1)

save_figs <- FALSE
# save_figs <- TRUE

n <- 400

################################################
# Simulate data
f_Z <- function(x) 3 * sin(x)
f_Y <- function(x, z) z + x / 2
sd_X <- 3
sd_Z <- 2
sd_Y <- 2

all_data <- data.frame(X = rnorm(n, 0, sd_X))
all_data$Z <- f_Z(all_data$X) + rnorm(n, 0, sd_Z)
all_data$eps_Y <- rnorm(n, 0, sd_Y)
all_data$Y <- f_Y(all_data$X, all_data$Z) + all_data$eps_Y
ftrue <- function(x) f_Y(x, f_Z(x))

all_data$pi <- (all_data$X < (mean(all_data$X) + 2)) *
  trans_linear(sigmoid(as.numeric(scale(all_data$Z) * 20)), 1 / 20, 1)
all_data$S <- runif(n) < all_data$pi
selected_data <- all_data[all_data$S, ]

################################################
# Plot true and naive fit
all_data$yhat_true <- ftrue(all_data$X)
all_data <- cbind_naive(all_data)
if (save_figs) pdf("output/figures/example/1_true_and_naive.pdf", width = 6, height = 4)
plot_results(all_data, legend_flag = TRUE)
if (save_figs) dev.off()

################################################
# Plot imputed values
all_data <- cbind_recursive(all_data, impute_linear = FALSE)
if (save_figs) pdf("output/figures/example/2a_imputed.pdf", width = 6, height = 4)
offsets_y <- c(25, 45, 62)
loc_x <- min(all_data$X) - 3

plot_results(all_data[, c("X", "Y", "S", "y_imputed")], xlim = c(loc_x, max(all_data$X)), ylim = c(min(all_data$Y), max(all_data$Y) + max(offsets_y)))

text(loc_x, mean(all_data$eps_Y) + offsets_y[3] - 1, latex2exp::TeX("$\\epsilon_Y$"), cex = 1.8, pos = 3, col = "black")
points(all_data$X, all_data$eps_Y + offsets_y[3], cex = .75, pch = 16, col = "gray")
text(mean(all_data$X), 48, latex2exp::TeX("$+$"), cex = 2, pos = 3, col = "black")

text(loc_x, mean(all_data$X / 2) + offsets_y[2] - 5, latex2exp::TeX("$\\frac{1}{2}X$"), cex = 1.5, pos = 3, col = "black")
points(all_data$X, all_data$X / 2 + offsets_y[2], cex = .75, pch = 16)
text(mean(all_data$X), 34, latex2exp::TeX("$+$"), cex = 2, pos = 3, col = "black")

text(loc_x, mean(all_data$Z) + offsets_y[1] - 1, latex2exp::TeX("$Z$"), cex = 1.5, pos = 3, col = "black")
points(all_data$X, all_data$Z + offsets_y[1], cex = .75, pch = 16)
text(mean(all_data$X), 10, latex2exp::TeX("$=$"), cex = 2, pos = 3, col = "black")
text(loc_x, mean(all_data$Y) - 1, latex2exp::TeX("$Y$"), cex = 1.5, pos = 3, col = "black")
if (save_figs) dev.off()

print(lm(Y ~ X + Z, data = selected_data)$coefficients)

################################################
# Plot recursive method
if (save_figs) pdf("output/figures/example/2b_imputed.pdf", width = 6, height = 4)
plot_results(all_data, legend_flag = TRUE)
if (save_figs) dev.off()

################################################
# Plot IPW estimator with estimated probabilities
all_data <- cbind_ipw(all_data)
selected_data <- all_data[all_data$S, ]
if (save_figs) pdf("output/figures/example/3_ipw_estimated_weights.pdf", width = 6, height = 4)
plot_results(all_data[, c("X", "S", "Y", "yhat_true", "yhat_naive", "yhat_ipw_est")],
  weights_obs = trans_linear(selected_data$weights_est, 0.75, max(selected_data$weights_est) * 3 / 5),
  legend_flag = TRUE
)
if (save_figs) dev.off()

################################################
# Plot IPW estimator with known probabilities
if (save_figs) pdf("output/figures/example/4_ipw_true_weights.pdf", width = 6, height = 4)
plot_results(all_data[, c("X", "S", "Y", "yhat_true", "yhat_naive", "yhat_ipw_true")],
  weights_obs = trans_linear(selected_data$weights_true, 0.75, max(selected_data$weights_true) * 3 / 5), 
  legend_flag = TRUE
)
if (save_figs) dev.off()

################################################
# Plot naive, the IPW estimated residuals, and the doubly robust estimator
all_data <- cbind_doubly_robust(all_data, direct_method = "yhat_naive")
selected_data <- all_data[all_data$S, ]
offset <- max(selected_data$dr_resid) - min(all_data$Y) + 5
ord <- order(all_data$X)
if (save_figs) pdf("output/figures/example/5_dr_true_weights.pdf", width = 6, height = 4)
plot_results(all_data[, c("X", "S", "Y", "yhat_true", "yhat_naive", "yhat_dr_true")],
  ylim = c(min(all_data$Y) - offset, max(all_data$Y)), legend_flag = FALSE
)
points(selected_data$X, selected_data$dr_resid - offset, pch = 16, col = "black",
       cex =  trans_linear(selected_data$weights_true, 0.75, max(selected_data$weights_true) * 3 / 5))
lines(all_data$X[ord], all_data$residhat_ipw_true[ord] - offset, col = "#F0E442", lwd = 2.5)
items <- c("yhat_true", "yhat_naive", "yhat_dr_true")
legend_labels_temp <- legend_labels
legend_labels_temp["yhat_dr_true"] <- "Doubly Robust"
legend("right", legend = c(legend_labels_temp[items], "Residuals"), col = c(palette[items], "#F0E442"),
        lty = 1, lwd = 2.5, inset = 0.01, bg = "white"
)
if (save_figs) dev.off()

################################################
# Plot overview of naive, imputed, IPW and DR estimates
if (save_figs) pdf("output/figures/example/6_overview.pdf", width = 6, height = 4)
plot_results(all_data, legend_flag = TRUE)
if (save_figs) dev.off()

######################## Plot 3D ########################
# rejected_data <- all_data[!all_data$S, ]
# fig <- plot_ly(all_data) %>%
#   add_trace(
#     x = ~ selected_data$X, y = ~ selected_data$Z, z = ~ selected_data$Y,
#     name = "selected", type = "scatter3d", mode = "markers",
#     marker = list(size = 3, color = "black")
#   ) %>%
#   add_trace(
#     x = ~ rejected_data$X, y = ~ rejected_data$Z, z = ~ rejected_data$Y,
#     name = "rejected", type = "scatter3d", mode = "markers",
#     marker = list(size = 3, color = "darkgrey")
#   ) %>%
#   add_trace(
#     x = ~ all_data$X, y = ~ all_data$Z, z = ~ all_data$y_imputed,
#     name = "imputed", type = "scatter3d", mode = "markers",
#     marker = list(size = 3, color = "orange")
#   ) %>%
#   add_trace(
#     x = ~ all_data$X, y = ~ all_data$Z, z = ~ all_data$yhat_recursive,
#     name = "yhat_recursive", type = "scatter3d", mode = "markers",
#     marker = list(size = 3, color = "red")
#   ) %>%
#   layout(scene = list(
#     xaxis = list(title = "X"),
#     yaxis = list(title = "Z"),
#     zaxis = list(title = "Y")
#   ))
# fig


######################## Evaluation ########################
mse_result <- get_mse_result(all_data)
# print(mse_result)
