library(plotly)
library(mgcv)

source("experiment.R")

# set.seed(5)
set.seed(1)

save_figs <- FALSE
# save_figs <- TRUE

n <- 700
f_Z <- function(x) 3 * sin(x) + x
f_Z <- function(x) 3 * sin(x)
f_Y <- function(x, z) z - 2*x
f_Y <- function(x, z) z + x / 2
sd_X <- 3
sd_Z <- 2
sd_Y <- 2

# X <- runif(n, -7, 7)
all_data <- data.frame(X = rnorm(n, 0, sd_X))
all_data$Z <- f_Z(all_data$X) + rnorm(n, 0, sd_Z)
all_data$Y <- f_Y(all_data$X, all_data$Z) + rnorm(n, 0, sd_Y)
ftrue <- function(x) f_Y(x, f_Z(x))

# P_S <- function(z) bump(x, 10, 1 / 10, 1) * bump(z, 10, 1 / 10, 1)
all_data$pi <- (all_data$X < (mean(all_data$X) + 2)) * 
  translate_between_values(sigmoid(as.numeric(scale(all_data$Z)*20)), 1/20, 1)
all_data$S <- runif(n) < all_data$pi
selected_data <- all_data[all_data$S, ]

# Plot true and naive fit
all_data$yhat_true <- ftrue(all_data$X)
all_data <- cbind_naive(all_data)
if(save_figs) pdf("output/figures/example/1_true_and_naive.pdf", width = 6, height = 4)
plot_results(all_data)
if(save_figs) dev.off()

# Plot imputed values, and direct recursive method, for GAM
all_data <- cbind_recursive(all_data, impute_linear = FALSE)
print(lm(Y ~ X + Z, data=selected_data)$coefficients)
if(save_figs) pdf("output/figures/example/2_imputed.pdf", width = 6, height = 4)
plot_results(all_data)
if(save_figs) dev.off()

# Plot direct IPW estimator for GAM with estimated probabilities
all_data <- cbind_ipw(all_data)
selected_data <- all_data[all_data$S, ]
if(save_figs) pdf("output/figures/example/3_ipw_estimated_weights.pdf", width = 6, height = 4)
plot_results(all_data[, c("X", "S", "Y", "yhat_true", "yhat_naive", "yhat_ipw_est")], weights_obs = selected_data$weights_est)
if(save_figs) dev.off()

# Plot direct IPW estimator for GAM with known probabilities
if(save_figs) pdf("output/figures/example/4_ipw_true_weights.pdf", width = 6, height = 4)
plot_results(all_data[, c("X", "S", "Y", "yhat_true", "yhat_naive", "yhat_ipw_true")], weights_obs = selected_data$weights_true)
if(save_figs) dev.off()

# Plot Poly direct, the gam-IPW estimated residuals,
# and the resulting doubly robust estimator
all_data <- cbind_doubly_robust(all_data)
selected_data <- all_data[all_data$S, ]
offset <- max(selected_data$resid_naive) - min(all_data$Y)
if(save_figs) pdf("output/figures/example/5_dr_true_weights.pdf", width = 6, height = 4)
plot_results(all_data[, c("X", "S", "Y", "yhat_true", "yhat_naive", "yhat_dr_est")],
  ylim = c(min(all_data$Y) - offset, max(all_data$Y))
)

ord <- order(all_data$X)
points(selected_data$X, selected_data$resid_naive - offset, pch=1, cex = selected_data$weights_true, col = "black")
lines(all_data$X[ord], all_data$residhat_ipw_est[ord] - offset, col = "#F0E442", lwd = 2.5)
if(save_figs) dev.off()

# Plot overview of naive, imputed, IPW and DR estimates
if(save_figs) pdf("output/figures/example/6_overview.pdf", width = 6, height = 4)
plot_results(all_data)
if(save_figs) dev.off()

##### 3D Plot P(S=1 | X, Z)
# if (output) {
#   pdf("./Figures/selection_probability.pdf", width = 7, height = 4)
# }
# seq.small <- seq(-8.7, 8.7, length.out = 30)
# pmat <- persp(seq.small, seq.small, outer(seq.small, seq.small, P_S),
#       scale=FALSE, ticktype="detailed",
#       expand=8,
#       xlab="X", ylab="Z", zlab="", theta=45, phi=15)
# selected.points <- trans3d(selected_data$X, selected_data$Z, P_S(selected_data$X, selected_data$Z), pmat=pmat)
# rejected.points <- trans3d(rejected_data$X, rejected_data$Z, P_S(rejected_data$X, rejected_data$Z), pmat=pmat)
# points(selected.points, pch = 16, col = "black", cex=.6)
# points(rejected.points, pch = 16, col = "darkgrey", cex=.6)
# if (output) {
#   dev.off()
# }

rejected_data <- all_data[!all_data$S, ]
fig <- plot_ly(all_data) %>%
  add_trace(x=~selected_data$X, y=~selected_data$Z, z=~selected_data$Y,
            name="selected", type = "scatter3d", mode = "markers",
            marker=list(size=3, color="black"))  %>%
  add_trace(x=~rejected_data$X, y=~rejected_data$Z, z=~rejected_data$Y,
            name="rejected", type = "scatter3d", mode = "markers",
            marker=list(size=3, color="darkgrey")) %>%
  add_trace(x=~all_data$X, y=~all_data$Z, z=~all_data$y_imputed,
            name="imputed", type = "scatter3d", mode = "markers",
            marker=list(size=3, color="orange")) %>%
  add_trace(x=~all_data$X, y=~all_data$Z, z=~all_data$yhat_recursive,
            name="yhat_recursive", type = "scatter3d", mode = "markers",
            marker=list(size=3, color="red")) %>%
  layout(scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Z"),
                      zaxis = list(title = "Y")))
fig


######################## Evaluation ####################
mse_result <- get_mse_result(all_data)
# print(mse_result)
