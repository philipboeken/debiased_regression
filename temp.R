library(mgcv)
source("experiment.R")

n <- 300
seed <- 10
graph_nr <- 1

amat <- get_graph(graph_nr)
all_data <- simulate_nonlinear(amat, n, seed)
all_data$weights_true <- 1 / all_data$pi
selected_data <- all_data[all_data$S, ]
rejected_data <- all_data[!all_data$S, ]

# IPW with true weights
ipw_model_1 <- gam(Y ~ s(X, bs = "gp"), data = selected_data, weights = selected_data$weights_true/1000)
all_data$yhat_ipw_1 <- predict(ipw_model_1, data.frame(X = all_data$X))

# ipw_model_2 <- gam(Y ~ s(X, bs = "gp"), data = selected_data, weights = selected_data$weights_true)
ipw_model_2 <- gam(Y ~ s(X, bs = "gp"), data = selected_data, weights = rep(1, sum(all_data$S)))
all_data$yhat_ipw_2 <- predict(ipw_model_2, data.frame(X = all_data$X))

ord <- order(all_data$X)
par(mar = c(1, 1, 1, 1))
plot(range(all_data$X), range(all_data$Y), type = "n", xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
points(rejected_data$X, rejected_data$Y, pch = 16, cex = .75, col = "grey")
# points(selected_data$X, selected_data$Y, pch = 16, cex = .75)
symbols(selected_data$X, selected_data$Y, circles = selected_data$weights_true, bg="black", inches = 1 / 3, add = TRUE)
lines(all_data$X[ord], all_data$yhat_ipw_1[ord], pch = 16, col = "#0072B2", lwd = 2.5)
lines(all_data$X[ord], all_data$yhat_ipw_2[ord], pch = 16, col = "#56B4E9", lwd = 2.5)







########################

# x <- seq(-1, 1, length.out=1000)
# grid <- expand.grid(x, x)
# covs <- apply(grid, 1, function(i) matern_kernel(i[1], i[2]))
# expand.grid.mat <- function(...) as.matrix(expand.grid(...))