# Taken from:
# https://www.r-bloggers.com/2019/07/sampling-paths-from-a-gaussian-process/

# CONSTANT KERNEL
const_kernel <- function(x, y, sigma = 1) {
  sigma
}

# RATIONAL QUADRATIC KERNEL
rq_kernel <- function(x, y, alpha = 1, sigma = 1, length = 1) {
  sigma^2 * (1 + sum((x - y)^2) / (2 * alpha * length^2))^(-alpha)
}

# SQUARED EXPONENTIAL KERNEL (RBF)
se_kernel <- function(x, y, sigma = 1 / 2, length = 1) {
  sigma^2 * exp(-sum((x - y)^2) / (2 * length^2))
}

# POLYNOMIAL KERNEL
poly_kernel <- function(x, y, sigma = 1, d = 1) {
  (sigma^2 + x * y)^d
}

# PERIODIC KERNEL
period_kernel <- function(x, y, p = 1, sigma = 1, length = 1) {
  sigma^2 * exp(-2 * sin(pi * abs(x - y) / p)^2 / length^2)
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

# BROWNIAN MOTION KERNEL
bm_kernel <- function(x, y) {
  pmin(x, y)
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

f_interp <- function(x, Y, X) {
  fn <- function(x) {
    if (x <= min(X)) {
      return(Y[1])
    }
    if (x >= max(X)) {
      return(Y[length(Y)])
    }
    idx <- max(findInterval(x, X), 1)
    p <- (X[idx + 1] - x) / (X[idx + 1] - X[idx])
    p * Y[idx] + (1 - p) * Y[idx + 1]
  }
  sapply(x, fn)
}

random_function <- function(xrange = c(-4, 4), n = 20) {
  x_seq <- sapply(xrange, function(ran) seq(ran[1], ran[2], length.out = n))
  x_grid <- expand.grid(data.frame(x_seq))
  Y <- draw_gp(x_seq, kernel_fn = se_kernel, length = 3 / 2)
  # Y <- draw_gp(x_seq, kernel_fn = matern_kernel, nu=2.5)
  # Y <- draw_gp(x_grid, kernel_fn = poly_kernel, d=3)
  # Y <- draw_gp(x_seq, kernel_fn = bm_kernel)
  # TODO INTERPOLATION DOESNT WORK ON GRID
  function(x) f_interp(x, Y, as.matrix(x_grid))
}

# fn <- random_function(xrange=list(c(-3,3)), n=20)
# x <- seq(-5, 5, length.out=100)
# Y <- fn(x)
# plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n")
# lines(x, Y)