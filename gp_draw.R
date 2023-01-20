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
