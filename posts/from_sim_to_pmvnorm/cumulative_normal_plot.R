# Install and load required packages

library(mvtnorm)
library(viridis)

# Define grid and parameters
x <- seq(-4, 4, length.out = 80)
y <- seq(-4, 4, length.out = 80)
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.6, 0.6, 1), ncol = 2)

# Compute the CDF over grid
Z <- outer(x, y, Vectorize(function(x, y) {
  pmvnorm(lower = c(-Inf, -Inf), upper = c(x, y), mean = mu, sigma = Sigma)
}))
Z <- matrix(as.numeric(Z), nrow = length(x))

# Scale Z for color mapping
z_min <- min(Z)
z_max <- max(Z)
z_scaled <- (Z - z_min) / (z_max - z_min)

# Create color matrix using viridis
cols <- viridis(100)
cols <- viridis(100, option = "turbo")
cols <- viridis(100, option = "plasma")
col_indices <- as.numeric(cut(z_scaled, breaks = 100))
col_matrix <- matrix(cols[col_indices], nrow = length(x))

# Create facet color matrix (one color per facet)
facet_col <- col_matrix[-1, -1]

# 3D surface plot
persp(
  x, y, Z,
  theta = 45, phi = 25, expand = 0.7,
  col = facet_col,
  ticktype = "detailed",
  xlab = "X", ylab = "Y", zlab = "CDF",
  main = "Cumulative Bivariate Normal"
)
