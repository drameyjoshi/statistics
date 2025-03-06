# Surface area of a unit hypersphere of D dimensions.
surface_area <- function(D) {
  2 * pi ^ (D / 2) / gamma(D / 2)
}

rho <- function(D, r) {
  surface_area(D) * r ^ (D - 1) * exp(-r ^ 2 / 2) / (2 * pi) ^ (D / 2)
}

r.vals <- seq(from = 0, to = 10, by = 0.1)

plot(
  r.vals,
  rho(1, r.vals),
  type = "l",
  col = 1,
  xlab = expression(r),
  ylab = expression(rho(r)),
  main = "Radial part of gaussian distributions"
)
plot_p <- function(D, n) {
  lines(r.vals, rho(D, r.vals), col = n)
}

sample_set <- data.frame(dim = c(2, 5, 10, 20, 40),
                         col = c(2, 3, 4, 5, 6))
apply(sample_set, 1, function(x)
  plot_p(x[1], x[2]))
legend(
  "topright",
  legend = paste(sep = "", "D=", c(1, sample_set$dim)),
  col = c(1, sample_set$col),
  lty = rep(1, 6),
  cex = 0.6
)
