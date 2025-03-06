x.bar <- 39.12
y.bar <- 310.72
var.x <- 150.03
var.y <- 605.738
s.xy <- 798.24

# These two should give you the regression equation.
b1 <- s.xy/var.x
b0 <- y.bar - b1*x.bar
cat(paste("y =", b0, " + ", b1, "x\n"))

N <- 25 # Number of observations
regression.df <- 1 # This is a univariate regression
residual.df <- (N - 1) - regression.df

# Regression sum of square
reg.ss <- b1^2 * var.x
# Total sum of square can be immediately found from sample variance.
tot.ss <- (N - 1) * var.y
# Residual sum of square
res.ss <- tot.ss - reg.ss

# Regression variance
reg.var <- reg.ss/1
# Residual variance
res.var <- res.ss/(N - 2)

# F statistics
F.stat <- reg.var/res.var

# p-value
p = 1 - pf(F.stat, regression.df, residual.df)

print(paste("Regression df =", regression.df))
print(paste("Residual df =", residual.df))
print(paste("Regression SS =", reg.ss))
print(paste("Residual SS =", res.ss))
print(paste("Total SS =", tot.ss))
print(paste("Regression MS =", reg.var))
print(paste("Residual MS =", res.var))
print(paste("F statistics =", F.stat))
print(paste("p-value =", p))