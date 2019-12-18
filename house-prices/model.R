library(MASS)
setwd("house-prices")
model.data <- readRDS("model_data.Rds")
all.predictors <- colnames(model.data)[-c(1, 13)]

# A straight-forward linear model.
lm.v1 <-
  lm(reformulate(response = "SalePrice", termlabels = all.predictors),
     data = model.data)

# Let us check if the residuals are normal.
t.residuals <- studres(lm.v1)
qqnorm(t.residuals, cex = 0.5)
qqline(t.residuals)

# The residuals seem to have a fat tail. A histogram and density plot are
hist(
  t.residuals,
  freq = FALSE,
  main = "Model residuals",
  xlab = "Studentized residual",
  ylim = c(0, 0.5)
)
lines(density(t.residuals, na.rm = TRUE))
x <- seq(from = -6,
         to = 6,
         length.out = 500)
lines(x, dnorm(x), lty = 2, col = 2)
legend(
  "topright",
  legend = c("Est. density", "Normal density"),
  col = c(1, 2),
  lty = c(1, 2),
  cex = 0.6
)
rm(x)
# A comparison of the studentized residuals and the standard normal density
# indicates that residuals are not normally distributed. Let us confirm that
# using the Shapiro-Wilk normality test whose null hypothesis is that the data
# are normal.
shapiro.test(residuals(lm.v1))

# The very small p-value suggests that our conclusion after seeing the density
# plot is correct.

# We now plot the fitted values with the residuals
plot(
  fitted(lm.v1),
  t.residuals,
  cex = 0.6,
  xlab = "Fitted sale price",
  ylab = "Studentized residuals",
  main = "Residuals"
)
abline(h = 0, lty = 2)
identify(fitted(lm.v1), t.residuals, model.data$Id, cex = 0.6)

# Refer to
# https://stats.stackexchange.com/questions/2492/is-normality-testing-essentially-useless

# Cases with Id values 899 and 1325 have the highest residuals. Let us now look
# at the influence of various data points.
lm.v1.hat <- lm.influence(lm.v1)$hat

# From the summary of the linear model, we read that there are 1297 degrees of
# freedom. The number of cases is 1421. Therefore, the trace of the hat matrix
# should be 1421 - 1297 = 124. We confirm it.
sum(lm.v1.hat) == 1421 - 1297

# Which cases have the most influence? The top ten influencers are
order(lm.v1.hat, decreasing = TRUE)[1:10]

# Their sale prices are
cbind(model.data[order(lm.v1.hat, decreasing = TRUE)[1:10], c("Id", "SalePrice")],
      Infl = lm.v1.hat[order(lm.v1.hat, decreasing = TRUE)[1:10]])

# Most of the sale prices are in the extreme range. Let us find out how many 
# cases have a larger than expected influence.
plot(lm.v1.hat, cex = 0.5, col = 3, ylab = "Influence")
abline(h = sum(lm.v1.hat)/nrow(model.data), lty = 2)

# A very large number of points have higher than expected variance.
n <- length(lm.v1.hat[lm.v1.hat > sum(lm.v1.hat)/nrow(model.data)])
print(paste("% cases with higher than expected influence", round(100 * n/nrow(model.data), 1)))

# Remove just the points with maximum residuals.
model.data.v2 <- model.data[!(model.data$Id %in% c(899, 1325)), ]
lm.v2 <-
  lm(reformulate(response = "SalePrice", termlabels = all.predictors),
     data = model.data.v2)
t.residuals.v2 <- studres(lm.v2)
qqnorm(t.residuals.v2, cex = 0.5, col = 2)
qqline(t.residuals.v2)
plot(
  fitted(lm.v2),
  t.residuals.v2,
  cex = 0.6,
  xlab = "Fitted sale price",
  ylab = "Studentized residuals",
  main = "Residuals"
)
abline(h = 0, lty = 2)
identify(fitted(lm.v2), t.residuals.v2, model.data.v2$Id, cex = 0.6)

lm.v2.hat <- lm.influence(lm.v2)$hat
sum(lm.v2.hat) == 1419 - 1295

order(lm.v2.hat, decreasing = TRUE)[1:10]

# Remove points with highest influence.
model.data.v3 <-model.data.v2[-order(lm.v2.hat, decreasing = TRUE)[1:8], ]
lm.v3 <-
  lm(reformulate(response = "SalePrice", termlabels = all.predictors),
     data = model.data.v3)
t.residuals.v3 <- studres(lm.v3)
qqnorm(t.residuals.v3, cex = 0.5, col = 2)
qqline(t.residuals.v3)
plot(
  fitted(lm.v3),
  t.residuals.v3,
  cex = 0.6,
  xlab = "Fitted sale price",
  ylab = "Studentized residuals",
  main = "Residuals"
)
abline(h = 0, lty = 2)

# We are not improving the linear model.
