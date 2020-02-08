# A simple regression model to predict 'better life index' using a country's GDP.
bli <- read.csv("datasets/bli.csv")
gdp <- read.csv("datasets/gdp.csv")
X <- merge(bli, gdp)[, c("Country", "Rank", "Quality.of.Life.Index", "GDP")]
colnames(X)[3] <- "BLI"

# View the GDP data
with(X, hist(
  GDP,
  breaks = seq(from = 0, to = 21000, by = 1000),
  xlab = "GDP",
  main = "GDP by countries"
))
# The histogram indicates that a few countries have a very large GDP while a 
# majority of countries have it below 3000. The countries with very large GDP 
# will be outliers in the model. Let us remove them. Prior to that, let us view
# the box plots.
old.par <- par(mfrow = c(1, 2))
with(X, boxplot(BLI, main = "BLI"))
with(X, boxplot(GDP, main = "GDP"))
par(old.par)

# There are a lot of outliers in GDP.
gdp.outliers <- boxplot.stats(X$GDP)$out

# Which cases are GDP outliers?
outliers <- X[which(X$GDP %in% gdp.outliers), ]
# I would have liked to use setdiff but it is not working!!
Y <- X[!(X$Country %in% outliers$Country), ]
# Are the two variables correlated?
with(Y, cor(GDP, BLI))
# There is only a 21% correlation. I don't expect a great model fit.
with(Y,
     plot(
       GDP,
       BLI,
       xlab = "GDP",
       ylab = "BLI",
       main = "BLI v GDP",
       cex = 0.3
     ))

m1 <- lm(BLI ~ GDP, data = Y)
abline(m1, lty = 2, col = "blue")

# The blue line shows the fitted model. How good is it?
summary(m1)
# Not a very promising model. Nevertheless, we proceed to look at the residuals.
plot(m1$residuals, ylab = "residuals", main = "Model residuals")
abline(h = 0)
# The residuals are not symmetric about zero. However, there is no trend in them.
# Are they normally distributed?
qqnorm(m1$residuals)
qqline(m1$residuals)

# How do density and histogram of the residuals look like?
hist(
  m1$residuals,
  xlab = "residuals",
  main = "Histogram of residuals",
  probability = TRUE,
  breaks = seq(from = -100, to = 100, by = 10)
)
lines(density(m1$residuals))
# Hard to tell if the residuals are normally distributed. 
shapiro.test(m1$residuals)
# The high p-value suggests that we cannot reject the null hypothesis that the
# data is normally distributed.

# Returning to the model summary, we note the following:
# 1. The R-squared and the adjusted R-squared are very low. Recall that R-squared
#    is the ratio of ESS to SS.
SS <- sum((Y$BLI - mean(Y$BLI))^2)
ESS <- sum((fitted(m1) - mean(Y$BLI))^2)
R.squared <- ESS/SS
# Only 4% of the total variation in the data is explained by the model. Another
# view point is that R.squared is the square of correlation between actual and
# predicted values. (That's also the reason it is called r^2.)

# Recall that adjusted R^2 is calculated as
adj.R.sq <- 1 - (1 - R.squared) * (nrow(Y) - 1)/(nrow(Y) - 1 - 1)
