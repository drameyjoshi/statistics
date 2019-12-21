library(randomForest)

model.data <- readRDS("model_data.Rds")
all.predictors <- colnames(model.data)[-c(10, 38)]
selected.predictors <-
  c(
    "Neighborhood.1",
    "OverallQual.1",
    "Functional.1",
    "Foundation",
    "SaleCondition.1",
    "HeatingQC",
    "TotRmsAbvGrd",
    "TotalBsmtSF",
    "GrLivArea",
    "MSSubClass",
    "MSZoning",
    "LandContour",
    "BsmtExposure",
    "CentralAir",
    "FullBath",
    "FireplaceQu",
    "GarageFinish",
    "PavedDrive"
  )
model.data.imputed <-
  rfImpute(reformulate(response = "SalePrice",
                       termlabels = selected.predictors),
           data = model.data)
rf.v1 <- randomForest(
  reformulate(response = "SalePrice",
              termlabels = selected.predictors),
  data = model.data.imputed,
  importance = TRUE
)
mean(rf.v1$rsq)
mean(rf.v1$mse) / 1460

evaluate_param_set <- function(ntree) {
  cat(paste("Running random forest with"), ntree, "trees.\n")
  rf <- randomForest(reformulate(response = "SalePrice",
                                 termlabels = selected.predictors),
                     data = model.data.imputed)
  mean.rsq <- mean(rf$rsq)
  oob.range <- range(rf$oob.times)
  c(ntree, mean.rsq, oob.range)
}

results <-
  sapply(seq(from = 100, to = 2000, by = 100), evaluate_param_set)
# Plot R^2
plot(
  results[1,],
  results[2,],
  cex = 0.6,
  xlab = "ntree",
  ylab = "rsq",
  main = expression(R ^ 2)
)
abline(h = mean(results[2,]), lty = 2)

ymin <- round(min(results[3, ]), -1) - 10
ymax <- round(max(results[4, ]), -1) + 10
plot(
  results[1, ],
  results[3, ],
  type = "l",
  lty = 2,
  ylim = c(ymin, ymax),
  xlab = "ntree",
  ylab = "OOB",
  main = "OOB range"
)
lines(results[1, ], results[4, ], lty = 3)
abline(v = results[1, which.min(results[4, ])], lty = 4, col = 2)
legend(
  locator(1),
  legend = c("Min. OOB", "Max. OOB", "Least max OOB"),
  lty = c(2, 3, 4),
  col = c(1, 1, 2),
  bty = "n",
  cex = 0.6
)
# The plot is saves in the file 'OOB-range.png'.

# The range of R^2 is quite narrow and most trees seem to be equally good at
# explaining the variance. We therefore select the one with least maximum OOB.
# It is a forest with 600 trees.

rf.final <- randomForest(
  reformulate(response = "SalePrice",
              termlabels = selected.predictors),
  data = model.data.imputed,
  importance = TRUE,
  ntree = 600
)
predict.final <- predict(rf.final, model.data.imputed)
plot(
  model.data.imputed$SalePrice,
  predict.final,
  xlab = "Actual",
  ylab = "Predicted",
  main = "Sale price",
  cex = 0.5,
  col = 3
)
abline(a = 0, b = 1)
# The plot is saved in the file rf-pred-v-actual-final.png

# Analysis of residuals.
residuals.final <- predict.final - model.data.imputed$SalePrice
plot(
  residuals.final,
  cex = 0.5,
  col = 2,
  xlab = "Case",
  ylab = "Residual",
  main = "Residual analysis"
)
abline(h = 0, lty = 2)
# The plot is saved in residual-analysis.png
