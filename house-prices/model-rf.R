library(randomForest)
setwd("house-prices")

model.data <- readRDS("model_data.Rds")
all.predictors <- colnames(model.data)[-c(10, 38)]
rf.1 <-
  randomForest(
    reformulate(response = "SalePrice", termlabels = all.predictors),
    data = model.data,
    importance = TRUE
  )
# Random forest fails because it cannot handle more than 53 levels in all the
# categorical variables put together. Let us first examine the number of levels
# in each variable.
sort(sapply(sapply(model.data, levels), length))
# We then sort the variables by the number of columns and then compute the
# cumulative sum
cumsum(sort(sapply(sapply(model.data, levels), length)))
# Based on our prior analysis we know that we definitely need Neighborhood (6, 0),
# OverallQual (9, 15), Functional (3, 18), Foundation (6, 24), SaleCondition (4, 28),
# HeatingQC (5, 33), TotRmsAbvGrd (12, 45)
selected.predictors <-
  c(
    "Neighborhood.1",
    "OverallQual.1",
    "Functional.1",
    "Foundation",
    "SaleCondition.1",
    "HeatingQC",
    "TotRmsAbvGrd",
    "TotalBsmtSF"
  )
rf.2 <- randomForest(
  reformulate(response = "SalePrice",
              termlabels = selected.predictors),
  data = model.data,
  importance = TRUE
)
pred.2 <- predict(rf.2, model.data)
plot(
  model.data$SalePrice,
  pred.2,
  xlab = "Actual price",
  ylab = "Est. price",
  main = "Predicted v actual sale price (V2)",
  cex = 0.3,
  col = 2
)
abline(a = 0, b = 1, lty = 2)
#identify(model.data$SalePrice, pred.2, model.data$Id, cex = 0.5)

# The fit isn't bad. How many cases were overestimated and how many were
# underestimated?
over.est <- length(pred.2[pred.2 > model.data$SalePrice])
under.est <- length(pred.2[pred.2 < model.data$SalePrice])
print(paste("# over est. =", over.est, "# under est. =", under.est))
# Zoom the plot. It is quite clear that the dashed line seems to have more points
# under it than over it. We are, in general, over-estimating the price.

# The importance of variables is
varImpPlot(rf.2, main = "Importance of variables", cex = 0.7)

# Does growing more trees help?
rf.3 <- update(rf.2, ntree = 1000)
pred.3 <- predict(rf.3, model.data)
plot(
  model.data$SalePrice,
  pred.3,
  xlab = "Actual price",
  ylab = "Est. price",
  main = "Predicted v actual sale price (V3)",
  cex = 0.3,
  col = 3
)
abline(a = 0, b = 1, lty = 2)
#identify(model.data$SalePrice, pred.2, model.data$Id, cex = 0.5)

over.est <- length(pred.3[pred.3 > model.data$SalePrice])
under.est <- length(pred.3[pred.3 < model.data$SalePrice])
print(paste("# over est. =", over.est, "# under est. =", under.est))

# Looks like growing more trees makes the model worse. Let us add GrLivArea as
# a numerical variable.
selected.predictors <- c(selected.predictors, "GrLivArea")
rf.4 <-  randomForest(
  reformulate(response = "SalePrice",
              termlabels = selected.predictors),
  data = model.data,
  importance = TRUE,
  ntree = 2000
)
pred.4 <- predict(rf.4, model.data)
plot(
  model.data$SalePrice,
  pred.4,
  xlab = "Actual price",
  ylab = "Est. price",
  main = "Predicted v actual sale price (V4)",
  cex = 0.3,
  col = 4
)
abline(a = 0, b = 1, lty = 2)
identify(model.data$SalePrice, pred.2, model.data$Id, cex = 0.5)

over.est <- length(pred.4[pred.4 > model.data$SalePrice])
under.est <- length(pred.4[pred.4 < model.data$SalePrice])
print(paste("# over est. =", over.est, "# under est. =", under.est))
summary(pred.4 - model.data$SalePrice)
summary(pred.2 - model.data$SalePrice)

saveRDS(rf.4, "rf-v4.Rds")

selected.predictors <-
  c(
    selected.predictors,
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
# The new predictors have NAs. Let us impute them.
model.data.imputed <- 
  rfImpute(reformulate(response = "SalePrice",
                       termlabels = selected.predictors),
           data = model.data)
rf.5 <-  randomForest(
  reformulate(response = "SalePrice",
              termlabels = selected.predictors),
  data = model.data.imputed,
  importance = TRUE,
  ntree = 1000,
  do.trace = TRUE)
pred.5 <- predict(rf.5, model.data.imputed)
plot(
  model.data.imputed$SalePrice,
  pred.5,
  xlab = "Actual price",
  ylab = "Est. price",
  main = "Predicted v actual sale price (V5)",
  cex = 0.3,
  col = 6
)
abline(a = 0, b = 1, lty = 2)
summary(pred.2 - model.data$SalePrice)
summary(pred.3 - model.data$SalePrice)
summary(pred.4 - model.data$SalePrice)
summary(pred.5 - model.data$SalePrice)

saveRDS(rf.5, "rf.5.Rds")
