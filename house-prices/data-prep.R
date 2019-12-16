# The list of variables was generated using the shell command
# cat variables.txt | grep Keep | cut -d":" -f1 | sed 's/\([[:alnum:]][[:alnum:]]*\)/"\1",/g'
var.list <- c(
  "Id",
  "MSSubClass",
  "MSZoning",
  "LotShape",
  "LandContour",
  "LotConfig",
  "Neighborhood",
  "Condition1",
  "BldgType",
  "HouseStyle",
  "OverallQual",
  "OverallCond",
  "RoofStyle",
  "ExterQual",
  "ExterCond",
  "Foundation",
  "BsmtQual",
  "BsmtCond",
  "BsmtExposure",
  "TotalBsmtSF",
  "HeatingQC",
  "CentralAir",
  "Electrical",
  "GrLivArea",
  "BsmtFullBath",
  "FullBath",
  "KitchenQual",
  "TotRmsAbvGrd",
  "Functional",
  "FireplaceQu",
  "GarageFinish",
  "GarageCond",
  "PavedDrive",
  "YearBuilt",
  "YrSold",
  "SaleType",
  "SaleCondition",
  "SalePrice"
)
# Generated using
# cat level1.vars.txt  | grep drop | sed "s/  */ /g" | cut -d" " -f2 | sed 's/\([[:alnum:]][[:alnum:]]*\)/"\1",/g'
drop.list <- c(
  "LotShape",
  "Condition1",
  "BldgType",
  "HouseStyle",
  "OverallCond",
  "RoofStyle",
  "ExterQual",
  "ExterCond",
  "BsmtQual",
  "BsmtCond",
  "KitchenQual",
  "GarageCond",
  "GarageFinish",
  "FireplaceQu"
)

training.data <- read.csv("train.csv")
raw.data <- training.data[, var.list]
raw.data <- raw.data[,!colnames(raw.data) %in% drop.list]

# Compute age and split age into bins
max.age <- max(raw.data$YrSold - raw.data$YearBuilt)
raw.data$Age.Lvl <- cut(raw.data$YrSold - raw.data$YearBuilt,
                        breaks = seq(from = -10, to = round(max.age, -1), by = 10))

# MSZoning
X <-
  aggregate(cbind(avg.price = SalePrice) ~ MSZoning, training.data, mean)
mapper <- data.frame(MSZoning = X[order(X$avg.price), "MSZoning"],
                     MSZoning.1 = as.factor(c(1, 2, 2, 3, 4)))
raw.data <- merge(raw.data, mapper)
rm(mapper)
drop.list.1 <- c("MSZoning")

# LotConfig
X <-
  aggregate(cbind(avg.price = SalePrice) ~ LotConfig, training.data, mean)
mapper <- data.frame(LotConfig = X[order(X$avg.price), "LotConfig"],
                     LotConfig.1 = as.factor(c(rep(1, 3), 2, 2)))
raw.data <- merge(raw.data, mapper)
rm(mapper)
drop.list.1 <- c(drop.list.1, "LotConfig")

# Neighborhood
X <-
  aggregate(cbind(avg.price = SalePrice) ~ Neighborhood, training.data, mean)
mapper <-
  data.frame(Neighborhood = X[order(X$avg.price), "Neighborhood"],
             Neighborhood.1 = as.factor(c(
               rep(1, 3), rep(2, 3), rep(3, 6), rep(4, 5), rep(5, 5), rep(6, 3)
             )))
raw.data <- merge(raw.data, mapper)
rm(mapper)
drop.list.1 <- c(drop.list.1, "Neighborhood")

# OverallQual
X <-
  aggregate(cbind(avg.price = SalePrice) ~ OverallQual, training.data, mean)
mapper <-
  data.frame(OverallQual = X[order(X$avg.price), "OverallQual"],
             OverallQual.1 = as.factor(c(2, 2, 3, 4, 5, 6, 7, 8 , 9 , 10)))
raw.data <- merge(raw.data, mapper)
rm(mapper)
drop.list.1 <- c(drop.list.1, "OverallQual")

# Electrical
X <-
  aggregate(cbind(avg.price = SalePrice) ~ Electrical, training.data, mean)
mapper <-
  data.frame(Electrical = X[order(X$avg.price), "Electrical"],
             Electrical.1 = as.factor(c(1, rep(2, 3), 3)))
raw.data <-
  merge(raw.data, mapper, all.x = TRUE) # One case has NA for Electrical
rm(mapper)
drop.list.1 <- c(drop.list.1, "Electrical")

# BsmtFullBath
X <-
  aggregate(cbind(avg.price = SalePrice) ~ BsmtFullBath, training.data, mean)
mapper <-
  data.frame(BsmtFullBath = X[order(X$avg.price), "BsmtFullBath"],
             BsmtFullBath.1 = as.factor(c(rep(0, 2), rep(1, 2))))
raw.data <- merge(raw.data, mapper)
rm(mapper)
drop.list.1 <- c(drop.list.1, "BsmtFullBath")

# Functional
X <-
  aggregate(cbind(avg.price = SalePrice) ~ Functional, training.data, mean)
mapper <-
  data.frame(Functional = X[order(X$avg.price), "Functional"],
             Functional.1 = as.factor(c(1, 1, rep(2, 4), 3)))
raw.data <- merge(raw.data, mapper)
rm(mapper)
drop.list.1 <- c(drop.list.1, "Functional")

# SaleType
X <-
  aggregate(cbind(avg.price = SalePrice) ~ SaleType, training.data, mean)
mapper <- data.frame(SaleType = X[order(X$avg.price), "SaleType"],
                     SaleType.1 = as.factor(c(1, rep(2, 3), 3, rep(4, 2), rep(5, 2))))
raw.data <- merge(raw.data, mapper)
rm(mapper)
drop.list.1 <- c(drop.list.1, "SaleType")

# SaleCondition
X <-
  aggregate(cbind(avg.price = SalePrice) ~ SaleCondition,
            training.data,
            mean)
mapper <-
  data.frame(SaleCondition = X[order(X$avg.price), "SaleCondition"],
             SaleCondition.1 = as.factor(c(1, 2, 2, 3, 3, 4)))
raw.data <- merge(raw.data, mapper)
rm(mapper)
drop.list.1 <- c(drop.list.1, "SaleCondition")

# Drop the old columns
raw.data <- raw.data[,!(colnames(raw.data) %in% drop.list.1)]

# Convert a few variables into levels, which they are
raw.data$MSSubClass <- as.factor(raw.data$MSSubClass)
raw.data$FullBath <- as.factor(raw.data$FullBath)
raw.data$TotRmsAbvGrd <- as.factor(raw.data$TotRmsAbvGrd)

# Convert GrLivArea into as factor
low.limit <- round(min(raw.data$GrLivArea), -2)
high.limit <- round(max(raw.data$GrLivArea), -2) + 100
raw.data$GrLivArea.Lvl <- cut(raw.data$GrLivArea, 
                              breaks = seq(low.limit, high.limit, by = 100))

drop.list.2 <- c("YearBuilt", "YearSold", "GrLivArea")
raw.data <- raw.data[, !(colnames(raw.data) %in% drop.list.2)]

# We will use only complete cases for our analysis.
saveRDS(raw.data[complete.cases(raw.data), ], "model_data.Rds")
rm(list = ls())
