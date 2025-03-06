library(glmnet)

data <- read.csv('model_data.csv')
set.seed(15081947)
N <- nrow(data)

trn.samples <- sample.int(N, N * 0.7)
trn.data <- data[trn.samples,]
tst.data <- data[-trn.samples,]

all.Xs <- colnames(data)[c(2:4, 6, 8:10)]
model.1 <- all.Xs[c(2:5, 7)]
X <- trn.data[, model.1]
y <- trn.data[, 'Outcome']
result.1 <- glmnet(as.matrix(X), y, family = "binomial", alpha = 1)
cv.1 <-
  cv.glmnet(
    as.matrix(data[, model.1]),
    data[, 'Outcome'],
    nfolds = 13,
    family = "binomial",
    alpha = 1
  ) # 13 is a factor of 110526

plot(cv.1)

# The coefficients of the model with least lambda are:
coef(cv.1$glmnet.fit, s = cv.1$lambda.min)
# 6 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)   0.223748136
# SMS_received  0.019591434
# VisitNum     -0.048825303
# CumNoShow     0.329623732
# Age.Grp      -0.004292481
# Gap.Grp       0.040077374
print(paste("Mean cross-validated error:", cv.1$cvm[length(cv.1$cvm)]))
