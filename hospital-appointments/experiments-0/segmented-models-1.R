# We will try to fit a separate k-NN regression model for every neighbourhood.
data <- read.csv('clean_data_v1.csv')
neighbourhoods <- unique(data$Neighbourhood)
data$No.show <- as.factor(data$No.show)

data.1 <- data[data$Neighbourhood == neighbourhoods[1], ]
set.seed(15081947)
trn.samples <- sample.int(nrow(data.1), 0.7*nrow(data.1), replace=FALSE)

trn.data.1 <- data.1[trn.samples, ]
tst.data.1 <- data.1[-trn.samples, ]

print_cm_results <- function(cm, i) {
  errors <- cm[1, 2] + cm[2, 1]
  print(paste('Error rate = ', round(errors / sum(cm) * 100, 2), "% for NN =", i))
}

Xs <- colnames(data.1)[c(6, 8, 9, 10, 13, 15, 17)]
library('class')
for (i in seq(4:15)) {
  pred <- knn(trn.data.1[, Xs], tst.data.1[, Xs], trn.data.1$No.show, k = i)
  cm <- table(pred, tst.data.1$No.show)
  print_cm_results(cm, i)
}

#N = 6 gives best results.