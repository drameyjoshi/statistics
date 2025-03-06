library('class')
set.seed(18111842)

data.0 <- read.csv('clean_data.csv')
cols.of.interest <-
  c(
    'Age',
    'Neighbourhood',
    'Hipertension',
    'Diabetes',
    'SMS_received',
    'Gap.d',
    'No.show'
  )

# k-NN works only with numerical X's.
Xs <- cols.of.interest[c(1,3,4,5,6)]
data <- unique(data.0[, cols.of.interest])
rm('data.0')

data$No.show <- as.factor(data$No.show)
trn.samples <- sample(1:nrow(data), 0.7 * nrow(data))

trn.data <- data[trn.samples,]
tst.data <- data[-trn.samples,]

print_cm_results <- function(cm) {
  errors <- cm[1, 2] + cm[2, 1]
  print(paste('Error rate = ', round(errors / sum(cm) * 100, 2), "%"))
}

# k = 5
pred.5 <- knn(trn.data[, Xs], tst.data[, Xs], trn.data$No.show, k = 5)
cm <- table(pred.5, tst.data$No.show)
print_cm_results(cm)

for (i in seq(5, 15)) {
  pred <- knn(trn.data[, Xs], tst.data[, Xs], trn.data$No.show, k = i)
  cm <- table(pred, tst.data$No.show)
  print_cm_results(cm)
}

# The error rate monotonically drops from 31% with k=5 to 29.29% for k=15. Yet
# it is unacceptably high.
