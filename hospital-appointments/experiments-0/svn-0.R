library('e1071')

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

# SVM works only with numerical X's.
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

svm.1 <- svm(No.show ~ Age + Hipertension + Diabetes + SMS_received + Gap.d,
             data = trn.data,
             kernel = 'radial',
             gamma = 1,
             cost = 1)
pred.1 <- fitted(svm.1)
cm <- table(pred.1, trn.data$No.show)
print_cm_results(cm)
# Error rate is 28.28%, quite bad.