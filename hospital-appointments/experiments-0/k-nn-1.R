library('class')
set.seed(18111842)

data <- read.csv('clean_data_v2.csv')
data$No.show <- as.factor(data$No.show)

trn.samples <- sample(1:nrow(data), 0.7 * nrow(data))

trn.data <- data[trn.samples,]
tst.data <- data[-trn.samples,]

print_cm_results <- function(cm, n) {
  errors <- cm[1, 2] + cm[2, 1]
  print(paste('Error rate = ', round(errors / sum(cm) * 100, 2), "%, NN =", n))
}

trn.data.1 <- trn.data[trn.data$Nbd.class == 'High', ]
tst.data.1 <- tst.data[tst.data$Nbd.class == 'High', ]
# Xs <- colnames(data)[c(6, 8, 9, 10, 13, 15)]
Xs <- colnames(data)[c(6, 8, 10, 15)]
for (i in seq(15, 25)) {
  pred <- knn(trn.data.1[, Xs], tst.data.1[, Xs], trn.data.1$No.show, k = i)
  cm <- table(pred, tst.data.1$No.show)
  print_cm_results(cm, i)
}
# N = 20 is a good choice at error rate 18.88%.

trn.data.2 <- trn.data[trn.data$Nbd.class == 'Highest', ]
tst.data.2 <- tst.data[tst.data$Nbd.class == 'Highest', ]
for (i in seq(15, 25)) {
  pred <- knn(trn.data.2[, Xs], tst.data.2[, Xs], trn.data.2$No.show, k = i)
  cm <- table(pred, tst.data.2$No.show)
  print_cm_results(cm, i)
}
# N = 20 is a good choice at error rate 16.44%%.

# No choice of N is good.
Xs <- colnames(data)[c(6, 9, 8, 10, 15)]
trn.data.4 <- trn.data[trn.data$Nbd.class == 'Low', ]
trn.data.4$Age <- scale(trn.data.4$Age)
trn.data.4$Gap.d <- scale(trn.data.4$Gap.d)
tst.data.4 <- tst.data[tst.data$Nbd.class == 'Low', ]
tst.data.4$Age <- scale(tst.data.4$Age)
tst.data.4$Gap.d <- scale(tst.data.4$Gap.d)
for (i in seq(15, 25)) {
  pred <- knn(trn.data.4[, Xs], tst.data.4[, Xs], trn.data.4$No.show, k = i)
  cm <- table(pred, tst.data.4$No.show)
  print_cm_results(cm, i)
}

glm.1 <- glm(No.show ~ Age + Gap.d,
             data = trn.data.3,
             family = binomial)
prob.3 <- predict(glm.1, type='response')
pred.3 <- rep('0', length(prob.3))
pred.3[prob.3 > 0.5] <- '1'
table(pred.3, trn.data.3$No.show)
