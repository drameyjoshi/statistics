library(nnet)
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

# No choice of N is good.
Xs <- colnames(data)[c(6, 8, 9, 10, 17)]
cname <- 'Highest'
trn.data.4 <- trn.data[trn.data$Nbd.class == cname, ]
tst.data.4 <- tst.data[tst.data$Nbd.class == cname, ]

nn.1 <-
  nnet(
    No.show ~ Scholarship + Hipertension + Diabetes + SMS_received + Gap.d + Age.Grp,
    data = trn.data.4,
    size = 4,
    decay = 1e-4,
    maxit = 200
  )
predicted.class <- predict(nn.1, trn.data.4, type='class')
cm <- table(predicted.class, trn.data.4$No.show)
table(predict(nn.1, tst.data.4, type="class"), tst.data.4$No.show)
cm
print_cm_results(cm)
cm

## Neural network fails even on 'Highest' class, where k-NN worked well.