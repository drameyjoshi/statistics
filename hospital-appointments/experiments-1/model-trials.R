data <- read.csv('model_data.csv')
set.seed(15081947)
N <- nrow(data)

diagnose_cm <- function(cm) {
  # Compute the standard metrics from a confusion matrix.
  
  err_rate <- (cm[1, 2] + cm[2, 1])/sum(cm)
  precision <- cm[2, 2]/(cm[2, 2] + cm[2, 1]) # TP/(TP + FP)
  recall <- cm[2, 2]/(cm[2, 2] + cm[1, 2])    # TP/(TP + FN)
  f1.score <- 2*precision*recall/(precision + recall)
  
  print(paste('Error rate =', round(err_rate * 100, 3), "%."))
  print(paste('Precision =', round(precision * 100, 3), "%."))
  print(paste('Recall =', round(recall * 100, 3), "%."))
  print(paste('F1-score =', round(f1.score * 100, 3), "%."))
}

trn.samples <- sample.int(N, N * 0.7)

trn.data <- data[trn.samples, ]
tst.data <- data[-trn.samples, ]

try_glm <- function(model.str) {
  glm.res <- glm(as.formula(model.str), data = trn.data, family = binomial)
  glm.prob <- predict(glm.res, newdata = tst.data, type='response')
  glm.pred <- rep(0, length(glm.prob))
  glm.pred[glm.prob > 0.5] <- 1
  cm <- table(glm.pred, tst.data$Outcome)
  print(model.str)
  diagnose_cm(cm)
  print(paste(rep('*', 80), collapse=""))
  cm
}

models <- 
  c('Outcome ~ Age.Grp + SMS_received + Gap.Grp + VisitNum + CumNoShow',
    'Outcome ~ SMS_received + Gap.Grp + VisitNum + CumNoShow',
    'Outcome ~ Age.Grp + Gap.Grp + VisitNum + CumNoShow',
    'Outcome ~ Age.Grp + SMS_received + VisitNum + CumNoShow',
    'Outcome ~ Age.Grp + SMS_received + Gap.Grp + CumNoShow',
    'Outcome ~ Gap.Grp + VisitNum + CumNoShow',
    'Outcome ~ Age.Grp + VisitNum + CumNoShow',
    'Outcome ~ Age.Grp + SMS_received + Gap.Grp')

results <- c()
for (m in models) {
  resutls <- c(try_glm(m), results)
}

# [1] "Outcome ~ Age.Grp + SMS_received + Gap.Grp + VisitNum + CumNoShow"
# [1] "Error rate = 6.514 %."
# [1] "Precision = 86.42 %."
# [1] "Recall = 80.503 %."
# [1] "F1-score = 83.356 %."
# [1] "********************************************************************************"
# [1] "Outcome ~ SMS_received + Gap.Grp + VisitNum + CumNoShow"
# [1] "Error rate = 6.556 %."
# [1] "Precision = 86.695 %."
# [1] "Recall = 79.908 %."
# [1] "F1-score = 83.163 %."
# [1] "********************************************************************************"
# [1] "Outcome ~ Age.Grp + Gap.Grp + VisitNum + CumNoShow"
# [1] "Error rate = 6.484 %."
# [1] "Precision = 86.675 %."
# [1] "Recall = 80.354 %."
# [1] "F1-score = 83.395 %."
# [1] "********************************************************************************"
# [1] "Outcome ~ Age.Grp + SMS_received + VisitNum + CumNoShow"
# [1] "Error rate = 6.587 %."
# [1] "Precision = 85.647 %."
# [1] "Recall = 81.083 %."
# [1] "F1-score = 83.303 %."
# [1] "********************************************************************************"
# [1] "Outcome ~ Age.Grp + SMS_received + Gap.Grp + CumNoShow"
# [1] "Error rate = 12.917 %."
# [1] "Precision = 75.312 %."
# [1] "Recall = 53.937 %."
# [1] "F1-score = 62.857 %."
# [1] "********************************************************************************"
# [1] "Outcome ~ Gap.Grp + VisitNum + CumNoShow"
# [1] "Error rate = 6.415 %."
# [1] "Precision = 86.26 %."
# [1] "Recall = 81.292 %."
# [1] "F1-score = 83.702 %."
# [1] "********************************************************************************"
# [1] "Outcome ~ Age.Grp + VisitNum + CumNoShow"
# [1] "Error rate = 7.244 %."
# [1] "Precision = 88.469 %."
# [1] "Recall = 73.88 %."
# [1] "F1-score = 80.519 %."
# [1] "********************************************************************************"
# [1] "Outcome ~ Age.Grp + SMS_received + Gap.Grp"
# [1] "Error rate = 20.502 %."
# [1] "Precision = 40.749 %."
# [1] "Recall = 2.59 %."
# [1] "F1-score = 4.87 %."
# [1] "********************************************************************************"
