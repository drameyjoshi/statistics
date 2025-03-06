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
model.1 <- 'Outcome ~ Age.Grp + Scholarship + Hipertension + SMS_received + Gap.Grp + VisitNum + CumNoShow'
glm.1 <- glm(as.formula(model.1),
             data = trn.data,
             family = binomial)
summary(glm.1)

# Remove the variables Scholarship and Hipertension as their coefficients are 
# not significant.

model.2 <- 'Outcome ~ Age.Grp + SMS_received + Gap.Grp + VisitNum + CumNoShow'
glm.2 <- glm(as.formula(model.2),
             data = trn.data,
             family = binomial)
summary(glm.2)

# Find performance on training data.
glm.2.prob <- predict(glm.2, type = 'response')
glm.2.pred <- rep(0, length(glm.2.prob))
glm.2.pred[glm.2.prob > 0.5] <- 1
cm <- table(glm.2.pred, trn.data$Outcome)
diagnose_cm(cm)

# Error rate = 6.358%

# Find performance on test data.
glm.2.prob <- predict(glm.2, newdata = tst.data, type='response')
glm.2.pred <- rep(0, length(glm.2.prob))
glm.2.pred[glm.2.prob > 0.5] <- 1
cm <- table(glm.2.pred, tst.data$Outcome)
diagnose_cm(cm)

# [1] "Error rate = 6.514 %."
# [1] "Precision = 86.42 %."
# [1] "Recall = 80.503 %."
# [1] "F1-score = 83.356 %."
# 
# Call:
# glm(formula = as.formula(model.2), family = binomial, data = trn.data)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -8.4904  -0.2874  -0.2071  -0.0524   7.2505  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.458861   0.034816 -41.902  < 2e-16 ***
#   Age.Grp      -0.042781   0.009038  -4.734 2.21e-06 ***
#   SMS_received  0.348146   0.032321  10.772  < 2e-16 ***
#   Gap.Grp       0.316896   0.011003  28.801  < 2e-16 ***
#   VisitNum     -1.378707   0.017329 -79.559  < 2e-16 ***
#   CumNoShow     4.412670   0.032976 133.814  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 77779  on 77367  degrees of freedom
# Residual deviance: 32863  on 77362  degrees of freedom
# AIC: 32875
# 
# Number of Fisher Scoring iterations: 8
#
# Interpretation:
# 1) The strongest influencer is CumNoShow. If a patient has missed an appointment
#    he is likely to miss it again. More the number of times he misses, greater
#    are his chances of missing again.
# 2) If a person has taken multiple appointments then there is a chance that he
#    will miss less.
# 3) The probability of missing an appointment rises if there is a large gap 
#    between schedule day and appointment day.
# 4) Strangely, receiving an SMS makes people miss an appointment.
# 5) Older patients miss an appointment with a lower probability.
# 
model.3 <- 'Outcome ~ Age.Grp + Gap.Grp + VisitNum + CumNoShow'
glm.3 <- glm(as.formula(model.3),
             data = trn.data,
             family = binomial)
# Find performance on training data.
glm.3.prob <- predict(glm.3, type = 'response')
glm.3.pred <- rep(0, length(glm.3.prob))
glm.3.pred[glm.3.prob > 0.5] <- 1
cm <- table(glm.3.pred, trn.data$Outcome)
diagnose_cm(cm)

# [1] "Error rate = 6.311 %."
# [1] "Precision = 86.39 %."
# [1] "Recall = 81.545 %."
# [1] "F1-score = 83.898 %."

# Find performance on test data.
glm.3.prob <- predict(glm.3, newdata = tst.data, type='response')
glm.3.pred <- rep(0, length(glm.3.prob))
glm.3.pred[glm.3.prob > 0.5] <- 1
cm <- table(glm.3.pred, tst.data$Outcome)
diagnose_cm(cm)
# [1] "Error rate = 6.484 %."
# [1] "Precision = 86.675 %."
# [1] "Recall = 80.354 %."
# [1] "F1-score = 83.395 %."

model.4 <- 'Outcome ~ Gap.Grp + VisitNum + CumNoShow'
glm.4 <- glm(as.formula(model.4), data = trn.data, family = binomial)
# Find performance on training data.
glm.4.prob <- predict(glm.4, type = 'response')
glm.4.pred <- rep(0, length(glm.4.prob))
glm.4.pred[glm.4.prob > 0.5] <- 1
cm <- table(glm.4.pred, trn.data$Outcome)
diagnose_cm(cm)

# [1] "Error rate = 6.24 %."
# [1] "Precision = 86.017 %."
# [1] "Recall = 82.455 %."
# [1] "F1-score = 84.198 %."

# Find performance on test data.
glm.4.prob <- predict(glm.4, newdata = tst.data, type='response')
glm.4.pred <- rep(0, length(glm.4.prob))
glm.4.pred[glm.4.prob > 0.5] <- 1
cm <- table(glm.4.pred, tst.data$Outcome)
diagnose_cm(cm)

# [1] "Error rate = 6.415 %."
# [1] "Precision = 86.26 %."
# [1] "Recall = 81.292 %."
# [1] "F1-score = 83.702 %."

model.5 <- 'Outcome ~ VisitNum + CumNoShow'
glm.5 <- glm(as.formula(model.5), data = trn.data, family = binomial)
# Find performance on training data.
glm.5.prob <- predict(glm.5, type = 'response')
glm.5.pred <- rep(0, length(glm.5.prob))
glm.5.pred[glm.5.prob > 0.5] <- 1
cm <- table(glm.5.pred, trn.data$Outcome)
diagnose_cm(cm)

# [1] "Error rate = 7.022 %."
# [1] "Precision = 87.888 %."
# [1] "Recall = 75.59 %."
# [1] "F1-score = 81.276 %."

# Find performance on test data.
glm.5.prob <- predict(glm.5, newdata = tst.data, type='response')
glm.5.pred <- rep(0, length(glm.5.prob))
glm.5.pred[glm.5.prob > 0.5] <- 1
cm <- table(glm.5.pred, tst.data$Outcome)
diagnose_cm(cm)
# [1] "Error rate = 7.268 %."
# [1] "Precision = 88.126 %."
# [1] "Recall = 74.118 %."
# [1] "F1-score = 80.517 %."