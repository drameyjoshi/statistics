set.seed(18111842)

data.0 <- read.csv('clean_data.csv')
cols.of.interest <-
  c(
    'Gender',
    'Age',
    'Neighbourhood',
    'Hipertension',
    'Diabetes',
    'SMS_received',
    'Gap.d',
    'No.show'
  )
data <- unique(data.0[, cols.of.interest])
rm('data.0')

data$No.show <- as.factor(data$No.show)
trn.samples <- sample(1:nrow(data), 0.7 * nrow(data))

trn.data <- data[trn.samples,]
tst.data <- data[-trn.samples,]

run_glm <- function(formula.str) {
  lr <-
    glm(as.formula(formula.str),
        data = trn.data,
        family = binomial)
  
  lr.probs <- predict(lr, type = 'response')
  lr.preds <- rep('0', nrow(trn.data))
  lr.preds[lr.probs > 0.5] <- "1"
  
  return (list("results" = lr, "predictions" = lr.preds))
}

print_cm_results <- function(cm) {
  errors <- cm[1, 2] + cm[2, 1]
  print(paste('Error rate = ', round(errors / sum(cm) * 100, 2), "%"))
}

analyze <- function(formula.str, print.summary = TRUE) {
  glm.res <- run_glm(formula.str = formula.str)
  if (print.summary) {
    print(summary(glm.res[["results"]]))
  }
  
  cm <- table(glm.res[["predictions"]], trn.data$No.show)
  print_cm_results(cm)
}

# First model.
analyze(
  "No.show ~ Age + Neighbourhood + Hipertension + Diabetes + SMS_received + Gap.d",
  print.summary = FALSE
)
# This is worse than a dumb model that randomly assigns '0' to every X.

# Second model.
analyze("No.show ~ Age + Hipertension + Diabetes + SMS_received + Gap.d",
        print.summary = FALSE)
# This too is a terrible model.

# Third model.
analyze("No.show ~ Age + Hipertension + Gap.d", print.summary = FALSE)
# Unacceptable error rate of 26.8%.
