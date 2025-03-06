library('boot')

data <- read.csv('model_data.csv')
set.seed(15081947)
N <- nrow(data)

trn.samples <- sample.int(N, N * 0.7)
trn.data <- data[trn.samples, ]
tst.data <- data[-trn.samples, ]

model.2 <- 'Outcome ~ Age.Grp + SMS_received + Gap.Grp + VisitNum + CumNoShow'
glm.2 <- glm(as.formula(model.2),
             data = trn.data,
             family = binomial)

summary(glm.2)

cv.results <- cv.glm(data=data, glm.2, K=26)
