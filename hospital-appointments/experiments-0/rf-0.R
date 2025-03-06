library('randomForest')

data.0 <- read.csv('clean_data_v1.csv')
cols.of.interest <-
  c(
    'Age',
    'Neighbourhood',
    'Hipertension',
    'Diabetes',
    'SMS_received',
    'Gap.d',
    'visit_num',
    #'AppointmentMonth',
    'No.show'
  )

Xs <- cols.of.interest[1:length(cols.of.interest) - 1]
data <- unique(data.0[, cols.of.interest])
rm('data')

data.0$No.show <- as.factor(data.0$No.show)
trn.samples <- sample(1:nrow(data.0), 0.7 * nrow(data.0))

trn.data <- data.0[trn.samples,]
tst.data <- data.0[-trn.samples,]

formula <- 'No.show ~ Age + Neighbourhood + Hipertension + Diabetes + SMS_received + Gap.d + visit_num'
rf.1 <- randomForest(trn.data[, Xs], trn.data$No.show, importance = TRUE)
save(rf.1, file = 'rf.1.Rds')

rf.1.pred <- rf.1$predicted

rf.1 <- readRDS('rf.1.Rds')
