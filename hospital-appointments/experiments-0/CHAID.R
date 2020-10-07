library('CHAID')
data <- read.csv('clean_data.csv')
cols.of.interest <- c('Gender', 'Age', 'Neighbourhood', 'Hipertension', 
                      'Diabetes', 'SMS_received', 'Gap.d', 'No.show')
data.1 <- unique(data[, cols.of.interest])
rm('data')

set.seed(18111842)
trn.samples <- sample(1:nrow(data.1), 0.7*nrow(data.1))

trn.data <- data.1[trn.samples, ]
tst.data <- data.1[-trn.samples, ]

ctrl <- chaid_control(minsplit = 200, minprob = 0.1)

trn.data$Gender <- as.factor(trn.data$Gender)
trn.data$Neighbourhood <- as.factor(trn.data$Neighbourhood)
trn.data$Hipertension <- as.factor(trn.data$Hipertension)
trn.data$Diabetes <- as.factor(trn.data$Diabetes)
trn.data$SMS_received <- as.factor(trn.data$SMS_received)
trn.data$No.show <- as.factor(trn.data$No.show)
chaid.res.1 <- chaid(No.show ~ Gender + Neighbourhood + Hipertension + Diabetes + SMS_received,
                     data = trn.data,
                     control = ctrl)
