library('CHAID')
data <- read.csv('clean_data_v1.csv')
categorical_vars <- colnames(data)[c(6, 7, 8, 9, 10, 13, 15, 17, 18)]

data$Age <- as.factor(data$Age)
data$Neighbourhood <- as.factor(data$Neighbourhood)
data$Scholarship <- as.factor(data$Scholarship)
data$Hipertension <- as.factor(data$Hipertension)
data$Diabetes <- as.factor(data$Diabetes)
data$SMS_received <- as.factor(data$SMS_received)
data$No.show <- as.factor(data$No.show)
data$Gap.d <- as.factor(data$Gap.d)
data$visit_num <- as.factor(data$visit_num)

ctrl <- chaid_control(minsplit = 200, minprob = 0.1)

chaid.res.all <- 
  chaid(No.show ~ Age + Neighbourhood + Scholarship + Hipertension + Diabetes + SMS_received + Gap.d + visit_num,
        data = data,
        control = ctrl)

print(chaid.res.all)
