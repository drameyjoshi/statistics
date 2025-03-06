library('CHAID')
data <- read.csv('all_data.csv')
categorical_vars <- colnames(data)[c(3, 7, 8, 9:14)]

data$Gender <- as.factor(data$Gender)
data$Neighbourhood <- as.factor(data$Neighbourhood)
data$Scholarship <- as.factor(data$Scholarship)
data$Hipertension <- as.factor(data$Hipertension)
data$Diabetes <- as.factor(data$Diabetes)
data$Alcoholism <- as.factor(data$Alcoholism)
data$Handcap <- as.factor(data$Handcap)
data$SMS_received <- as.factor(data$SMS_received)
data$No.show <- as.factor(data$No.show)

ctrl <- chaid_control(minsplit = 200, minprob = 0.1)

chaid.res.all <- chaid(No.show ~ Gender + Neighbourhood + Scholarship + Hipertension + Diabetes + Alcoholism + Handcap + SMS_received,
                     data = data,
                     control = ctrl)

print(chaid.res.all)
