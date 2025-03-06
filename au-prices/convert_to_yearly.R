data <- read.csv('prices.csv')
data$bus_date <- as.Date(data$Date, '%m/%d/%Y')
jan_data <- data[format(data$bus_date, '%m') == '01', ]
yearly_data <- aggregate(data = jan_data,
                         . ~ format(jan_data$bus_date, '%Y'),
                         FUN = head, 1)
colnames(yearly_data)[1] <- 'year'
write.csv(yearly_data, file='yearly.csv', row.names = FALSE)
