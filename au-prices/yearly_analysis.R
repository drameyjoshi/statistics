library(lattice)

yearly <- read.csv('yearly.csv')[, c('year', 'GBP')]
with(yearly,
     plot(x=year,
          y=GBP,
          xlab='Year',
          ylab='Price',
          main='Gold prices in London',
          type='l',
          lwd=2))
yearly$prev <- c(NA, yearly$GBP[1:nrow(yearly) - 1])
yearly$yret <- (yearly$GBP - yearly$prev)/yearly$prev
with(yearly,
     plot(x=year,
          y=yret,
          xlab='Year',
          ylab='Price',
          main='Yearly return on gold prices in London',
          type='l',
          lwd=2))
abline(h=0)

# Check for normality
qqnorm(yearly$yret, main = 'Yearly return')
qqline(yearly$yret)

shapiro.result <- shapiro.test(yearly$yret)
if (shapiro.result$p.value < 0.05) {
  cat('Shapiro-Wilk test: data is not normal.\n')
} else {
  cat('Shapiro-Wilk test: data is normal.\n')
}

yret.mean <- mean(yearly$yret, na.rm = TRUE)
yret.sd <- sd(yearly$yret, na.rm = TRUE)
ks.result <- ks.test(yearly$yret, 'pnorm', yret.mean, yret.sd)
if (ks.result$p.value < 0.05) {
  cat('Kolmogorov-Smirnov test: data is not normal.\n')
} else {
  cat('Kolmogorov-Smirnov test: data is normal.\n')
}

hist(x=yearly$yret, 
     probability = TRUE, 
     main = 'Distribution of yearly return', 
     xlab='Yearly return')
lines(density(yearly$yret, na.rm = TRUE), col=4, lwd=2)

# Assuming that the distribution of yearly returns is stationary, I will simulate
# 100 runs of 20 data points. Each run will have the returns for 20 years.
Nsims <- 100
X <- seq(1, 20)
for (n in 1:Nsims) {
  X <- cbind(X, rnorm(n=20, mean=yret.mean, sd=yret.sd))
}
X.df <- as.data.frame(X)
X.long = reshape(X.df,
                 direction='long',
                 varying=colnames(X.df)[-1],
                 v.names='sim.yret',
                 idvar='X',
                 timevar='Run',
                 times=1:Nsims)
sample.sims <- sample(1:Nsims, 5)
xyplot(data = X.long[X.long$Run %in% sample.sims, ],
       sim.yret ~ X,
       group = Run,
       type='l',
       xlab='Year',
       ylab='Yearly returns',
       main='Simulation of yearly returns')
