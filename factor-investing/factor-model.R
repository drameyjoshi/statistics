# Read factor returns.
factor.data <- read.csv("data/factor_returns.csv")

# Read portfolio returns
portfolio.data <- read.csv("portfolio-daily-returns.csv")

X <- merge(factor.data[, c("Date", "Ret_rf")], portfolio.data)

calc_excess_retn <- function(r){
  r[3:12] <- r[3:12] - r[2]
  c(r[1], r[3:12])
}

portf.excess.retn <- t(apply(X, 1, calc_excess_retn))

# Get the factor returns with excess returns.
X <- merge(portf.excess.retn, factor.data)
portf.excess.retn <- X[, c(1:14)]

# Run time-series regression
run_ts_regression <- function(n) {
  model.formula <- paste(paste(sep = ".", "Portfolio", n), "~ SMB + HML + WML")
  ts.reg <- lm(as.formula(model.formula),
               data = portf.excess.retn)
  ts.reg$coefficients[c("SMB", "HML", "WML")]
}

X <- lapply(1:10, run_ts_regression)
exposures <- do.call(rbind, X)
# exposures <- as.data.frame(exposures)
colnames(exposures) <- paste(sep = ".", "exposure", c("SMB", "HML", "WML"))

# Run cross-sectional regression
run_cs_regression <-function(t) {
  r <- unlist(portf.excess.retn[t, c(2:11)]) # Portfolio returns for day 't'.
  cs.reg <- lm(r ~ exposures)
  coeff <- cs.reg$coefficients[2:4]
}

X <- lapply(1:nrow(portf.excess.retn), run_cs_regression)
loadings <- do.call(rbind, X)
colnames(loadings) <- paste(sep = ".", "loading", c("SMB", "HML", "WML"))

loading.means <- colMeans(loadings)
loading.sd <- apply(loadings, 2, sd)
loading.t.stat <- loading.means/(loading.sd/sqrt(nrow(loadings)))

print("Mean loadings for SMB, HML, WML")
print(loading.means)

print("t-statistics for SML, HML, WML")
print(loading.t.stat)
