library(rvest)

set.seed(12111842)

url <- "https://en.wikipedia.org/wiki/BSE_SENSEX"
sensex <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table() %>% as.data.frame()

constituents <- sensex[, "Exchange.ticker"]

# Build 10 portfolios of 3 stocks at random.
random.portfolios <-
  matrix(data = constituents[sample(1:nrow(sensex), nrow(sensex), replace = FALSE)],
         nrow = 10,
         ncol = 3)

# Get daily returns of all stocks in the SENSEX.
get_daily_returns <- function(n) {
  filename <- paste(sep = "/", "data", paste(sep = "", n, ".csv"))
  X <- read.csv(filename)
  
  daily.returns <- diff(X$Close.Price) / X$Close.Price[-nrow(X)]
  
  X <- data.frame("C" = daily.returns)
  colnames(X) <- paste(sep = ".", n, "dly.ret")
  X
}

results <- lapply(constituents, get_daily_returns)
all.daily.returns <- do.call(cbind, results)

get_portfolio_returns <- function(p) {
  rowMeans(all.daily.returns[, paste(sep = ".", p, "dly.ret")])
}

portfolio.daily.returns <-
  apply(random.portfolios, 1, get_portfolio_returns)

colnames(portfolio.daily.returns) <-
  paste(sep = ".", "Portfolio", 1:10)

# Attach dates to the portfolio returns. Use one of the CSV files.
X <-
  read.csv(paste(sep = "/", "data", paste(sep = "", constituents[1], ".csv")))
bus.dates <-
  format(as.Date(X[1:(nrow(X) - 1),]$Date, "%d-%B-%Y"), "%Y%m%d")
portfolio.daily.returns <- as.data.frame(portfolio.daily.returns)
portfolio.daily.returns$Date <- bus.dates
portfolio.daily.returns <- portfolio.daily.returns[, c(11, 1:10)]

# Let's save these for future reference.
write.csv(portfolio.daily.returns,
          "portfolio-daily-returns.csv",
          row.names = FALSE)
