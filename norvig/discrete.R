library(MASS)

pmf <- function(X) Y <- {
  if (is.vector(X)) {
    df <- as.data.frame(table(X) / length(X))
  } else if (is.data.frame(X)) {
    stopifnot(ncol(X) == 2)
    df <- X
  }
  
  colnames(df) <- c('value', 'prob')
  total <- sum(df$prob)
  df$prob <- df$prob/total
  
  df
}

such_that <- function(X, f) vapply(X, f, logical(1))

P <- function(event, space) {
  if (is.function(event)) {
    subset <- such_that(space, event)
  } else {
    subset <- event
  }

  if(is.table(space)) {
    prob <- sum(space[subset, ]$prob)
  } else {
    prob <- fractions(length(space[subset])/length(space))
  }
  
  prob
}

# Testing
# such_that(seq(0, 12), function(n) n %% 2 == 0)
# dane_data <- data.frame(pairs = c('GG', 'GB', 'BG', 'BB'), nos = c(121801, 126840, 127123, 135138))
# P2 <- pmf(dane_data)
# P1 <- pmf(sample.int(10, 20, TRUE))
