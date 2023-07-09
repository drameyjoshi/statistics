library(ggplot2)
library(reshape2)

N <- 2000
capture_prob <- seq(from = 0.01, to = 0.25, by = 0.02)
n_tries <- 100

simulate <- function(p, N, n_tries) {
  simulation <- function(p, N) {
    capture <- function(p, N) {
      n_repetitions <- p * N
      result <- sample.int(n = N,
                           size = n_repetitions,
                           replace = FALSE)
      result
    }
    
    first_capture <- capture(p, N)
    n <- length(first_capture)
    second_capture <- capture(p, N)
    K <- length(second_capture)
    recapture <- intersect(second_capture, first_capture)
    k <- length(recapture)
    
    lpe <- ifelse(k == 0, K * n, K * n / k)
    che <- floor((K + 1) * (n + 1) / (k + 1)) - 1
    
    lpe_error = (lpe - N) / N * 100
    che_error = (che - N) / N * 100
    
    list("lpe_error" = lpe_error, "che_error" = che_error)
  }
  
  lpe_error <- 0
  che_error <- 0
  
  for (i in 1:100) {
    rv <- simulation(p, N)
    lpe_error <- lpe_error + rv[[1]]
    che_error <- che_error + rv[[2]]
  }
  
  c(lpe_error/n_tries, che_error/n_tries)
}


che <- list()
lpe <- list()

for (p in capture_prob) {
  rv <- simulate(p, N, n_tries)
  lpe[[length(lpe) + 1]] <- rv[1]
  che[[length(che) + 1]] <- rv[2]
}

results <- data.frame("capture_prob"=capture_prob, 
                      "ch_error"=unlist(che), 
                      "lp_error"=unlist(lpe))

ggplot(data = melt(results, id.vars=c("capture_prob")), 
       aes(x=capture_prob, y=value, group=variable)) + 
  geom_line(aes(color=variable)) +
  xlab("Capture probability") +
  ylab("Error") +
  ggtitle("Error probability")
