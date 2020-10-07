library(glmnet)

data <- read.csv('model_data.csv')
set.seed(15081947)

all.Xs <- colnames(data)[c(2:4, 6, 8:10)]
n.Xs <- length(all.Xs)
# We do not consider singleton sets and the empty set.
x.list <- character(2 ^ n.Xs - n.Xs - 1)
cv.err.list <- numeric(2 ^ n.Xs - n.Xs - 1)

i <- 1
t0 <- Sys.time()
for (n in seq(2, n.Xs)) {
  sets <- combn(all.Xs, n)
  for (s in seq(1, dim(sets)[2])) {
    Xs <- sets[, s]
    cv.results <-
      cv.glmnet(
        as.matrix(data[, Xs]),
        data[, 'Outcome'],
        nfolds = 13,
        family = "binomial",
        alpha = 1
      )
    x.list[i] <- paste(Xs, collapse = ", ")
    cv.err.list[i] <- cv.results$cvm[length(cv.results$cvm)]
    print(paste("Subset:", i, x.list[i]))
    i <- i + 1
  }
}
t1 <- Sys.time()
print(t1 - t0)

least.cv.error <- min(cv.err.list)
Xs.least.cv.error <- x.list[which.min(cv.err.list)]
#
all.cv.results <-
  data.frame("x.list" = x.list[1:503], "cv.err" = cv.err.list[1:503])
all.cv.results$nvars <- sapply(strsplit(all.cv.results$x.list, ","), length)
all.cv.results <- all.cv.results[order(all.cv.results$cv.err),]
write.csv(all.cv.results, 'cv_glmnet_results.csv', row.names = FALSE)
