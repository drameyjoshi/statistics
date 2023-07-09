library(deSolve)

parameters <- c(a = 10, b = 2.5)
state <- c(x = 1, y = 1)
oscillator <- function(tau, state, parameters) {
  with (as.list(c(state, parameters)), {
    dx <- a - x - 4 * x * y / (1 + x ^ 2)
    dy <- b * (x - x * y / (1 + x ^ 2))
    
    list(c(dx, dy))
  })
  
}

times <- seq(0, 100, by = 0.01)

out <-
  ode(
    y = state,
    times = times,
    func = oscillator,
    parms = parameters
  )

ylim = c(0, max(out[, "x"], out[, "y"]))
plot(x=times, y=out[, "y"], type="l", col="blue", xlab="t", ylab="x or y", ylim=ylim)
lines(x=times, y=out[, "x"], col="green")
plot(out[, "x"], out[, "y"], pch=".", xlab="x", ylab="y")

# Choose parameters a and b
# Fix a and find various values of b.
a <- 10
critical_b <- 3*a/5 - 25/a
cat(paste("critical b", critical_b))
# We have already considered a value of b lower than critical b. Now consider a
# higher value, say b = 4.5
parameters <- c(a = 10, b = 4.5)

out <-
  ode(
    y = state,
    times = times,
    func = oscillator,
    parms = parameters
  )

ylim = c(0, max(out[, "x"], out[, "y"]))
plot(x=times, y=out[, "y"], type="l", col="blue", xlab="t", ylab="x or y", ylim=ylim)
lines(x=times, y=out[, "x"], col="green")
plot(out[, "x"], out[, "y"], pch=".", xlab="x", ylab="y")
