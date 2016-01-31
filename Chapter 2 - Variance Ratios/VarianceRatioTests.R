library(PerformanceAnalytics)
library(xts)


observedParams <- function(returns) {
  sd.sum <- 0.0
  mu.sum <- 0.0
  for (i in 1:ncol(returns)) {
    r <- returns[ ,i]
    sd.sum <- sd.sum + StdDev.annualized(r)[[1]]
    mu.sum <- mu.sum + Return.annualized(r)[[1]]
  }
  return(list(sd.sum / ncol(returns),
              mu.sum / ncol(returns)))
}


getMu <- function(X) {
  X <- as.numeric(as.vector(X))
  n <- length(X)
  mu <- (X[n] - X[1])/n
  return(mu)
}


getSigmaA <- function(X) {
  
}


getSigmaB <- function(X) {
  
}


returnProcesses <- function(n, t = 252, x0 = 1.0, mu = 0.1062, rd.mu = 0.0, rd.sigma = 0.45, 
                            dt = 0.003968254, method = "discrete") {
  prices <- priceProcesses(n, t, x0, mu, rd.mu, rd.sigma, dt)
  return(Return.calculate(prices, method = method))
}


priceProcesses <- function(n, t = 252, x0 = 1.0, mu = 0.1062, rd.mu = 0.0, rd.sigma = 0.45, dt = 0.003968254) {
  processes <- xtsProcess(priceProcess(t, x0, mu, rd.mu, rd.sigma, dt), "S1")
  if (n > 1) for (i in 2:n)
    processes <- merge.xts(processes, xtsProcess(priceProcess(t, x0, mu, rd.mu, rd.sigma, dt), 
                                                 paste("S", i, sep = '')))
  return(processes)
}


xtsProcess <- function(ts, ts.name) {
  dates <- seq.Date(Sys.Date(), Sys.Date() + (length(ts) - 1), 1)
  ts.xts <- xts(ts, order.by = dates, col.names = ts.name)
  colnames(ts.xts) <- ts.name
  return(ts.xts)
}


priceProcess <- function(t = 252, x0 = 1.0, mu = 0.1062, rd.mu = 0.0, rd.sigma = 0.45, dt = 0.003968254) {
  return(exp(logPriceProcess(t, x0, mu, rd.mu, rd.sigma, dt)))
}


logPriceProcess <- function(t = 252, X0 = 1.0, mu = 0.1062, rd.mu = 0.0, rd.sigma = 0.45, dt = 0.003968254) {
  X <- rep(0.0, t)
  for (ti in 2:t)
    X[ti] <- logPriceStep(X[ti - 1], mu, randomDisturbance(rd.mu, rd.sigma), dt)
  return(X)
}


logPriceStep <- function(Xt1, mu, rd, dt = 0.003968254) {
  Xt2 <- Xt1 + (mu * dt) + (rd * sqrt(dt))
  return (Xt2)
}


randomDisturbance <- function(mu = 0.0, sigma = 0.45) {
  return (rnorm(1, mean = mu, sd = sigma))
}


