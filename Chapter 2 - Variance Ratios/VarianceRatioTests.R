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


testCalibration <- function(passes = 30) {
  mu.avg <- 0.0
  sd1.avg <- 0.0
  sd2.avg <- 0.0
  
  for (i in 1:passes) {
    X <- log(priceProcess(t = (252 * 5)))
    mu.avg <- mu.avg + calibrateMu(X)
    sd1.avg <- sd1.avg + calibrateSigmaA(X)
    sd2.avg <- sd2.avg + calibrateSigmaB(X)
  }
  
  print(paste("mu:", mu.avg / passes))
  print(paste("sd:", sd1.avg / passes))
  print(paste("sd:", sd2.avg / passes))
}


calibrateMu <- function(X) {
  X <- as.numeric(as.vector(X))
  n <- length(X)
  mu.est <- (X[n] - X[1])/n
  return(mu.est)
}


calibrateSigmaA <- function(X) {
  X <- as.numeric(as.vector(X))
  mu.est <- getMu(X)
  
  sd.est <- 0.0
  n <- length(X)
  for (t in 2:n)
    sd.est <- sd.est + (X[t] - X[t - 1] - mu.est)^2
  return(sd.est / n)
}


calibrateSigmaB <- function(X, mu) {
  X <- as.numeric(as.vector(X))
  mu.est <- getMu(X)
  
  sd.est <- 0.0
  n <- floor(length(X)/2)
  for (t in 2:n)
    sd.est <- sd.est + (X[t * 2] - X[(t * 2) - 2] - (2 * mu.est))^2
  return(sd.est / (2 * n))
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


