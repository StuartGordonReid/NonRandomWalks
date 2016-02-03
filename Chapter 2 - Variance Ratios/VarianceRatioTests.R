library(PerformanceAnalytics)
library(xts)


#' @title Estimate the value of mu.
#' @description Given a log price process, estimate the value of mu. Mu is the daily component of the returns which is
#' attributable to upward, or downward, drift. This estimate can be annualized by multiplying it by 252.
#' 
#' @param X vector :: A log price process.
#' @param annualize logical :: Annualize the parameter estimate. True or False.
#' 
calibrateMu <- function(X, annualize = TRUE) {
  X <- as.numeric(as.vector(X))
  n <- length(X)
  mu.est <- (X[n] - X[1])/n
  
  if (!annualize) return(mu.est)
  else return(mu.est * 252)
}


#' @title Estimate the value of Sigma.
#' @description Given a log price process, estimate the value of Sigma. Sigma represents the standard deviation of the 
#' random disturbance component of daily returns. This estimate can be annualized by taking the square root of the daily
#' estimate multiplied by 252. This calibration also receives an argument, q, which controls how often prices are
#' sampled from the log price process for use in the esimation. For example, when q = 1 every price is used but when
#' q = 4 then every fourth price is used to estimate the value of the sigma.
#' 
#' @param X vector :: A log price process.
#' @param q int :: The sampling interval for the estimator.
#' @param annualize logical :: Annualize the parameter estimate. True or False.
#' 
calibrateSigma <- function(X, q = 1, annualize = TRUE, unbiased = TRUE) {
  mu.est <- calibrateMu(X, annualize = FALSE)
  X <- as.numeric(as.vector(X))
  
  sd.est <- 0.0
  n <- floor(length(X)/q)

  for (t in 2:n)
    sd.est <- sd.est + (X[t * q] - X[(t * q) - q] - (q * mu.est))^2
  
  if (!unbiased)
    sd.est <- sd.est / (q * n)
  else 
    sd.est <- sd.est / (q * n - 1)
  
  if (!annualize) return(sd.est)
  else return(sqrt((sd.est * 252)))
}


#' @title A more efficient method for estimating the value of Sigma.
#' @description Given a log price process, estimate the value of Sigma using overlapping intervals. Sigma represents the
#' standard deviation of the random disturbance component of daily returns. This estimate can be annualized by taking
#' the square root of the daily estimate multiplied by 252 days. Like the calibrateSigma function, this method receives
#' the argument q which specifies the sampling interval to be used when estimating sigma. The difference between the 
#' calibrateSigma method and the calibrateSigmaOverlapping method is that the first uses discrete non-overlapping return
#' sub-sequences to estimate sigma, whereas the second method uses overlapping sub sequences. This estimator converges 
#' faster than the calibrateSigma method and has a higher power. In other words, it is more robust.
#' 
#' @param X vector :: A log price process.
#' @param q int :: The sampling interval for the estimator.
#' @param annualize logical :: Annualize the parameter estimate. True or False.
#' 
calibrateSigmaOverlapping <- function(X, q = 1, annualize = TRUE, unbiased = TRUE) {
  mu.est <- calibrateMu(X, annualize = FALSE)
  X <- as.numeric(as.vector(X))
  
  sd.est <- 0.0
  n <- floor(length(X)/q)
  
  for (t in (q + 1):(n * q))
    sd.est <- sd.est + (X[t] - X[t - q] - (q * mu.est))^2
  
  if (!unbiased)
    sd.est <- sd.est / (n * (q^2))
  else
    sd.est <- sd.est / ((q * ((n * q) - q + 1)) * (1 - (q / (n * q))))
  
  if (!annualize) return(sd.est)
  else return(sqrt((sd.est * 252)))
}


#' @title Interface to the calibrateMu and calibrateSigmaOverlapping functions.
#' @description This method returns the estimate parameter values for mu and sigma. These are computed using the 
#' calibrateMu and calibrateSigmaOverlapping functions. 
#' 
#' @param X vector :: A log price process.
#' @param q int :: The sampling interval for the estimator.
#' @param annualize logical :: Annualize the parameter estimate. True or False.
#' 
calibrate <- function(X, q, annualize = TRUE) {
  mu.est <- calibrateMu(X)
  sd.est <- calibrateSigmaOverlapping(X, q)
  return(list(mu.est, sd.est))
}


jDifferences <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  sigmaA <- calibrateSigma(X, q = 1, annualize, unbiased)
  sigmaB <- calibrateSigma(X, q = q, annualize, unbiased)
  # This statistic should converge to zero.
  return(sigmaA - sigmaB)
}


jRatio <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  sigmaA <- calibrateSigma(X, q = 1, annualize, unbiased)
  sigmaB <- calibrateSigma(X, q = q, annualize, unbiased)
  # This statistic should converge to zero.
  return((sigmaA / sigmaB) - 1)
}


mDifferences <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  sigmaA <- calibrateSigma(X, q = 1, annualize, unbiased)
  sigmaB <- calibrateSigmaOverlapping(X, q = q, annualize, unbiased)
  # This statistic should converge to zero.
  return(sigmaA - sigmaB)
}


mRatio <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  sigmaA <- calibrateSigma(X, q = 1, annualize, unbiased)
  sigmaB <- calibrateSigmaOverlapping(X, q = q, annualize, unbiased)
  # This statistic should converge to zero.
  return((sigmaA / sigmaB) - 1)
}


calibrateTheta <- function(X, q, j) {
  mu.est <- calibrateMu(X, FALSE)
  n <- floor(length(X)/q)
  
  numerator <- 0.0
  # Double check the 2:...
  for (k in (j + 2):(n * q)) {
    t1 <- (X[k] - X[k - 1] - mu.est)^2
    t2 <- (X[k - j] - X[k - j - 1] - mu.est)^2
    numerator <- numerator + (t1 * t2)
  }
  
  denominator <- 0.0
  for (k in 2:(n * q))
    denominator <- denominator + (X[k] - X[k - 1] - mu.est)^2
  
  thetaJ <- (n * q * numerator) / (denominator^2)
  return(thetaJ)
}


asymptoticVariance <- function(X, q) {
  avar <- 0.0
  for (j in 1:(q - 1)) {
    theta <- calibrateTheta(X, q, j)
    avar <- avar + (2 * (q - j) / q) ^ 2 * theta
  }
  return(avar)
}


zTest <- function(X, q) {
  n <- floor(length(X)/q)
  z <- sqrt(n * q) * mRatio(X, q)
  z <- z / sqrt(asymptoticVariance(X, q))
  return(z)
}


zScatter <- function(samples = 2500) {
  x <- c()
  for (i in 2:samples) {
    # Generate a log price process.
    X <- logPriceProcess(t = (252 * 10), mu = runif(1, min = -0.15, max = 0.15),
                         rd.sigma = runif(1, min = 0.05, max = 0.75))
    
    for (q in c(2, 4, 8, 16)) {
      z <- zTest(X, q)
      x <- c(x, z)
    }
    
    print(paste("Completed", i, "out of", samples, "samples"))
  }
  
  model <- hist(x, 100)
  expected <- hist(rnorm(samples * 4, 0.0), 100)
  plot(model, col = rgb(0,0,1,1/4), xlim = c(-5, 5))
  plot(expected, col = rgb(1,0,0,1/4), xlim = c(-5, 5), add=T)
}


#' @title Generate a random disturbance.
#' @description This method returns a normally distributed random disturbance for use in a log price process. The mean
#' disturbance should always be equal to zero and the standard deviation of the disturbance can be estimated from a
#' historical log price process using the calibrateSigmaQ function.
#' 
#' @param mu double :: The expected value of the disturbance. This should be zero.
#' @param sigma double :: The standard deviation of the disturbance.
#' 
randomDisturbance <- function(mu = 0.0, sigma = 0.45) {
  return (rnorm(1, mean = mu, sd = sigma))
}


#' @title Step forward one unit of time in the log price process.
#' @description This step function is used to move from state t / t-1 in the log price process to t+1 / t. This involves
#' adding a static drift component to the previous price and adding a random disturbance sampled from some distribution.
#' 
#' @param Xt1 double :: The current price in the log price process.
#' @param mu double :: The drift component of returns.
#' @param rd double :: A random disturbance in the force.
#' @param dt double :: This variable controls time. It is set to 1/252 by default.
#' @return Xt2 double :: The next price in the log price process.
#' 
logPriceStep <- function(Xt1, mu, rd, dt = 0.003968254) {
  Xt2 <- Xt1 + (mu * dt) + (rd * sqrt(dt))
  return (Xt2)
}


#' @title Generate a log price process.
#' @description This method is used to generate a log price process which simulates the log price of an asset starting
#' with the value X0. The process is controlled by a number of parameters including mu, the average daily return, rd.mu
#' the average daily random disturbance, rd.sigma, the volatility of the random disturbance, and dt, time. The method
#' works by iteratively sampling a new log price using the logPriceStep function.
#' 
#' @param t int :: The number of days worth of prices to simulate.
#' @param X0 int :: The starting price of the asset. Set to 1.0.
#' @param mu double :: The drift component of returns.
#' @param rd.mu double :: The average daily disturbance to be added.
#' @param rd.sigma double :: The volatility of daily disturbances to be added.
#' @param dt double :: Time. This variable is set to 1/252 by default.
#' @return X vector :: A simulated log price process.
#' 
logPriceProcess <- function(t = 252, X0 = 1.0, mu = 0.1062, rd.mu = 0.0, rd.sigma = 0.45, dt = 0.003968254) {
  X <- rep(X0, t)
  for (ti in 2:t)
    X[ti] <- logPriceStep(X[ti - 1], mu, randomDisturbance(rd.mu, rd.sigma), dt)
  return(X)
}


#' @title Generate a discrete price process.
#' @description This method exponentiates a log price process and returns it.
#' @inheritParams logPriceProcess
#' 
priceProcess <- function(t = 252, X0 = 1.0, mu = 0.1062, rd.mu = 0.0, rd.sigma = 0.45, dt = 0.003968254) {
  return(exp(logPriceProcess(t, log(X0), mu, rd.mu, rd.sigma, dt)))
}


#' @title Generate an XTS object containing multiple discrete price processes.
#' @description This method constructs an xts object which contains multiple discrete price processes as simulated using
#' the priceProcess function. These price processes are named S1, S2, ... , Sn.
#' 
#' @param n int :: The number of price processes to simulate.
#' @inheritParams logPriceProcess
#' 
priceProcesses <- function(n, t = 252, X0 = 1.0, mu = 0.1062, rd.mu = 0.0, rd.sigma = 0.45, dt = 0.003968254) {
  processes <- xtsProcess(priceProcess(t, X0, mu, rd.mu, rd.sigma, dt), "S1")
  if (n > 1) for (i in 2:n)
    processes <- merge.xts(processes, xtsProcess(priceProcess(t, X0, mu, rd.mu, rd.sigma, dt), 
                                                 paste("S", i, sep = '')))
  return(processes)
}


#' @title Generate an XTS object containing daily returns for multiple discrete price processes.
#' @description This method constructs an xts object which contains multiple daily return processes as simulated using
#' the priceProcess function. These price processes are named S1, S2, ... , Sn.
#' 
returnProcesses <- function(n, t = 252, X0 = 1.0, mu = 0.1062, rd.mu = 0.0, rd.sigma = 0.45, 
                            dt = 0.003968254, method = "discrete") {
  prices <- priceProcesses(n, t, X0, mu, rd.mu, rd.sigma, dt)
  return(Return.calculate(prices, method = method))
}


#' @title Convert a time series into an xts time series starting from today and going forward into the future.
#' @description This method just converts a time series (a vector of prices or returns) into an xts object.
#' 
#' @param ts vector :: The vector containing the time series values.
#' @param ts.name character :: The name of the time series.
#' 
xtsProcess <- function(ts, ts.name) {
  dates <- seq.Date(Sys.Date(), Sys.Date() + (length(ts) - 1), 1)
  ts.xts <- xts(ts, order.by = dates, col.names = ts.name)
  colnames(ts.xts) <- ts.name
  return(ts.xts)
}


#' @title Check that simulated paths have the expected statistics.
#' @description This method checks that the annualized returns and standard deviations of the simulated return series
#' are in line with the expected annualized return and standard deviations which were supplied to the model as inputs.
#' 
#' @param returns xts :: An XTS object containing multiple return sequences.
#' 
observedParams <- function(returns) {
  # Standard deviation and (annualized) expected return, mu. 
  sd.sum <- 0.0
  mu.sum <- 0.0
  for (i in 1:ncol(returns)) {
    r <- returns[ ,i]
    # Accumulate the sums.
    sd.sum <- sd.sum + StdDev.annualized(r)[[1]]
    mu.sum <- mu.sum + Return.annualized(r)[[1]]
  }
  # Return the averages.
  return(list(sd.sum / ncol(returns),
              mu.sum / ncol(returns)))
}


#' @title This method checks that the model calibration is working.
#' @description This method is used to testing that the parameter values estimated from simulated log price processes 
#' match the parameter values specified in the stochastic model that was used to simulate the data in the first place.
#' 
#' @param tries int :: The number of independent tests to run.
#' @param passes int :: The number of passes per test to run.
#' @param tau double :: A parameter to control the sensitivity of the test.
#' 
testCalibration <- function(tries = 30, passes = 300, tau = 0.025) {
  for (t in 1:tries) {
    # Actual variable specifications.
    mu <- runif(1, min = -0.25, 0.25)
    sigma <- runif(1, min = 0.05, 0.75)
    
    # Observed variable estimates.
    mu.est <- 0.0
    sigma1.est <- 0.0
    sigma2.est <- 0.0
    sigmaq.est <- 0.0
    sigmaq.overlapping.est <- 0.0
    
    for (i in 1:passes) {
      # Generate a log pricess process with the 
      X <- log(priceProcess(t = (252 * 5), mu = mu, rd.sigma = sigma))
      
      # Accumulate the observation variables.
      mu.est <- mu.est + calibrateMu(X)
      sigma1.est <- sigma1.est + calibrateSigma(X, 1)
      sigma2.est <- sigma2.est + calibrateSigma(X, 2)
      sigmaq.est <- sigmaq.est + calibrateSigma(X, 4)
      sigmaq.overlapping.est <- sigmaq.overlapping.est + calibrateSigmaOverlapping(X, 4)
    }
    
    # Calculate the estimates.
    mu.est <- mu.est / passes
    sigma1.est <- sigma1.est / passes
    sigma2.est <- sigma2.est / passes
    sigmaq.est <- sigmaq.est / passes
    sigmaq.overlapping.est <- sigmaq.overlapping.est / passes
    
    # Check that the differences are negligable.
    if (!abs(mu.est - mu) < tau) calibratorWarning("mu", mu, mu.est, tau)
    else if (!abs(sigma1.est - sigma) < tau) calibratorWarning("sigma", sigma, sigma1.est, tau)
    else if (!abs(sigma2.est - sigma) < tau) calibratorWarning("sigma", sigma, sigma2.est, tau)
    else if (!abs(sigmaq.est - sigma) < tau) calibratorWarning("sigma", sigma, sigmaq.est, tau)
    else if (!abs(sigmaq.overlapping.est - sigma) < tau) calibratorWarning("sigma", sigma, sigmaq.overlapping.est, tau)
    else print(paste(t, "Yay, no warnings! mu:", mu, "><", mu.est, "sigma:", sigma, "><", sigmaq.overlapping.est))
  }
}


#' @title Generate a useful warning saying that the calibrator has failed.
#'
calibratorWarning <- function(param, true, estimate, tau) {
  warning(paste("Estimate for", param, ",", estimate, "is further away from the true value for", 
                param, ",", true, "by more than", tau))
}


example <- function(y, k = 5) {
  library(vrtest)
  nob <- length(y)
  r <- log(y[2:nob])-log(y[1:(nob-1)])
  
  print(Lo.Mac(r, k)$Stats[2])
  print(mRatio(y, k))
  print(mDifferences(y, k))
  print(jRatio(y, k))
  print(jDifferences(y, k))
}


