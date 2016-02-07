# Author : Stuart Gordon Reid
# Website URL : www.TuringFinance.com
# Public Article URL : www.TuringFinance.com/variance-ratios-a-non-random-walk-down-wall-street
# 
# Description : This file contains an implementation of a statistical test for the random walk hypothesis defined by Lo
# and MacKinlay in their first paper: "Stock Markets Do Not Follow Random Walks: Evidence from a Simple Specification 
# Test. This test, called a variance ratio test, exploits the fact that under a Geometric Brownian Motion model with 
# Stochastic Volatility varianceestimates are linear in the sampling interval.


# Model Specification: Brownian Motion with Drift and Stochastic Volatility (RW2).
# ----------------------------------------------------------------------------------------------------------------------


# Load the required packages.
library(PerformanceAnalytics)
library(xts)


#' @title Sample a random disturbance from either a normal distribution with a 
#' constant standard deviation (Geometric Brownian Motion model) or from a 
#' distribution with a stochastic standard deviation (Stochastic Volatility GBM)
#' 
#' @description Given a long run random disturbance mean, mu, and a standard 
#' deviation, sigma, sample a random disturbance from either a standard normal 
#' distribution with mean mu and standard deviation sigma, or sample a random 
#' disturbance from a normal distribution with mean mu and a stochastic standard 
#' deviation sampled from a normal distribution centered around sigma with a 
#' standard deviation equal to half of sigma. This is maxed out at 0.01 because
#' a negative standard deviation is incoherent. 
#' 
#' @details When the stochastic volatility option is enabled, the resulting 
#' distribution of means for the random disturbance has much fatter tails. This 
#' can be illustrated with the compareRandomDisturbances function.
#' 
#' @param mu double :: The expected value of the disturbance (should be zero).
#' @param sigma double :: The standard deviation of the disturbance.
#' 
randomDisturbance <- function(mu = 0.0, sigma = 0.45, 
                              stochastic.volatility = TRUE) {
  if (!stochastic.volatility) {
    return (rnorm(1, mean = mu, sd = sigma))
  } else {
    # sigma.stochastic <-  rlnorm(1, meanlog = log(sigma), sdlog = sigma)
    sigma.stochastic <- max(rnorm(1, mean = sigma, sd = sigma/2), 0.0001)
    return (rnorm(1, mean = mu, sd = sigma.stochastic))
  }
}


#' @title Step forward one unit of time in the log price process.
#' @description This step function is used to move from state t / t-1 in the log 
#' price process to t+1 / t. This involves adding a static drift component to the 
#' previous price and adding a random disturbance sampled from some distribution.
#' 
#' @param Xt1 double :: The current price in the log price process.
#' @param mu double :: The drift component of returns.
#' @param rd double :: A random disturbance in the force.
#' @param dt double :: This variable controls time. It is set to 1/252 by default
#' @return Xt2 double :: The next price in the log price process.
#' 
logPriceStep <- function(Xt1, mu, rd, dt = 0.003968254) {
  Xt2 <- Xt1 + (mu * dt) + (rd * sqrt(dt))
  return (Xt2)
}


#' @title Generate a log price process.
#' @description This method is used to generate a log price process which 
#' simulates the log price of an asset starting with the value X0. The process 
#' is controlled by a number of parameters including mu, the average daily 
#' return, rd.mu the average daily random disturbance, rd.sigma, the volatility 
#' of the random disturbance, and dt, time. The method works by iteratively 
#' sampling a new log price using the logPriceStep function.
#' 
#' @param t int :: The number of days worth of prices to simulate.
#' @param X0 int :: The starting price of the asset. Set to 1.0.
#' @param mu double :: The drift component of returns.
#' @param rd.mu double :: The average daily disturbance to be added.
#' @param rd.sigma double :: The volatility of daily disturbances to be added.
#' @param dt double :: Time. This variable is set to 1/252 by default.
#' @return X vector :: A simulated log price process.
#' 
logPriceProcess <- function(t = 252, X0 = 1.0, mu = 0.1062, 
                            rd.mu = 0.0, rd.sigma = 0.45, 
                            dt = 0.003968254, stochastic.volatility = TRUE) {
  X <- rep(X0, t)
  for (ti in 2:t) {
    rd <- randomDisturbance(rd.mu, rd.sigma, stochastic.volatility)
    X[ti] <- logPriceStep(X[ti - 1], mu, rd, dt)
  }
  
  return(X)
}


#' @title Generate a discrete price process.
#' @description This method exponentiates a log price process and returns it.
#' @inheritParams logPriceProcess
#' 
priceProcess <- function(t = 252, X0 = 1.0, mu = 0.1062, 
                         rd.mu = 0.0, rd.sigma = 0.45, 
                         dt = 0.003968254, stochastic.volatility = TRUE) {
  return(exp(logPriceProcess(t, log(X0), mu, 
                             rd.mu, rd.sigma, 
                             dt, stochastic.volatility)))
}


#' @title Generate an XTS object containing multiple discrete price processes.
#' @description This method constructs an xts object which contains multiple 
#' discrete price processes as simulated using the priceProcess function. These 
#' price processes are named S1, S2, ... , Sn.
#' 
#' @param n int :: The number of price processes to simulate.
#' @inheritParams logPriceProcess
#' 
priceProcesses <- function(n, t = 252, X0 = 1.0, mu = 0.1062, 
                           rd.mu = 0.0, rd.sigma = 0.45, 
                           dt = 0.003968254, stochastic.volatility = TRUE) {
  processes <- xtsProcess(priceProcess(t, X0, mu, 
                                       rd.mu, rd.sigma, 
                                       dt, stochastic.volatility), "S1")
  if (n > 1) for (i in 2:n)
    processes <- merge.xts(processes, 
                           xtsProcess(priceProcess(t, X0, mu, rd.mu, rd.sigma, 
                                                   dt, stochastic.volatility), 
                                      paste("S", i, sep = '')))
  return(processes)
}


#' @title Generate an XTS object containing daily returns for multiple discrete 
#' price processes.
#' 
#' @description This method constructs an xts object which contains multiple 
#' daily return processes as simulated using the priceProcess function. These 
#' price processes are named S1, S2, ... , Sn.
#' 
returnProcesses <- function(n, t = 252, X0 = 1.0, mu = 0.1062, 
                            rd.mu = 0.0, rd.sigma = 0.45, dt = 0.003968254, 
                            stochastic.volatility = TRUE, method = "discrete") {
  prices <- priceProcesses(n, t, X0, mu, rd.mu, rd.sigma, dt, stochastic.volatility)
  return(Return.calculate(prices, method = method))
}


#' @title Convert a time series into an xts time series starting from today and 
#' going forward into the future.
#' 
#' @description This method just converts a time series (a vector of prices or 
#' returns) into an xts object.
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


# # Plot fifteen asset price paths without stochastic volatility.
# charts.PerformanceSummary(returnProcesses(15, t = (252*5), 
#                                           stochastic.volatility = FALSE), 
#                           colorset = seq(1,15))
# 
# # Plot fifteen asset price paths with stochastic volatility.
# charts.PerformanceSummary(returnProcesses(15, t = (252*5), 
#                                           stochastic.volatility = TRUE), 
#                           colorset = seq(1,15))


# Code for comparing the homoskedastic and heteroskedastic models.
# ------------------------------------------------------------------------------


#' @title Generate two density plots to show constant vs. stochastic volatility 
#' random disturbances.
#' 
#' @description This function generates two sequences of random disturbances 
#' sampled from the randomDisturbance function which and without stochastic 
#' volatility. As can be seen in the resulting plots, the distribution of 
#' disturbances generated by the randomDisturbance function with stochastic 
#' volatility has much fatter tails. This observation is more consistent with .
#' the actual returns generated by markets and it thus preferred.
#' 
#' @param sd double :: The standard deviation to feed into randomDisturbance.
#' @param samples int :: The number of independent samples to draw from random 
#' Disturbance.
#' 
compareRandomDisturbanceDistributions <- function(sd = 0.5, samples = 25000) {
  # Generate two independent samples.
  constantVol <- c()
  stochasticVol <- c()
  for (t in 1:samples) {
    constantVol <- c(constantVol, 
                     randomDisturbance(mu = 0.0, sigma = sd,
                                       stochastic.volatility = FALSE))
    stochasticVol <- c(stochasticVol, randomDisturbance(mu = 0.0, sigma = sd, 
                                                        stochastic.volatility = TRUE))
  }
  
  # Plot the distribution of random disturbances generated with constant volatility.
  constantVol.density <- density(constantVol)
  plot(constantVol.density, col = rgb(0,0,1,1/4), xlim = c(-3, 3), ylim = c(0.0, 1.2),
       main = "Comparison of Homoskedastic vs Heteroskedastic Increments")
  polygon(constantVol.density, col = rgb(0,0,1,1/4), alpha = 0.3)
  
  # Plot the distribution of random disturbances generated with stochastic volatility.
  stochasticVol.density <- density(stochasticVol)
  lines(stochasticVol.density, col = rgb(1,0,0,1/4), xlim = c(-4, 4), ylim = c(0.0, 1.2))
  polygon(stochasticVol.density, col = rgb(1,0,0,1/4), alpha = 0.3)
}


#' @title Generate two sample paths to show constant vs. stochastic volatility 
#' random disturbances.
#' 
#' @description This function generates two example sequences of length 252 
#' showing the difference between random disturbances sampled from a normal 
#' distribution against those sampled from a distribution with stochastich volatility
#' 
#' @param sd double :: The standard deviation to feed into randomDisturbance.
#' @param samples int :: The number of independent samples to draw from random Disturbance.
#' 
compareRandomDisturbancePaths <- function(sd = 0.5, days = (252)) {
  # Generate two independent samples.
  constantVol <- c()
  stochasticVol <- c()
  for (t in 1:days) {
    constantVol <- c(constantVol, randomDisturbance(mu = 0.0, sigma = sd, 
                                                    stochastic.volatility = FALSE))
    stochasticVol <- c(stochasticVol, randomDisturbance(mu = 0.0, sigma = sd, 
                                                        stochastic.volatility = TRUE))
  }
  
  print(sd(constantVol))
  print(sd(stochasticVol))
  
  # Plot the two paths for comparison.
  min.min <- min(min(constantVol), min(stochasticVol))
  max.max <- max(max(stochasticVol), max(stochasticVol))
  plot.ts(constantVol, col = rgb(0,0,1,2/4), ylim = c(min.min, max.max), 
          ylab = "Random Disturbance", xlab = "Time", 
          main = "Comparison of Homoskedastic vs Heteroskedastic Increments")
  lines(stochasticVol, col = rgb(1,0,0,2/4))
} 


# Calibrator Funtions for Mu, Sigma, Theta, and Asymptotic Variance.
# ------------------------------------------------------------------------------


#' @title Estimator for the value of mu. Mu is the drift component in the 
#' Geometric Brownian Motion model.
#' 
#' @description Given a log price process, this function estimates the value of 
#' mu. Mu is the daily component of the returns which is attributable to upward, 
#' or downward, drift. This estimate can be annualized.
#' 
#' @param X vector :: A log price process.
#' @param annualize logical :: Annualize the parameter estimate.
#' @return mu.est double :: The estimated value of mu.
#' 
calibrateMu <- function(X, annualize = TRUE) {
  # Ensure the format of X is appropriate.
  X <- as.numeric(as.vector(X))
  
  # Estimate the value of mu.
  n <- length(X)
  mu.est <- (X[n] - X[1])/n
  
  if (!annualize) return(mu.est)
  else return(mu.est * 252)
}


calibrateMuTest <- function(real.mu = 0.15, samples = 30, 
                            stochastic.volatility = TRUE) {
  # Start off the plot with the actual values (scatter)
  plot(rep(real.mu, 50), ylab = "Mu Estimate", xlab = "Length of X (years)",
       main = "Illustration of the paramator estimator for Mu",
       ylim = c(real.mu - (2 * real.mu), real.mu + (2 * real.mu)))
  
  # The add samples many estimates of mu from X's of length t.
  for (i in 1:samples) {
    mu.estimates <- c()
    for (t in seq(252, (252 * 50), 252)) {
      X <- logPriceProcess(t = t, mu = real.mu, rd.sigma = 0.25,
                           stochastic.volatility = stochastic.volatility)
      mu.estimates <- c(mu.estimates, calibrateMu(X))
    }
    lines(mu.estimates, col = colors(1)[292 + i], lw = 2)
  }
}


calibrateSigmaTest <- function(real.sigma = 0.5, samples = 30, 
                               stochastic.volatility = TRUE) {
  # Start off the plot with the actual values (scatter)
  plot(rep(real.sigma, 50), ylab = "Sigma Estimate", xlab = "Length of X (years)",
       main = "Illustration of the paramator estimator for Sigma",
       ylim = c(real.sigma - (0.05 * real.sigma), real.sigma + (0.05 * real.sigma)))
  
  # The add samples many estimates of mu from X's of length t.
  for (i in 1:samples) {
    sigma.estimates <- c()
    for (t in seq(252, (252 * 50), 252)) {
      X <- logPriceProcess(t = t, mu = 0.1, rd.sigma = real.sigma,
                           stochastic.volatility = stochastic.volatility)
      sigma.estimates <- c(sigma.estimates, calibrateSigmaOverlapping(X))
    }
    print(sigma.estimates)
    lines(sigma.estimates, col = colors(1)[292 + i], lw = 2)
  }
}


meanTest <- function(sigma, samples = 100000) {
  sigmas <- c()
  for (i in 1:samples)
    sigmas <- c(sigmas, randomDisturbance(mu = 0.0, sigma = sigma, 
                                          stochastic.volatility = TRUE))
  return(sd(sigmas))
}

colouredPoints <- function(points = 100, q = 2) {
  alls <- c()
  odds <- c()
  evens <- c()
  
  for (i in 1:points) {
    r1 <- runif(1, min = 0.25, max = 0.75)
    alls <- c(alls, r1)
    
    if (i %% q) {
      evens <- c(evens, r1)
      odds <- c(odds, NA)
    } else {
      odds <- c(odds, r1)
      evens <- c(evens, NA)
    }
  }
  
  plot(alls, pch = 17, col = 'black', ylim = c(0, 1), ylab = "Random Disturbance",
       xlab = "Time, t", main = "All Random Disturbances")
  plot(odds, pch = 19, col = 'darkgreen', ylim = c(0, 1), ylab = "Random Disturbance",
       xlab = "Time, t", main = "Two Subsets of Random Disturbances")
  points(evens, pch = 15, col = 'red')
}


#' @title Estimator for the value of sigma. Sigma is the standard deviation of 
#' the random component of returns in the Geometric Brownian Motion model. This 
#' estimate can be calculated in a biased or unbiased manner.
#' 
#' @description Given a log price process and a parameter, q, which specifies 
#' the sampling intervel this function estimates the value of Sigma. Sigma 
#' represents the standarddeviation of the random disturbance component of daily 
#' returns. This estimate can be annualized and can be computed in a biased or 
#' unbiased manner.
#' 
#' @details The parameter, q, specifies the sampling interval to use when 
#' estimating sigma. When q = 1 the function will use every day's prices, when 
#' q = 2 the function will use every second day's prices, and so on and so 
#' forth. For a sufficient number of days the estimation of sigma under 
#' different sampling intervales e.g. 2 and 4, should converge.
#' 
#' @param X vector :: A log price process.
#' @param q int :: The sampling interval for the estimator.
#' @param annualize logical :: Annualize the parameter estimate. True or False.
#' @param unbiased logical :: Use the unbiased estimate. True or False.
#' @return sd.est double :: The estimated value of Sigma.
#' 
calibrateSigma <- function(X, q = 1, annualize = TRUE, unbiased = TRUE) {
  # Get the estimate value for the drift component.
  mu.est <- calibrateMu(X, annualize = FALSE)
  
  # Ensure that the format of X is appropriate.
  X <- as.numeric(as.vector(X))
  
  # Calculate the number of times q goes into the length of X.
  n <- floor(length(X)/q)
  sd.est <- 0.0
  for (t in 2:n)
    sd.est <- sd.est + (X[t * q] - X[(t * q) - q] - (q * mu.est))^2
  
  # Calculate the average sigma using the unbiased or biased method.
  if (!unbiased) sd.est <- sd.est / (q * n)
  else sd.est <- sd.est / (q * n - 1)
  
  if (!annualize) return(sd.est)
  else return(sqrt((sd.est * 252)))
}


#' @title A more efficient estimator for the value of sigma. Sigma is the 
#' standard deviation of the random component of returns in the Geometric 
#' Brownian Motion model. This estimate can be calculated in an unbiased manner.
#' 
#' @description Given a log price process and a parameter, q, which specifies 
#' the sampling intervel this function estimates the value of Sigma. Sigma 
#' represents the standarddeviation of the random disturbance component of 
#' daily  returns. This estimate can be annualized and can be computed in 
#' a biased or unbiased manner.
#' 
#' @details The difference between this estimator and the estimator defined in 
#' the calibrateSigma function is that this method makes use of overlapping 
#' windows of log price data. Whilst this does increase the number of 
#' observations and improve the accuracy of the estimator it is no longer 
#' unbiased. That said, Monte Carlo simulations indicate that this bias is 
#' negligable and, in fact, this estimator is more accurate than the one 
#' defined by calibrateSigma. 
#' 
#' @inheritParams calibrateSigma
#' @return sd.est double :: The estimated value of Sigma.
#' 
calibrateSigmaOverlapping <- function(X, q = 1, annualize = TRUE, 
                                      unbiased = TRUE) {
  # Get the estimate value for the drift component.
  mu.est <- calibrateMu(X, annualize = FALSE)
  
  # Ensure that the format of X is appropriate.
  X <- as.numeric(as.vector(X))
  
  # Calculate the number of times q goes into the length of X.
  n <- floor(length(X)/q)
  sd.est <- 0.0
  for (t in (q + 1):(n * q))
    sd.est <- sd.est + (X[t] - X[t - q] - (q * mu.est))^2
  
  # Calculate the average sigma using the unbiased or biased method.
  if (!unbiased) sd.est <- sd.est / (n * (q^2))
  else sd.est <- sd.est / ((q * ((n * q) - q + 1)) * (1 - (q / (n * q))))
  
  if (!annualize) return(sd.est)
  else return(sqrt((sd.est * 252)))
}


#' @title Estimator for the value of the asymptotic variance of the Mr statistic. 
#' This is equivalent to a weighted sum of the asymptotic variances for each of 
#' the autocorrelation co-efficients under the null hypothesis.
#' 
#' @details Given a log price process, X, and a sampling interval, q, this 
#' method is used to estimate the asymptoticvariance of the Mr statistic in the 
#' presence of stochastic volatility. In other words, it is a heteroskedasticity 
#' consistent estimator of the variance of the Mr statistic. This parameter is 
#' used to estimate the probability that the given log price process was 
#' generated by a Brownian Motion model with drift and stochastic volatility. 
#' 
#' @param X vector :: A log price process.
#' @param q int :: The sampling interval for the estimator.
#' 
calibrateAsymptoticVariance <- function(X, q) {
  avar <- 0.0
  for (j in 1:(q - 1)) {
    theta <- calibrateDelta(X, q, j)
    avar <- avar + (2 * (q - j) / q) ^ 2 * theta
  }
  return(avar)
}


#' @title Helper function for the calibrateAsymptoticVariance function.
#' 
calibrateDelta <- function(X, q, j) { 
  # Get the estimate value for the drift component.
  mu.est <- calibrateMu(X, FALSE)
  
  # Estimate the asymptotice variance given q and j.
  n <- floor(length(X)/q)
  numerator <- 0.0
  for (k in (j + 2):(n * q)) {
    t1 <- (X[k] - X[k - 1] - mu.est)^2
    t2 <- (X[k - j] - X[k - j - 1] - mu.est)^2
    numerator <- numerator + (t1 * t2)
  }
  denominator <- 0.0
  for (k in 2:(n * q))
    denominator <- denominator + (X[k] - X[k - 1] - mu.est)^2
  
  # Compute and return the statistic.
  thetaJ <- (n * q * numerator) / (denominator^2)
  return(thetaJ)
}


#' @title Given a log price process, X, compute the Z-score which can be used 
#' to accept or reject the hypothesis that the process evolved according to a 
#' Brownian Motion model with drift and stochastic volatility.
#' 
#' @description Given a log price process, X, and a sampling interval, q, this 
#' method returns a Z score indicating the confidence we have that X evolved 
#' according to a Brownian Motion mode with drift and stochastic volatility. This
#' heteroskedasticity-consistent variance ratio test essentially checks to see 
#' whether or not the observed Mr statistic for the number of observations, is 
#' within or out of the limiting distribution defined by the Asymptotic Variance.
#' 
#' @param X vector :: A log price process.
#' @param q int :: The sampling interval for the estimator.
#' 
VRTestZScore <- function(X, q) {
  n <- floor(length(X)/q)
  z <- sqrt(n * q) * mRatio(X, q)
  z <- z / sqrt(calibrateAsymptoticVariance(X, q))
  return(z)
}


# Test statistics defined by Lo and MacKinlay for Jd, Jr, Md, and Mr
# ------------------------------------------------------------------------------


#' @title Compute the Jd statistic.
#' 
#' @description Compute the Jd statistic. The Jd statistic is the difference 
#' between two estimate values for Sigma computed using the calibrateSigma 
#' function for sampling intervals 1 and q. This statistic should converge to zero.
#' 
#' @inheritParams calibrateSigma
#' @return Jd double :: The Jd statistic defined by Lo and MacKinlay.
#' 
jDifferences <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  # Estimate the value of sigma at sampling intervals 1 and q.
  sigmaA <- calibrateSigma(X, q = 1, annualize, unbiased)
  sigmaB <- calibrateSigma(X, q = q, annualize, unbiased)
  Jd <- sigmaA - sigmaB
  return(Jd)
}


#' @title Compute the Jr statistic.
#' 
#' @description Compute the Jr statistic. The Jr statistic is the ratio of two 
#' estimate values for Sigma computed using the calibrateSigma function for 
#' sampling intervals 1 and q minus 1. This statistic should converge to zero.
#' 
#' @inheritParams calibrateSigma
#' @return Jr double :: The Jr statistic defined by Lo and MacKinlay.
#' 
jRatio <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  # Estimate the value of sigma at sampling intervals 1 and q.
  sigmaA <- calibrateSigma(X, q = 1, annualize, unbiased)
  sigmaB <- calibrateSigma(X, q = q, annualize, unbiased)
  Jr <- (sigmaA / sigmaB) - 1
  return(Jr)
}


#' @title Compute the Md statistic.
#' 
#' @description Compute the Md statistic. The Md statistic is the difference 
#' between two estimate values for Sigma computed using the calibrateSigma 
#' function for sampling intervals 1 and the estimate value for Sima computed 
#' using the CalibrateSigmaOverlapping function for sampling inveral q. 
#' This statistic should converge to zero.
#' 
#' @inheritParams calibrateSigma
#' @return Md double :: The Md statistic defined by Lo and MacKinlay.
#' 
mDifferences <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  # Estimate the value of sigma at sampling intervals 1 and q.
  sigmaA <- calibrateSigma(X, q = 1, annualize, unbiased)
  sigmaB <- calibrateSigmaOverlapping(X, q = q, annualize, unbiased)
  Md <- sigmaA - sigmaB
  return(Md)
}


mDifferencesTest <- function(paths = 500, q = 2, stochastic.volatility = TRUE) {
  values <- c()
  for (i in 1:paths) {
    mu.rand <- runif(1, min = -0.25, max = 0.25)
    sd.rand <- runif(1, min = 0.005, max = 0.75)
    
    X <- logPriceProcess(t = (252 * 10), mu = mu.rand, rd.sigma = sd.rand,
                         stochastic.volatility = stochastic.volatility)
    
    values <- c(values, mDifferences(X, q = q))
  }
  
  if (stochastic.volatility) main <- "Values of Md with Stochastic Volatility"
  else main <- "Values of Md without Stochastic Volatility"
  
  plot(values, main = main, xlab = "Log Price Process Index", 
       ylab = paste("Value of Md(q=", q, ")", sep = ''), 
       ylim = c(-0.00025, 0.00025))
}


mRatioTest <- function(paths = 500, q = 2, stochastic.volatility = TRUE) {
  values <- c()
  for (i in 1:paths) {
    mu.rand <- runif(1, min = -0.25, max = 0.25)
    sd.rand <- runif(1, min = 0.005, max = 0.75)
    
    X <- logPriceProcess(t = (252 * 10), mu = mu.rand, rd.sigma = sd.rand,
                         stochastic.volatility = stochastic.volatility)
    
    values <- c(values, mRatio(X, q = q))
  }
  
  if (stochastic.volatility) main <- "Values of Mr with Stochastic Volatility"
  else main <- "Values of Mr without Stochastic Volatility"
  
  plot(values, main = main, xlab = "Log Price Process Index", 
       ylab = paste("Value of Mr(q=", q, ")", sep = ''), 
       ylim = c(-0.175, 0.175))
}


#' @title Compute the Mr statistic.
#' 
#' @description Compute the Mr statistic. The Md statistic is the ratio between 
#' two estimate values for Sigma computed using the calibrateSigma function for 
#' sampling intervals 1 and the estimate value for Sima computed using the
#' CalibrateSigmaOverlapping function for sampling inveral q minus 1. 
#' This statistic should converge to zero.
#' 
#' @inheritParams calibrateSigma
#' @return Mr double :: The Mr statistic defined by Lo and MacKinlay.
#' 
mRatio <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  # Estimate the value of sigma at sampling intervals 1 and q.
  sigmaA <- calibrateSigma(X, q = 1, annualize, unbiased)
  sigmaB <- calibrateSigmaOverlapping(X, q = q, annualize, unbiased)
  Mr <- (sigmaA / sigmaB) - 1
  return(Mr)
}


# Test functions for checking that the code works. I.e. feed in data produced 
# by the model into the test statistics and check that the resulting outputs and 
# conclusions are consistent with the inputs.
# ------------------------------------------------------------------------------


zScatter <- function(q, samples = 2500, stochastic.volatility = TRUE) {
  x <- c()
  for (i in 2:samples) {
    # Generate a log price process.
    X <- logPriceProcess(t = (252 * runif(1, min = 5, max = 25)), 
                         mu = runif(1, min = -0.25, max = 0.25),
                         rd.sigma = runif(1, min = 0.05, max = 0.75), 
                         stochastic.volatility = stochastic.volatility)
    x <- c(x, VRTestZScore(X, q))
    print(paste("Completed", i, "out of", samples, "samples"))
  }
  y <- rnorm(samples, 0.0)
  checkIfNormal(x, y)
}


checkIfNormal <- function(x, y) {
  # Plot the density functions.
  x.density <- density(x)
  plot(x.density, col = rgb(0,0,1,1/4), xlim = c(-5, 5), 
       main = "Comparison of Densities of a Normal Distributed Variable (Red)
       against the Z* scores computed for GBM with stochastic volatility")
  polygon(x.density, col = rgb(0,0,1,1/4))
  
  y.density <- density(y)
  lines(y.density, col = rgb(1,0,0,1/4), xlim = c(-5, 5))
  polygon(y.density, col = rgb(1,0,0,1/4))

  if (length(x) < 5000) {
    print(shapiro.test(x))
    print(shapiro.test(y))
  }

  ## Plot using a qqplot
  qqnorm(x)
  qqline(x, col = 2)
  
  qqnorm(y)
  qqline(y, col = 2)
}


#' @title Check that simulated paths have the expected statistics.
#' @description This method checks that the annualized returns and standard 
#' deviations of the simulated return series are in line with the expected 
#' annualized return and standard deviationswhich were supplied to the model
#' as inputs.
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
#' @description This method is used to testing that the parameter values 
#' estimated from simulated log price processes match the parameter values 
#' specified in the stochastic model that was used to simulate the data in the 
#' first place.
#' 
#' @param tries int :: The number of independent tests to run.
#' @param passes int :: The number of passes per test to run.
#' @param tau double :: A parameter to control the sensitivity of the test.
#' 
testCalibration <- function(tries = 30, passes = 300, tau = 0.05) {
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
      sigmaq.overlapping.est <- sigmaq.overlapping.est + 
        calibrateSigmaOverlapping(X, 4)
    }
    
    # Calculate the estimates.
    mu.est <- mu.est / passes
    sigma1.est <- sigma1.est / passes
    sigma2.est <- sigma2.est / passes
    sigmaq.est <- sigmaq.est / passes
    sigmaq.overlapping.est <- sigmaq.overlapping.est / passes
    
    # Check that the differences are negligable.
    if (!abs(mu.est - mu) < tau) calibratorWarning("mu", mu, mu.est, tau)
    else if (!abs(sigma1.est - sigma) < tau) 
      calibratorWarning("sigma", sigma, sigma1.est, tau)
    else if (!abs(sigma2.est - sigma) < tau) 
      calibratorWarning("sigma", sigma, sigma2.est, tau)
    else if (!abs(sigmaq.est - sigma) < tau) 
      calibratorWarning("sigma", sigma, sigmaq.est, tau)
    else if (!abs(sigmaq.overlapping.est - sigma) < tau) 
      calibratorWarning("sigma", sigma, sigmaq.overlapping.est, tau)
    else print(paste(t, "Yay, no warnings! mu:", mu, "><", 
                     mu.est, "sigma:", sigma, "><", sigmaq.overlapping.est))
  }
}


#' @title Generate a useful warning saying that the calibrator has failed.
#'
calibratorWarning <- function(param, true, estimate, tau) {
  warning(paste("Estimate for", param, ",", estimate, 
                "is further away from the true value for", 
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


# Results obtained on real assets
# ------------------------------------------------------------------------------


realAssetPriceResults <- function(q, indices, frequency = "daily",
                                  subset.recent = TRUE, csv.outf = "out.csv") {
  library(Quandl)
  
  real.zs <- c()
  sims.zs <- c()
  lines <- c(paste("Quandl Code, Compute z* Score, Observations Used"))
  
  for (index in indices) {
    if (index != "YAHOO/INDEX_IBEX")
      raw.data <- Quandl(index, rows = -1, collapse = frequency)
    else
      raw.data <- Quandl(index, rows = -1, collapse = frequency,
                         start_date = "1993-10-03")
    
    # Compute the log price process from the Adjusted Close (if available).
    data = tryCatch({
      as.numeric(as.vector(log(raw.data$"Adjusted Close")))
    }, error = function(e) {
      as.numeric(as.vector(log(raw.data$"Adj Close")))
    }, error = function(e) {
      as.numeric(as.vector(log(raw.data$"Close")))
    })
    
    # Just remove any ugly data.
    data[is.nan(data)] <- NA
    data[is.infinite(data)] <- NA
    data <- na.omit(data)
    
    if (length(data) >= (252 * 10)) {
      # Subset the data (run on just the last decade)
      if (subset.recent)
        data <- head(data, (252 * 10))
      
      # Simulated similar data for dist comparison.
      mu.est <- calibrateMu(data)
      sd.est <- calibrateSigmaOverlapping(data)
      data.sim <- logPriceProcess(t = length(data), 
                                  mu = mu.est, rd.sigma = sd.est,
                                  stochastic.volatility = TRUE)
      
      # Compute the resulting z*-score.
      z.score.real <- VRTestZScore(data, q)
      z.score.simu <- VRTestZScore(data.sim, q)
      
      # Add it to the data.
      real.zs <- c(real.zs, z.score.real)
      sims.zs <- c(sims.zs, z.score.simu)
      
      # Print out the results.
      line <- paste(index, z.score.real, length(data), sep = ',')
      lines <- c(lines, line)
      print(line)
    }
  }
  # Check if the data is normal.
  checkIfNormalvTwo(real.zs, sims.zs)
  
  # Write out the results to CSV.
  file.out <- file(csv.outf)
  writeLines(lines, file.out)
  close(file.out)
}


checkIfNormalvTwo <- function(x, y) {
  # Plot the density functions.
  x.density <- density(x)
  y.density <- density(y)
  
  ymax <- max(max(x.density$y), max(y.density$y)) + 0.005
  xmax <- max(max(x.density$x), max(y.density$x)) + 0.005
  xmin <- min(min(x.density$x), min(y.density$x)) - 0.005
  
  plot(x.density, col = rgb(0,0,1,1/4), 
       xlim = c(xmin, xmax), ylim = c(0.0, ymax),
       main = "Comparison of Z* scores for simulated log price processes (Red)
       against the Z* scores of real stock market indices (Blue)")
  polygon(x.density, col = rgb(0,0,1,1/4))
  lines(y.density, col = rgb(1,0,0,1/4))
  polygon(y.density, col = rgb(1,0,0,1/4))
  
  if (length(x) < 5000) {
    print(shapiro.test(x))
    print(shapiro.test(y))
  }
}


getGlobalStockMarketIndices <- function() {
  # List of Global Stock Market Indices / ETFs (where index not available)
  indices <- c("YAHOO/INDEX_DJI", # Dow Jones Industrial (USA)
               "YAHOO/INDEX_GSPC", # S&P 500 Index (USA)
               "YAHOO/INDEX_MID", # S&P 400 Mid-cap Index (USA)
               "YAHOO/INDEX_SML", # S&P 600 Small-cap Index (USA)
               "YAHOO/NDAQ", # NASDAQ Composite (USA)
               "YAHOO/INDEX_NYA", # NYSE Composite (USA)
               "YAHOO/INDEX_RUI", # Russel 1000 Index (USA)
               "YAHOO/INDEX_RUT", # Russel 2000 Index (USA)
               "YAHOO/INDEX_RUA", # Russel 3000 Index (USA)
               "YAHOO/INDEX_XOI", # NYSE AMEX Oil Index (USA)
               "YAHOO/INDEX_XAX", # NYSE AMEX Composite Index (USA)
               "YAHOO/INDEX_MERV", # Merval Index (Argentina)
               "YAHOO/INDEX_BVSP", # Bovespa Index (Brazil)
               "YAHOO/INDEX_MXX", # Inmex Index (Mexico)
               "YAHOO/INDEX_MXY", # NYSE ARCA Mexico Index (Mexico)
               "YAHOO/INDEX_GSPTSE", # S&P TSX Index (Canada)
               "YAHOO/INDEX_IEQR_IR", # ISEQ General Total Return Index (Ireland)
               "YAHOO/INDEX_GD_AT", # Athens Composite Index (Greece)
               "YAHOO/INDEX_FCHI", # CAC40 Index (France)
               "YAHOO/INDEX_GDAXI", # DAX Index (Germany)
               "YAHOO/INDEX_OMXC20_CO", # OMX 20 Copenhagen (Denmark)
               "GOOG/JSE_STX40", # SATRIX Top 40 (South Africa)
               "GOOG/JSE_STXIND", # SATRIX Industrials (South Africa)
               "GOOG/JSE_STXFIN", # SATRIX Financials (South Africa)
               "GOOG/JSE_STXRES", # SATRIX Resources (South Africa)
               "GOOG/JSE_STXSWX", # SATRIX SWIX (South Africa)
               "YAHOO/INDEX_CCSI", # EGX 70 Price Index (Egypt) 
               "YAHOO/INDEX_RTS_RS", # Russia RTS Index (Russia)
               "YAHOO/INDEX_SSMI", # Swiss Market Index (Switzerland)
               "YAHOO/INDEX_TA100", # Tel Aviv Top 100 Index (Israel)
               "YAHOO/INDEX_STOXX50E", # Eurostoxx 50 (Europe)
               "YAHOO/INDEX_IBEX", # IBEX 35 (Spain)
               "YAHOO/INDEX_AEX", # AEX Index (Netherlands)
               "YAHOO/INDEX_BFX", # BEL20 Index (Belgium)
               "YAHOO/INDEX_ATX", # ATX Index (Austria)
               "YAHOO/INDEX_OMX", # OMX30 Index (Sweden)
               "YAHOO/INDEX_N225", # Nikkei 225 (Japan)
               "YAHOO/INDEX_JPN", # Japan AMEX Index (Japan)
               "YAHOO/INDEX_SSEC", # Shanghai Composite Index (China)
               "YAHOO/SS_000300", # CSI300 Index (China)
               "YAHOO/INDEX_HSI", # Hang Send Index (Hong Kong)
               "YAHOO/INDEX_KS11", # KOSPI Composite Index (South Korea)
               "YAHOO/INDEX_BSESN", # BSE 30 Sensitivity Index (SENSEX) (India)
               "YAHOO/INDEX_NSEI", # Nifty Fifty Index (India)
               "YAHOO/INDEX_CSE", # Colombo All Share Index (Sri Lanka)
               "YAHOO/INDEX_NZ50", # New Zealand Stock Exchange (New Zealand)
               "YAHOO/INDEX_TWII", # Taiwan Weighted Index (Taiwan)
               "YAHOO/INDEX_AORD", # All Ordinaries Index (Australia)
               "YAHOO/INDEX_AXJO", # S&P/ASX 200 Index (Australia)
               "YAHOO/INDEX_JKSE", # Jakarta Composite Index (Indonesia)
               "YAHOO/INDEX_STI") # Strait Times Index (Singapore)
  return(indices)
}


getSP500Constituents <- function() {
  sp500 <- c("YAHOO/A",
             "YAHOO/AA",
             "YAHOO/AAL",
             "YAHOO/AAP",
             "YAHOO/AAPL",
             "YAHOO/ABBV",
             "YAHOO/ABC",
             "YAHOO/ABT",
             "YAHOO/ACN",
             "YAHOO/ADBE",
             "YAHOO/ADI",
             "YAHOO/ADM",
             "YAHOO/ADP",
             "YAHOO/ADS",
             "YAHOO/ADSK",
             "YAHOO/ADT",
             "YAHOO/AEE",
             "YAHOO/AEP",
             "YAHOO/AES",
             "YAHOO/AET",
             "YAHOO/AFL",
             # "YAHOO/AGN", Ugly data.
             "YAHOO/AIG",
             "YAHOO/AIV",
             "YAHOO/AIZ",
             "YAHOO/AKAM",
             "YAHOO/ALL",
             "YAHOO/ALLE",
             "YAHOO/ALXN",
             "YAHOO/AMAT",
             "YAHOO/AME",
             "YAHOO/AMG",
             "YAHOO/AMGN",
             "YAHOO/AMP",
             "YAHOO/AMT",
             "YAHOO/AMZN",
             "YAHOO/AN",
             # "YAHOO/ANTM", Stock not found.
             "YAHOO/AON",
             "YAHOO/APA",
             "YAHOO/APC",
             "YAHOO/APD",
             "YAHOO/APH",
             "YAHOO/ARG",
             "YAHOO/ATVI",
             "YAHOO/AVB",
             "YAHOO/AVGO",
             "YAHOO/AVY",
             "YAHOO/AXP",
             "YAHOO/AZO",
             "YAHOO/BA",
             "YAHOO/BAC",
             "YAHOO/BAX",
             "YAHOO/BBBY",
             "YAHOO/BBT",
             "YAHOO/BBY",
             "YAHOO/BCR",
             "YAHOO/BDX",
             "YAHOO/BEN",
             "YAHOO/BF_B",
             "YAHOO/BHI",
             "YAHOO/BIIB",
             "YAHOO/BK",
             "YAHOO/BLK",
             "YAHOO/BLL",
             "YAHOO/BMY",
             "YAHOO/BRK_B",
             "YAHOO/BSX",
             "YAHOO/BWA",
             # "YAHOO/BXLT", Stock not found.
             "YAHOO/BXP",
             "YAHOO/C",
             "YAHOO/CA",
             "YAHOO/CAG",
             "YAHOO/CAH",
             "YAHOO/CAM",
             "YAHOO/CAT",
             "YAHOO/CB",
             "YAHOO/CBG",
             "YAHOO/CBS",
             "YAHOO/CCE",
             "YAHOO/CCI",
             "YAHOO/CCL",
             "YAHOO/CELG",
             "YAHOO/CERN",
             "YAHOO/CF",
             # "YAHOO/CFG", Stock not found.
             "YAHOO/CHD",
             "YAHOO/CHK",
             "YAHOO/CHRW",
             "YAHOO/CI",
             "YAHOO/CINF",
             "YAHOO/CL",
             "YAHOO/CLX",
             "YAHOO/CMA",
             "YAHOO/CMCSA",
             "YAHOO/CME",
             "YAHOO/CMG",
             "YAHOO/CMI",
             "YAHOO/CMS",
             "YAHOO/CNP",
             "YAHOO/CNX",
             "YAHOO/COF",
             "YAHOO/COG",
             "YAHOO/COH",
             "YAHOO/COL",
             "YAHOO/COP",
             "YAHOO/COST",
             "YAHOO/CPB",
             # "YAHOO/CPGX", Stock not found.
             "YAHOO/CRM",
             "YAHOO/CSCO",
             # "YAHOO/CSRA", Stock not found.
             "YAHOO/CSX",
             "YAHOO/CTAS",
             "YAHOO/CTL",
             "YAHOO/CTSH",
             "YAHOO/CTXS",
             "YAHOO/CVC",
             "YAHOO/CVS",
             "YAHOO/CVX",
             "YAHOO/D",
             "YAHOO/DAL",
             "YAHOO/DD",
             "YAHOO/DE",
             "YAHOO/DFS",
             "YAHOO/DG",
             "YAHOO/DGX",
             "YAHOO/DHI",
             "YAHOO/DHR",
             "YAHOO/DIS",
             "YAHOO/DISCA",
             "YAHOO/DISCK",
             "YAHOO/DLPH",
             "YAHOO/DLTR",
             "YAHOO/DNB",
             "YAHOO/DO",
             "YAHOO/DOV",
             "YAHOO/DOW",
             "YAHOO/DPS",
             "YAHOO/DRI",
             "YAHOO/DTE",
             "YAHOO/DUK",
             "YAHOO/DVA",
             "YAHOO/DVN",
             "YAHOO/EA",
             "YAHOO/EBAY",
             "YAHOO/ECL",
             "YAHOO/ED",
             "YAHOO/EFX",
             "YAHOO/EIX",
             "YAHOO/EL",
             "YAHOO/EMC",
             "YAHOO/EMN",
             "YAHOO/EMR",
             "YAHOO/ENDP",
             "YAHOO/EOG",
             "YAHOO/EQIX",
             "YAHOO/EQR",
             "YAHOO/EQT",
             # "YAHOO/ES", Stock not found.
             "YAHOO/ESRX",
             "YAHOO/ESS",
             "YAHOO/ESV",
             "YAHOO/ETFC",
             "YAHOO/ETN",
             "YAHOO/ETR",
             "YAHOO/EW",
             "YAHOO/EXC",
             "YAHOO/EXPD",
             "YAHOO/EXPE",
             "YAHOO/EXR",
             "YAHOO/F",
             "YAHOO/FAST",
             "YAHOO/FB",
             "YAHOO/FCX",
             "YAHOO/FDX",
             "YAHOO/FE",
             "YAHOO/FFIV",
             "YAHOO/FIS",
             "YAHOO/FISV",
             "YAHOO/FITB",
             "YAHOO/FLIR",
             "YAHOO/FLR",
             "YAHOO/FLS",
             "YAHOO/FMC",
             "YAHOO/FOX",
             "YAHOO/FOXA",
             "YAHOO/FRT",
             "YAHOO/FSLR",
             "YAHOO/FTI",
             "YAHOO/FTR",
             "YAHOO/GAS",
             "YAHOO/GD",
             "YAHOO/GE",
             "YAHOO/GGP",
             "YAHOO/GILD",
             "YAHOO/GIS",
             "YAHOO/GLW",
             "YAHOO/GM",
             "YAHOO/GMCR",
             "YAHOO/GME",
             "YAHOO/GOOG",
             "YAHOO/GOOGL",
             "YAHOO/GPC",
             "YAHOO/GPS",
             "YAHOO/GRMN",
             "YAHOO/GS",
             "YAHOO/GT",
             "YAHOO/GWW",
             "YAHOO/HAL",
             "YAHOO/HAR",
             "YAHOO/HAS",
             "YAHOO/HBAN",
             "YAHOO/HBI",
             "YAHOO/HCA",
             "YAHOO/HCN",
             "YAHOO/HCP",
             "YAHOO/HD",
             "YAHOO/HES",
             "YAHOO/HIG",
             "YAHOO/HOG",
             "YAHOO/HON",
             "YAHOO/HOT",
             "YAHOO/HP",
             # "YAHOO/HPE", Stock not found.
             "YAHOO/HPQ",
             "YAHOO/HRB",
             "YAHOO/HRL",
             "YAHOO/HRS",
             "YAHOO/HSIC",
             "YAHOO/HST",
             "YAHOO/HSY",
             "YAHOO/HUM",
             "YAHOO/IBM",
             "YAHOO/ICE",
             "YAHOO/IFF",
             "YAHOO/ILMN",
             "YAHOO/INTC",
             "YAHOO/INTU",
             "YAHOO/IP",
             "YAHOO/IPG",
             "YAHOO/IR",
             "YAHOO/IRM",
             "YAHOO/ISRG",
             "YAHOO/ITW",
             "YAHOO/IVZ",
             "YAHOO/JBHT",
             "YAHOO/JCI",
             "YAHOO/JEC",
             "YAHOO/JNJ",
             "YAHOO/JNPR",
             "YAHOO/JPM",
             "YAHOO/JWN",
             "YAHOO/K",
             "YAHOO/KEY",
             # "YAHOO/KHC", Stock not found.
             "YAHOO/KIM",
             "YAHOO/KLAC",
             "YAHOO/KMB",
             "YAHOO/KMI",
             "YAHOO/KMX",
             "YAHOO/KO",
             "YAHOO/KORS",
             "YAHOO/KR",
             "YAHOO/KSS",
             "YAHOO/KSU",
             "YAHOO/L",
             "YAHOO/LB",
             "YAHOO/LEG",
             "YAHOO/LEN",
             "YAHOO/LH",
             "YAHOO/LLL",
             "YAHOO/LLTC",
             "YAHOO/LLY",
             "YAHOO/LM",
             "YAHOO/LMT",
             "YAHOO/LNC",
             "YAHOO/LOW",
             "YAHOO/LRCX",
             "YAHOO/LUK",
             "YAHOO/LUV",
             "YAHOO/LVLT",
             "YAHOO/LYB",
             "YAHOO/M",
             "YAHOO/MA",
             "YAHOO/MAC",
             "YAHOO/MAR",
             "YAHOO/MAS",
             "YAHOO/MAT",
             "YAHOO/MCD",
             "YAHOO/MCHP",
             "YAHOO/MCK",
             "YAHOO/MCO",
             "YAHOO/MDLZ",
             "YAHOO/MDT",
             "YAHOO/MET",
             "YAHOO/MHFI",
             "YAHOO/MHK",
             "YAHOO/MJN",
             "YAHOO/MKC",
             "YAHOO/MLM",
             "YAHOO/MMC",
             "YAHOO/MMM",
             "YAHOO/MNK",
             "YAHOO/MNST",
             "YAHOO/MO",
             "YAHOO/MON",
             "YAHOO/MOS",
             "YAHOO/MPC",
             "YAHOO/MRK",
             "YAHOO/MRO",
             "YAHOO/MS",
             "YAHOO/MSFT",
             "YAHOO/MSI",
             "YAHOO/MTB",
             "YAHOO/MU",
             "YAHOO/MUR",
             # "YAHOO/MYL", Stock not found.
             # "YAHOO/NAVI", Stock not found.
             "YAHOO/NBL",
             "YAHOO/NDAQ",
             "YAHOO/NEE",
             "YAHOO/NEM",
             "YAHOO/NFLX",
             "YAHOO/NFX",
             "YAHOO/NI",
             "YAHOO/NKE",
             "YAHOO/NLSN",
             "YAHOO/NOC",
             "YAHOO/NOV",
             "YAHOO/NRG",
             "YAHOO/NSC",
             "YAHOO/NTAP",
             "YAHOO/NTRS",
             "YAHOO/NUE",
             "YAHOO/NVDA",
             "YAHOO/NWL",
             "YAHOO/NWS",
             "YAHOO/NWSA",
             "YAHOO/O",
             "YAHOO/OI",
             "YAHOO/OKE",
             "YAHOO/OMC",
             "YAHOO/ORCL",
             "YAHOO/ORLY",
             "YAHOO/OXY",
             "YAHOO/PAYX",
             "YAHOO/PBCT",
             "YAHOO/PBI",
             "YAHOO/PCAR",
             "YAHOO/PCG",
             "YAHOO/PCL",
             "YAHOO/PCLN",
             "YAHOO/PDCO",
             "YAHOO/PEG",
             "YAHOO/PEP",
             "YAHOO/PFE",
             "YAHOO/PFG",
             "YAHOO/PG",
             "YAHOO/PGR",
             "YAHOO/PH",
             "YAHOO/PHM",
             "YAHOO/PKI",
             "YAHOO/PLD",
             "YAHOO/PM",
             "YAHOO/PNC",
             "YAHOO/PNR",
             "YAHOO/PNW",
             "YAHOO/POM",
             "YAHOO/PPG",
             "YAHOO/PPL",
             "YAHOO/PRGO",
             "YAHOO/PRU",
             "YAHOO/PSA",
             "YAHOO/PSX",
             "YAHOO/PVH",
             "YAHOO/PWR",
             "YAHOO/PX",
             "YAHOO/PXD",
             # "YAHOO/PYPL", Stock not found.
             "YAHOO/QCOM",
             # "YAHOO/QRVO", Stock not found.
             "YAHOO/R",
             "YAHOO/RAI",
             "YAHOO/RCL",
             "YAHOO/REGN",
             "YAHOO/RF",
             "YAHOO/RHI",
             "YAHOO/RHT",
             "YAHOO/RIG",
             "YAHOO/RL",
             "YAHOO/ROK",
             "YAHOO/ROP",
             "YAHOO/ROST",
             "YAHOO/RRC",
             "YAHOO/RSG",
             "YAHOO/RTN",
             "YAHOO/SBUX",
             "YAHOO/SCG",
             "YAHOO/SCHW",
             "YAHOO/SE",
             "YAHOO/SEE",
             "YAHOO/SHW",
             "YAHOO/SIG",
             "YAHOO/SJM",
             "YAHOO/SLB",
             "YAHOO/SLG",
             "YAHOO/SNA",
             "YAHOO/SNDK",
             "YAHOO/SNI",
             "YAHOO/SO",
             "YAHOO/SPG",
             "YAHOO/SPLS",
             "YAHOO/SRCL",
             "YAHOO/SRE",
             "YAHOO/STI",
             "YAHOO/STJ",
             "YAHOO/STT",
             "YAHOO/STX",
             "YAHOO/STZ",
             "YAHOO/SWK",
             "YAHOO/SWKS",
             "YAHOO/SWN",
             # "YAHOO/SYF", Stock not found.
             "YAHOO/SYK",
             "YAHOO/SYMC",
             "YAHOO/SYY",
             "YAHOO/T",
             "YAHOO/TAP",
             "YAHOO/TDC",
             "YAHOO/TE",
             "YAHOO/TEL",
             # "YAHOO/TGNA", Stock not found.
             "YAHOO/TGT",
             "YAHOO/THC",
             "YAHOO/TIF",
             "YAHOO/TJX",
             "YAHOO/TMK",
             "YAHOO/TMO",
             "YAHOO/TRIP",
             "YAHOO/TROW",
             "YAHOO/TRV",
             "YAHOO/TSCO",
             "YAHOO/TSN",
             "YAHOO/TSO",
             "YAHOO/TSS",
             "YAHOO/TWC",
             "YAHOO/TWX",
             "YAHOO/TXN",
             "YAHOO/TXT",
             "YAHOO/TYC",
             "YAHOO/UA",
             "YAHOO/UAL",
             "YAHOO/UHS",
             "YAHOO/UNH",
             # "YAHOO/UNM", Removed due to ugly data.
             "YAHOO/UNP",
             "YAHOO/UPS",
             "YAHOO/URBN",
             "YAHOO/URI",
             "YAHOO/USB",
             "YAHOO/UTX",
             "YAHOO/V",
             "YAHOO/VAR",
             "YAHOO/VFC",
             "YAHOO/VIAB",
             "YAHOO/VLO",
             "YAHOO/VMC",
             "YAHOO/VNO",
             "YAHOO/VRSK",
             "YAHOO/VRSN",
             "YAHOO/VRTX",
             "YAHOO/VTR",
             "YAHOO/VZ",
             "YAHOO/WAT",
             # "YAHOO/WBA", Stock not found.
             "YAHOO/WDC",
             "YAHOO/WEC",
             "YAHOO/WFC",
             "YAHOO/WFM",
             "YAHOO/WHR",
             # "YAHOO/WLTW", Stock not found.
             "YAHOO/WM",
             "YAHOO/WMB",
             "YAHOO/WMT",
             # "YAHOO/WRK", Stock not found.
             "YAHOO/WU",
             "YAHOO/WY",
             "YAHOO/WYN",
             "YAHOO/WYNN",
             "YAHOO/XEC",
             "YAHOO/XEL",
             "YAHOO/XL",
             "YAHOO/XLNX",
             "YAHOO/XOM",
             "YAHOO/XRAY",
             "YAHOO/XRX",
             "YAHOO/XYL",
             "YAHOO/YHOO",
             "YAHOO/YUM",
             # "YAHOO/ZBH", Stock not found.
             "YAHOO/ZION",
             "YAHOO/ZTS")
}






















