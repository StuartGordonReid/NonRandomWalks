source(file = "./VarianceRatioTests.R")


getPriceData <- function(filename = "jse.csv") {
  dataset <- read.csv(filename)
  dates <- as.POSIXct(dataset[ ,"Date"])
  dataset <- dataset[, !(colnames(dataset) %in% c("Date"))]
  return(xts(dataset, order.by = dates))
}


getLogPriceData <- function(filename = "jse.csv") {
  dataset <- getPriceData(filename)
  return(log(dataset))
}


simulateTS <- function(asset.ts, stochastic.volatility, starting = FALSE) {
  asset.ts <- na.omit(asset.ts)
  mu.est <- calibrateMu(asset.ts)
  sd.est <- calibrateSigmaOverlapping(asset.ts)
  
  print(paste(mu.est, sd.est))
  
  if (starting) start <- exp(asset.ts[nrow(asset.ts)][[1]])
  else start = 1.0

  paths <- priceProcesses(n = 10000, X0 = start, t = (252 * 3), mu = mu.est, rd.sigma = sd.est, 
                          stochastic.volatility = stochastic.volatility)
  
  maximum.liklihood <- xts(exp(rowSums(log(paths)) / ncol(paths)), 
                           order.by = as.POSIXct(index(paths)))
  plot.xts(maximum.liklihood, ylim = c(min(paths, na.rm = TRUE), 
                                       max(paths, na.rm = TRUE)))
  
  observedParams(Return.calculate(maximum.liklihood))
  print(calibrateMu(log(maximum.liklihood)))
  
  sum <- 0.0
  for (p in 1:ncol(paths)) {
    color <- colors(1)[as.integer(runif(1, min = 292, max = ncol(paths)))]
    sum <- sum + calibrateMu(log(paths[ ,p]))
    lines(paths[ ,p], col = color)
  }
  
  lines(maximum.liklihood, lw = 2)
  print(sum / 300)
}


simulateAsset <- function(logPrices, stochastic.volatility, asset.name = "SEPHAKU") {
  asset.ts <- logPrices[ ,c(asset.name)]
  simulateTS(asset.ts, stochastic.volatility)
}


simulatePortfolio <- function(logPrices, stochastic.volatility, asset.names, asset.weights) {
  asset.ts <- logPrices[ ,c(asset.names[1])] * asset.weights[1]
  
  if (length(asset.names) >= 2) {
    for (i in 2:length(asset.names))
      asset.ts <- asset.ts + logPrices[ ,c(asset.names[i])] * asset.weights[i]
  }

  simulateTS(asset.ts, stochastic.volatility)
}


analysis <- function() {
  logs <- getLogPriceData()
  
  z.scores <- c()
  for (asset.name in colnames(logs)) {
    logs.asset <- logs[ ,c(asset.name)]
    logs.asset <- na.omit(logs.asset)
    logs.asset <- logs.asset[seq(4, length(logs.asset))]
    if (asset.name == "TREMATON")
      plot(logs.asset)
    logs.asset <- as.numeric(as.vector(logs.asset))
    if (length(logs.asset) > (252 * 5)) {
      logs.asset.z <- VRTestZScore(logs.asset, 2)
      if (!is.nan(logs.asset.z)) {
        z.scores <- c(z.scores, logs.asset.z)
        print(paste(asset.name, logs.asset.z))
      }
    }
  }
  
  print(length(z.scores))
  
  # Plot the distribution of random disturbances generated with constant volatility.
  normal.density <- density(rnorm(length(z.scores)))
  plot(normal.density, col = rgb(0,0,1,1/4), xlim = c(-8, 15),
       main = "Comparison of Homoskedastic vs Heteroskedastic Increments")
  polygon(normal.density, col = rgb(0,0,1,1/4), alpha = 0.3)
  
  # Plot the distribution of random disturbances generated with stochastic volatility.
  real.density <- density(z.scores)
  lines(real.density, col = rgb(1,0,0,1/4), xlim = c(-8, 15))
  polygon(real.density, col = rgb(1,0,0,1/4), alpha = 0.3)

  
#   simulatePortfolio(logs, stochastic.volatility = FALSE, 
#                     asset.names = c("DBX_USA", "SATRIX_INDI", "RESILIENT", "NEWGOLD"), 
#                     asset.weights = c(0.42, 0.21, 0.16,0.21))
#   
#   simulatePortfolio(logs, stochastic.volatility = TRUE, 
#                     asset.names = c("DBX_USA", "SATRIX_INDI", "RESILIENT", "NEWGOLD"), 
#                     asset.weights = c(0.42, 0.21, 0.16,0.21))
}

