source(file = "./VarianceRatioTests.R")


getPriceData <- function(filename = "jse.csv") {
  dataset <- read.csv(paste("../Stock Market Datasets/", filename, sep = ''))
  dates <- as.POSIXct(dataset[ ,"Date"])
  dataset <- dataset[, !(colnames(dataset) %in% c("Date"))]
  return(xts(dataset, order.by = dates))
}


getLogPriceData <- function(filename = "jse.csv") {
  dataset <- getPriceData(filename)
  return(log(dataset))
}


simulateTS <- function(asset.ts, starting = FALSE) {
  asset.ts <- na.omit(asset.ts)
  
  calibration <- calibrate(asset.ts, 2)
  mu.est <- calibration[[1]]
  sd.est <- calibration[[2]]
  
  print(paste(mu.est, sd.est))
  
  if (starting) start <- exp(asset.ts[nrow(asset.ts)][[1]])
  else start = 1.0

  paths <- priceProcesses(n = 700, X0 = start, t = (252), mu = mu.est, rd.sigma = sd.est)
  
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


simulateAsset <- function(logPrices, asset.name = "SEPHAKU") {
  asset.ts <- logPrices[ ,c(asset.name)]
  simulateTS(asset.ts)
}


simulatePortfolio <- function(logPrices, asset.names, asset.weights) {
  asset.ts <- logPrices[ ,c(asset.names[1])] * asset.weights[1]
  
  if (length(asset.names) >= 2) {
    for (i in 2:length(asset.names))
      asset.ts <- asset.ts + logPrices[ ,c(asset.names[i])] * asset.weights[i]
  }

  simulateTS(asset.ts)
}


analysis <- function() {
  logs <- getLogPriceData()
  
  simulateAsset(logs, "DBX_USA")
  simulateAsset(logs, "SATRIX_INDI")
  simulateAsset(logs, "RESILIENT")
  simulateAsset(logs, "NEWGOLD")
  
  simulatePortfolio(logs, c("DBX_USA", "SATRIX_INDI", "RESILIENT", "NEWGOLD"), c(0.42, 0.21, 0.16,0.21))
}

