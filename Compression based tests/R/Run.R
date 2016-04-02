library(BMS)
library(xts)
library(Quandl)
library(brotli)
library(ggplot2)
library(stringr)
library(plyr)
library(PerformanceAnalytics)


bin2rawhex <- function(bindata) {
  lbindata <- split(bindata, ceiling(seq_along(bindata)/4))
  hexdata <- as.vector(unlist(mclapply(lbindata, bin2hex)))
  hexdata <- paste(hexdata, sep = "", collapse = "")
  hexdata <- substring(hexdata,
                       seq(1, nchar(hexdata), 2),
                       seq(2, nchar(hexdata), 2))
  return(as.raw(as.hexmode(hexdata)))
}


benchmarkData <- function(kDays) {
  bindata <- rbinom(kDays, 1, 0.5)
  # plotBinData(bindata)
  return(bin2rawhex(bindata))
}


marketData <- function(closes, detrend = TRUE) {
  if (detrend) {
    logreturns <- Return.calculate(closes, method = "log")
    logreturns <- logreturns - mean(logreturns, na.rm = TRUE)
    returns <- exp(logreturns) - 1
  } else {
    returns <- Return.calculate(closes)
  }

  charts.PerformanceSummary(returns)
  returns <- as.numeric(as.vector(returns))
  returns[is.nan(returns)] <- NA
  returns[is.infinite(returns)] <- NA
  returns[returns == 0.0] <- NA
  returns <- na.omit(returns)
  
  bindata <- returns
  bindata[bindata > 0] <- 1
  bindata[bindata < 0] <- 0
  # plotBinData(bindata)
  return(bin2rawhex(bindata))
}


plotBinData <- function(bindata) {
  bindata[bindata == 0] <- -1
  plot.ts(cumsum(bindata))
}


compress <- function(x, method = "b") {
  y <- switch(method,
              g = Rcompression::gzip(x),
              b = brotli_compress(x))
  return(length(y) / length(x))
}


monteCarloSim <- function(kDays, sims = 30) {
  benchmarks <- list()
  for (i in 1:sims)
    benchmarks[[i]] <- benchmarkData(kDays)
  compressions <- mclapply(benchmarks, compress)
  return(unlist(compressions))
}


run <- function(index = "YAHOO/INDEX_GSPC", years = 5, collapse = "daily", type = "real") {
  # Download the data from Quandl.com
  pricedata <- Quandl(index, rows = -1, collapse = collapse, 
                      end_date = "2015-12-31")
  # Extract the dates as a POSIX variable.
  dates <- as.Date(pricedata$"Date")
  
  # Extract the relevant time series data.
  if (length(grep("YAHOO", index)) > 0)
    ts <- as.xts(pricedata$"Adjusted Close", order.by = dates)
  else if (length(grep("CURRFX", index)) > 0)
    ts <- as.xts(pricedata$"Rate", order.by = dates)
  else # Assume we want to use the second column.
    ts <- as.xts(pricedata[ ,2], order.by = dates)
  
  # Get the split positions.
  pos <- endpoints(ts, on = "years") + 1
  pos <- pos[seq(1, length(pos) - 1)]
  
  # If we have enough sub samples.
  if (ceiling(length(pos) / years) > 3) {
    pos <- pos[seq(years, length(pos), years)]
    ts.subs <- splitAt(ts, pos)
    ts.subs.dates <- c(index(ts.subs[[1]])[1])
    for (i in 2:length(ts.subs))
      ts.subs.dates <- c(ts.subs.dates, index(ts.subs[[i]])[1])
    ts.hexdata <- lapply(ts.subs, marketData)
    ts.compressions <- unlist(lapply(ts.hexdata, compress))
    ts.xts <- xts(ts.compressions[seq(2, length(ts.compressions))], 
                  order.by = as.Date(ts.subs.dates[seq(2, length(ts.subs.dates))]))
    plot(ts.xts, main = "Subsequence Compression Ratios")
  } else {
    print(paste("Compression Achieved:", compress(marketData(ts))))
  }
  
  # Plot the mean compression on random data.
  mc.means <- c()
  for (sub in ts.subs)
    mc.means <- c(mc.means, mean(monteCarloSim(length(sub))))
  mc.xts <- xts(mc.means[seq(2, length(mc.means))], 
                order.by = as.Date(ts.subs.dates[seq(2, length(ts.subs.dates))]))
  lines(mc.xts, col = "red")
}


splitAt <- function(x, pos) {
  unname(split(x, cumsum(seq_along(x) %in% pos)))
}


# suppressWarnings(hist(monteCarloSim(sims = 100)))
# suppressWarnings(compress(marketData()))
