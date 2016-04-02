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
  return(bin2rawhex(bindata))
}


marketData <- function(closes, detrend = FALSE) {
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
  returns[returns > 0] <- 1
  returns[returns < 0] <- 0
  return(bin2rawhex(returns))
}


compress <- function(x, method = "xz") {
  # y <- memCompress(x, method)
  print(length(x))
  y1 <- brotli_compress(x)
  y2 <- memCompress(x, type = "gzip")
  y3 <- memCompress(x, type = "xz")
  y4 <- memCompress(x, type = "bzip2")
  cps <- c(length(y1), length(y2), 
           length(y3), length(y4)) / length(x)
  return(min(cps))
}


monteCarloSim <- function(sims = 1000) {
  benchmarks <- list()
  for (i in 1:sims)
    benchmarks[[i]] <- benchmarkData()
  compressions <- mclapply(benchmarks, compress)
  return(unlist(compressions))
}


run <- function(index = "YAHOO/INDEX_GSPC", years = 5, collapse = "weekly", type = "real") {
  freq <- switch(collapse,
                 weekly = 52,
                 daily = 252)
  
  pricedata <- Quandl(index, rows = -1, collapse = collapse)
  closes <- as.xts(pricedata$Close, order.by = as.Date(pricedata$Date))
  samples.close <- split(closes, ceiling(seq_along(closes) / (freq * years)))
  
  if (type == "real") {
    samples.hex <- lapply(samples.close, marketData)
  } else {
    samples.sims <- as.list(rep(252 * years, length(samples.close)))
    samples.hex <- lapply(samples.sims, benchmarkData)
  }
  
  samples.comp <- lapply(samples.hex, compress)
  compressions <- unlist(samples.comp)
  compressions <- compressions[seq(1, length(compressions) - 1)]
  years.start <- year(pricedata$Date[length(pricedata$Date)])
  years.end <- years.start + ((length(compressions) - 1) * years)
  years.seq <- seq(years.start, years.end, years)
  years.seq <- as.Date(paste(years.seq, "-01-01", sep = ""))
  compressions <- xts(compressions, order.by = years.seq)
  plot(compressions)
  return(compressions)
}


# suppressWarnings(hist(monteCarloSim(sims = 100)))
# suppressWarnings(compress(marketData()))
