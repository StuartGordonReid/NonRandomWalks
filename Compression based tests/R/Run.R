library(BMS)
library(xts)
library(Quandl)
library(brotli)
library(ggplot2)
library(stringr)
library(plyr)
library(parallel)
library(PerformanceAnalytics)


runExperiments <- function() {
  for (algo in c("gzip", "bzip", "xz")) {
    print(paste("Testing compression algorithm:", algo))
    
    # Some liquid market index tests.
    # indices <- getGlobalStockMarketIndices()
    # for (ix in indices)
    #   compressionTest(ix, algo = algo)
    
    # Some liquid currency tests.
    pairs <- getCurrencyPairs()
    for (fx in pairs)
      compressionTest(fx, algo = algo)
  }
}


compressionTest <- function(code, years = 7, algo = "g") {
  # The generic Quandl API key for TuringFinance.
  Quandl.api_key("t6Rn1d5N1W6Qt4jJq_zC")
  
  # Download the raw price data.
  data <- Quandl(code, rows = -1, type = "xts", collapse = "daily")
  
  # Extract the variable we are interested in.
  ix.ac <- which(colnames(data) == "Adjusted Close")
  if (length(ix.ac) == 0) 
    ix.ac <- which(colnames(data) == "Close")
  ix.rate <- which(colnames(data) == "Rate")
  closes <- data[ ,max(ix.ac, ix.rate)]
  
  # Get the month endpoints.
  monthends <- endpoints(closes)
  monthends <- monthends[2:length(monthends) - 1]
  
  # Observed compression ratios.
  cratios <- c()
  for (t in ((12 * years) + 1):length(monthends)) {
    # Extract a window of length equal to years.
    window <- closes[monthends[t - (12 * years)]:monthends[t]]
    # Compute detrended log returns.
    returns <- Return.calculate(window, method = "log")
    returns <- na.omit(returns) - mean(returns, na.rm = T)
    # Binarize the returns.
    returns[returns < 0] <- 0
    returns[returns > 0] <- 1
    # Convert into raw hexadecimal.
    hexrets <- bin2rawhex(returns)
    # Compute the compression ratio
    cratios <- c(cratios, length(memCompress(hexrets)) / 
                   length(hexrets))
  }
  
  # Expected compression ratios.
  ecratios <- c()
  for (i in 1:length(cratios)) {
    # Generate some benchmark returns.
    returns <- rnorm(252 * years)
    # Binarize the returns.
    returns[returns < 0] <- 0
    returns[returns > 0] <- 1
    # Convert into raw hexadecimal.
    hexrets <- bin2rawhex(returns)
    # Compute the compression ratio
    ecratios <- c(ecratios, length(memCompress(hexrets)) / 
                    length(hexrets))
  }
  
  if (mean(cratios) >= min(1.0, mean(ecratios))) {
    print(paste("Dataset:", code, "is not compressible { c =", 
                mean(cratios), "} --> efficient."))
  } else {
    print(paste("Dataset:", code, "is compressible { c =", 
                mean(cratios), "} --> inefficient."))
  }
}


bin2rawhex <- function(bindata) {
  bindata <- as.numeric(as.vector(bindata))
  lbindata <- split(bindata, ceiling(seq_along(bindata)/4))
  hexdata <- as.vector(unlist(mclapply(lbindata, bin2hex)))
  hexdata <- paste(hexdata, sep = "", collapse = "")
  hexdata <- substring(hexdata,
                       seq(1, nchar(hexdata), 2),
                       seq(2, nchar(hexdata), 2))
  return(as.raw(as.hexmode(hexdata)))
}


testRobustness <- function(t = (252 * 7)) {
  # Generate an underlying signal.
  signal <- sin(seq(1, t)) / 50
  signal <- signal - mean(signal)
  
  # For different noise levels
  sds <- seq(0.0, 0.020, 0.0005)
  cratios <- c()
  for (s in sds) {
    # Generate a noisy signal
    noise <- rnorm(t, mean = 0, sd = s)
    returns <- signal + noise
    # Binarize the data.
    returns[returns < 0] <- 0
    returns[returns > 0] <- 1
    # Convert into raw hexadecimal.
    hexrets <- bin2rawhex(returns)
    # Compute the compression ratio
    cratios <- c(cratios, length(memCompress(hexrets)) / 
                   length(hexrets))
    
    # Plot the returns for the eye ball test.
    dates <- seq.Date(Sys.Date(), Sys.Date() + (252 * 7) - 1, 1)
    # plot.ts(xts(signal + noise, order.by = dates))
    
    if (s %in% c(0.010, 0.015, 0.020)) {
      charts.PerformanceSummary(xts(signal + noise, order.by = dates))
    }
  }
  
  # Plot the compression ratios.
  plot(sds, cratios)
}


bettingStrategyProof <- function(t = (252 * 7), w = 5, sd = 0.02) {
  # Generate an underlying signal.
  signal <- sin(seq(1, t)) / 50
  signal <- signal - mean(signal)
  
  # Add some noise to get a return series.
  returns <- signal + rnorm(t, mean = 0.0, sd = sd)
  
  # On your marks. Place your bets.
  weights <- c(1.0)
  for (t in (w + 1):length(returns)) {
    cumret <- prod(1 + returns[(t-w):(t-1)]) - 1
    # Determine if we are risk-on or risk-off:
    if (cumret < 0) weights <- c(weights, 1.0)
    else weights <- c(weights, -1.0)
  }
  
  # Turn our hypothetical returns into time series.
  rets <- returns[w:length(returns)]
  rets.weighted <- rets * weights
  dates <- dates <- seq.Date(Sys.Date(), Sys.Date() + 
                               length(rets) - 1, 1)
  
  # Plott he resulting "equity curves".
  charts.PerformanceSummary(xts(rets, order.by = dates))
  charts.PerformanceSummary(xts(rets.weighted, order.by = dates))
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


getCurrencyPairs <- function() {
  pairs <- c()
  for (fx1 in c("USD", "EUR", "GBP")) {
    for (fx2 in c("ZAR", "BRL", "RUB")) {
      pair <- paste(fx1, fx2, sep = "")
      pairs <- c(pairs, paste("CURRFX", pair, sep = "/"))
    }
  }
  pairs <- c(pairs, "CURRFX/USDGBP", "CURRFX/USDEUR", "CURRFX/GBPEUR")
  return(pairs)
}

