lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

p.portion <- function(x, y, q = 1){ # potential security portion in portfolio
  
  p <- getSymbols(x, from=as.Date(Sys.Date()), src="yahoo", auto.assign=F)[,4]
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  p <- as.timeSeries(p) # Make it time series
  
  p <- round(as.numeric(p * q / (as.numeric(y[nrow(y),ncol(y)]) + p * q)), 4)
  
  p # Display
}
p.portion("X", df_portfolio, q = 1) # Test
