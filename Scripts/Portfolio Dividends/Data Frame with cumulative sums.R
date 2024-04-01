p.dividends.cum <- function(x){ # Data Frame with cumulative dividends
  
  s.names <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  x <- cumsum(x[,3 * seq(ncol(x) %/% 3, from = 1)]) # Cumulative total sum
  
  colnames(x) <- s.names # Assign tickers for columns
  
  x <- cbind(x, as.timeSeries(rowSums(x, na.rm = T))) # Sum dividends
  
  colnames(x)[ncol(x)] <- "Total" # Column name for cumulative sum
  
  x <- x[,colSums(x) !=0] # Subtract column of securities without dividends
  
  x # Display
}
p.dividends.cum(df_portfolio_dividend) # Test
