library(timeSeries) # Library

p.dividends.cum <- function(x){ # Data Frame with cumulative dividends
  
  S <- cumsum(x[,3 * seq(ncol(x) %/% 3, from = 1)]) # Cumulative total sum
  
  colnames(S) <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  S <- cbind(S, as.timeSeries(rowSums(S, na.rm = T))) # Sum dividends
  
  colnames(S)[ncol(S)] <- "Total" # Column name for cumulative sum
  
  S[,colSums(S) !=0] # Subtract column of securities without dividends
}
p.dividends.cum(df_portfolio_dividend) # Test
