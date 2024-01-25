p.weights <- function(x){ # Get Portfolio Weight Values
  
  a.names <- colnames(x[,1 + 3 * seq(31, from = 0)]) # Assign tickers
  
  # Take values of last observations
  p.weights <- x[,3+3*seq(31,from=0)][nrow(x),]/as.numeric(x[nrow(x),ncol(x)])
  
  p.weights <- round(t(as.data.frame(p.weights)) * 100, 2) # Values
  
  rownames(p.weights) <- a.names # Assign tickers to this data frame
  colnames(p.weights) <- "Weights (%)" 
  
  p.weights # Display
}
p.weights(df_portfolio) # Test
