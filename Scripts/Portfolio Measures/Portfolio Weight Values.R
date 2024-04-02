p.weights <- function(x){ # Get Portfolio Weight Values
  
  tickers <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)])
  
  # Take values of last observations and divide by the total sum
  w <- round(t(as.data.frame(x[,3*seq(ncol(x)%/%3,from=1)][nrow(x),]/
                               as.numeric(x[nrow(x),ncol(x)]))) * 100, 2)
  
  rownames(w) <- tickers # Assign tickers to this data frame
  colnames(w) <- "Weights (%)" 
  
  w # Display
}
p.weights(df_portfolio) # Test
