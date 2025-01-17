p.weights <- function(x){ # Portfolio Weight Values
  
  # Take values of last observations and divide by the total sum
  w <- x[,3*seq(ncol(x)%/%3,from=1)][nrow(x),] / as.numeric(x[nrow(x),ncol(x)])
  w <- round(t(as.data.frame(w)) * 100, 2)
  
  rownames(w) <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)])
  colnames(w) <- "Weights (%)" 
  
  w # Display
}
p.weights(df_portfolio) # Test
