p.position.info.full <- function(x){ # Info of Total Sum for each Stock
  
  # Tickers
  d <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,1:(ncol(x) %/% 3)])
  
  s <- x[,1 + 3*seq(ncol(x)%/%3, from=0)][nrow(x),] # Price per Stock
  
  # Number of each Stock
  q <- t(as.data.frame(x[,2 + 3 * seq(ncol(x) %/% 3 - 1, from = 0)][nrow(x),]))
  
  s <- as.data.frame(s[-length(s)]) # Reduce total sum of all securities
  
  # Total sum for each security
  P <- t(as.data.frame(x[,3 + 3 * seq(ncol(x) %/% 3 - 1, from = 0)][nrow(x),]))
  
  rownames(s) <- d # Give common row names as tickers to join data
  rownames(q) <- d
  rownames(P) <- d
  
  D <- cbind.data.frame(s, q, P) # Join
  
  colnames(D) <- c("Price", "Quantity", "Total Sum") # Column names
  
  D # Display
}
p.position.info.full(df_portfolio) # test
