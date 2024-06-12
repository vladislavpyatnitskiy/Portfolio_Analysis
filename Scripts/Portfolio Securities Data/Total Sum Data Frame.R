p.position.info <- function(x){ # Info of Total Sum for each Stock
  
  # Tickers
  d <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,1:(ncol(x) %/% 3)])
  
  # Total sum for each security
  D <- t(as.data.frame(x[,3*seq(ncol(x)%/%3, from=1)][nrow(x),]))
  
  rownames(D) <- d
  colnames(D) <- "Total"
  
  D # Display
}
p.position.info(df_portfolio) # test
