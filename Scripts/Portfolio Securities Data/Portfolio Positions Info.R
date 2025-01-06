p.position.info.full <- function(x){ # Info of Total Sum for each Stock
  
  s = x[,1+3*seq(ncol(x)%/%3,from=0)][nrow(x),] # Price, Number & Sum per Stock
  q = t(as.data.frame(x[,2 + 3 * seq(ncol(x) %/% 3 - 1, from = 0)][nrow(x),]))
  P = t(as.data.frame(x[,3 + 3 * seq(ncol(x) %/% 3 - 1, from = 0)][nrow(x),]))
  
  D <- cbind.data.frame(as.data.frame(s[-length(s)]), q, P) # Join
  
  colnames(D) <- c("Price", "Quantity", "Total Sum") # Column & Row names
  rownames(D) <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)]) 
  
  D # Display
}
p.position.info.full(df_portfolio) # Test
