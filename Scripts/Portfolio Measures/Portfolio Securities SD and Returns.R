p.sd.return <- function(x){ # Data Frame with Standard Deviations and Returns
  
  A <- x[,1 + 3*seq(ncol(x) %/% 3, from = 0)][,1:(ncol(x)%/%3)] # Securities
  
  d <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(A)){ s <- A[,n] # For each security in data frame
  
    # Clean data to reduce NA and calculate return for ownership period  
    j <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    d <- rbind.data.frame(d, cbind(sd(j)*1000, (exp(sum(j))-1)*100)) } # Join
    
  rownames(d) <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)])
  colnames(d) <- c("Standard Deviation", "Return") # Column names
  
  d # Display
}
p.sd.return(df_portfolio) # Test
