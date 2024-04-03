p.bar.plt <- function(x, main = NULL, v = NULL){ # Bar Plot of Stocks Returns
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x)%/%3+1)] # Data
  
  for (n in 1:ncol(x)){ s <- x[,n] # Clean data, calculate logs and returns
  
    security <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    v <- cbind(v, (exp(sum(security)) - 1) * 100) } # Add value to Data Frame
    
  colnames(v) <- colnames(x) # Give column names
  
  # Sort values for column names and move names to new variable 
  p.tickers <- colnames(sort(as.data.frame(v), decreasing = T))
  
  v <- sort(as.numeric(v), decreasing = T) # Make data numeric and sort values
  
  mx <- ceiling(round(max(v)) / 10 ^ (nchar(round(max(v))) - 1)) *
    10 ^ (nchar(round(max(v))) - 1) # Round maximum value up
  
  if (min(v) < 0){ # Round minimum value down
    
    if (any(seq(9) == as.numeric(strsplit(as.character(min(v)),"")[[1]][3]))){
      
      mn <- (as.numeric(strsplit(as.character(min(v)),"")[[1]][2]) + 1) * -10 }
  }
  # Bar Plot with securities returns
  plt <- barplot(v, names.arg = p.tickers, horiz = T, xlab = "Returns (%)",
                 main = "Performance of Portfolio Securities Returns",
                 xlim = c(mn, mx),  las = 1,
                 col=c(rep("green4",length(v)-sum(v<0)),rep("red3",sum(v<0)))) 
  
  # Add vertical (returns) and horizontal (through bars) grey dotted lines
  abline(h = plt, col = "grey", lty = 3) 
  for (n in seq(mn, mx, by = 10)){ abline(v = n, col = "grey", lty = 3) } 
  
  axis(side = 1, at = seq(mn, mx, by = 10), las = 1) # configure x axis
  
  box() # Make borders for plot
}
p.bar.plt(df_portfolio) # Test
