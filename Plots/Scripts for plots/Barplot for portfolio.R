# Function to generate barplot
p.bar.plt <- function(x, main = NULL, col = "blue", v = NULL){
  
  x <- x[,1 + 3 * seq(31, from = 0)] # Subset values with secuirities' values
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column in data set
  
    # Clean data from NA & zeros and calculate logs
    security <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    v <- cbind(v, exp(sum(security)) - 1) } # Calculate return
    
  colnames(v) <- colnames(x) # Give column names
  
  # Sort values for column names and move names to new variable 
  p.tickers <- colnames(sort(as.data.frame(v), decreasing = T))
  
  v <- sort(as.numeric(v), decreasing = T) # Make data numeric and sort values
  
  # Create barplot
  plt <- barplot(v, names.arg = p.tickers, horiz = T, las= 1, col = col,
                 main = main, xlab = "Returns", ylab = "",
                 xlim = c(round(min(v), 1), round(max(v), 1)))
  
  # Add grey dotted lines
  abline(h = plt, col = "grey",lty = 3) # through bars
  
  p.seq <- seq(round(min(v), 1), round(max(v), 1), by = .1)
  
  for (n in p.seq){ abline(v = n, col = "grey", lty = 3) } # vertical bars
  
  axis(side = 1, at = p.seq, las = 1) # configure x axis
  
  box() # Make borders for plot
}
p.bar.plt(df_portfolio, main = "Securities Performance") # Test
