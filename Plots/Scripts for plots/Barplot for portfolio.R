p.bar.plt <- function(x){ v = NULL # Bar Plot of Stocks Returns
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x) %/% 3 + 1)] # Data
  
  for (n in 1:ncol(x)){ s <- x[,n] # Clean data, calculate logs and returns
    
    security <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    v <- cbind(v, (exp(sum(security)) - 1) * 100) } # Add value to Data Frame
    
  colnames(v) <- colnames(x) # Give column names and sort returns
  
  tickers <- colnames(sort(as.data.frame(v), decreasing = T))
  v <- sort(as.numeric(v), decreasing = T) # Make data numeric and sort values
  
  B <- barplot(v, names.arg = tickers, xlim = c(min(v) - 1, max(v) + 1),
               main = "Returns of Portfolio Securities (%)", las = 1,
               col = ifelse(v < 0, "red3", "green4"), horiz = T) # Bar Plot
  
  abline(h = B, col = "grey", lty = 3) # Horizontal and Vertical lines
  grid(nx = NULL, ny = 1, col = "grey", lwd = 1)
  abline(v = 0) # Break Even line
  
  box() # Make borders for plot
}
p.bar.plt(df_portfolio) # Test
