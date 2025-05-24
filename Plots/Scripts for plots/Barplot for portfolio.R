p.bar.plt <- function(x){ v = NULL # Bar Plot of Stocks Returns
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x) %/% 3 + 1)] # Data
  
  for (n in 1:ncol(x)){ s <- x[,n] # Clean data, calculate logs and returns
  
    security <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    v <- c(v, (exp(sum(security)) - 1) * 100) } # Add value to Data Frame
    
  names(v) <- colnames(x) # Give column names and sort returns
  
  v <- sort(v, decreasing = T) # Make data numeric and sort values
  
  B <- barplot(
    v,
    xlim = c(min(v) - 1, max(v) + 1),
    main = "Returns of Portfolio Securities (%)",
    las = 1,
    col = ifelse(v < 0, "red3", "green4"),
    horiz = T
    ) # Bar Plot
  
  abline(h = B, col = "grey", lty = 3) # Horizontal lines
  grid(nx = NULL, ny = 1, col = "grey", lwd = 1) # Vertical lines
  abline(v = 0) # Break Even line
  
  axis(side = 4, at = B, labels = names(v), las = 1,tick = F, line = -0.5)
  
  par(mar = rep(4, 4)) # Margins
  
  box() # Make borders for plot
}
p.bar.plt(df_portfolio) # Test
