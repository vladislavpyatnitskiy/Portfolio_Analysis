p.drawdown.plt <- function(x, SD = T){ # Drawdown plot for portfolio
  
  if (isFALSE(SD)){ r <- rownames(x) # Save dates for Cumulative Returns
  
    x <- apply(x, 2, function(col) exp(cumsum(col)) - 1) # Cumulative Returns
    
    rownames(x) <- r } # Return dates as row names as they were vanished 
    
  x <- x * 100 # Multiply returns by 100
  
  x[x > 0] <- 0 # Replace positive values as 0
  
  for (n in 1:ncol(x)){ s <- x[,n] # Plot column in data frame
  
    if (isTRUE(SD)){ m <- "Drawdown Plot of %s Fluctuations" } else {
      
      m <- "Drawdown Plot of %s" } # Title depending on SD
    
    plot(s, col = "red", ylim = c(min(s), 0), type = "l", xlab = "Trading Days",
         main = sprintf(m, colnames(s)), las = 2, ylab = "Negative Returns (%)")
    
  abline(h = 0) } # Add horizontal line at break even
  
  par(mar = c(5, 5, 5, 5)) # Define borders of the plot
  
  grid(nx = 1, ny = NULL, col = "grey", lty = "dotted", lwd = 1) 
  
  axis(side = 4, las = 2)
}
p.drawdown.plt(returns_df, SD = F) # Test
