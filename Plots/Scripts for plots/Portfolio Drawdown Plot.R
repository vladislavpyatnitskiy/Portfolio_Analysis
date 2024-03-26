p.drawdown.plt <- function(x, SD = T){ # Drawdown plot for portfolio
  
    if (isFALSE(SD)){ r <- rownames(x) # Save dates for Cumulative Returns
    
    x <- apply(x, 2, function(col) exp(cumsum(col)) - 1) # Cumulative Returns
    
    rownames(x) <- r } # Return dates as row names as they were vanished 
    
    x <- x * 100 # Multiply returns by 100
    
    x[x > 0] <- 0 # Replace positive values as 0
    
    for (n in 1:ncol(x)){ security <- x[,n] # Plot column in data frame
    
    if (isTRUE(SD)){ m <- "Drawdown Plot of %s Fluctuations" } else {
      
      m <- "Drawdown Plot of Cumulative %s" } # Title depending on SD
    
    plot(security, col = "red", ylim = c(min(security), 0), type = "l",
         main = sprintf(m, colnames(security)), las = 2, xlab = "Trading Days",
         ylab = "Negative Returns (%)")
    
    abline(h = 0) # Add horizontal line at break even
    
    if (abs(min(security)) < 10){ # Add grey horizontal dotted lines
      
      abline(h = seq(-10, -1, 1), lty = 3, col = "grey") } # 2
    
    else if (abs(min(security)) < 15){ # Add grey horizontal dotted lines
      
      abline(h = seq(-20, -2, 2), lty = 3, col = "grey") } # 2
    
    else if (abs(min(security)) > 15 && abs(min(security)) < 45){ # 5
      
      abline(h = seq(-50, -5, 5), lty = 3, col = "grey") }
    
    else if (abs(min(security)) > 45 && abs(min(security)) < 90){ # 10
      
      abline(h = seq(-100, -10, 10), lty = 3, col = "grey") } }
    
    par(mar = c(5, 5, 5, 5)) # Define borders of the plot
    
    axis(side = 4, at = sort(seq(100, from=0, by=1) * -1, decreasing=T), las=1)
}
p.drawdown.plt(returns_df, SD = T) # Test
