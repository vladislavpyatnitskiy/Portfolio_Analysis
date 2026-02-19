p.plt.dividends <- function(x, c = "$US", sub = NULL){ # Dividend Accumulation
  
  x <- cumsum(x[,ncol(x)]) # Calculate cumulative sum of dividends

  par(mar = c(5, 3.5, 5, 3.5)) # Define borders of the plot
  
  plot(x, main = sprintf("Dividend Amount Accumulation (in %s)", c), lwd = 3,
       col="red", las=1, ylab = "", xlab = "Trading Days", sub=sub, type = "l")
  
  abline(h = 0) # Add black horizontal line showing 0 and grey line for others
  
  grid(nx = 1, ny = NULL, col = "grey", lty = "dotted", lwd=1) # grid lines
  
  axis(side = 4, las = 2) # Right y-axis
}
p.plt.dividends(df_portfolio_dividend, sub = "Data Source: Yahoo! Finance")
