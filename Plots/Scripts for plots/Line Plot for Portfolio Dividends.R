p.plt.dividends <- function(x, N){ # Plot showing Dividend Accumulation
  
  x <- cumsum(x[,ncol(x)]) # Calculate cumulative sum of dividends
  
  plot(x, main = "Dividend Amount Accumulation (in US$)", col = "red", las = 1,
       ylab = "", xlab = "Trading Days", sub = "Data Source: Yahoo! Finance",
       lwd = 3)
  
  par(mar = c(5, 3.5, 5, 3.5)) # Define borders of the plot
  
  abline(h = 0) # Add black horizontal line showing 0 and grey line for others
  abline(h = seq(x[nrow(x),], from = N, by = N), col = "grey", lty = 3)
  
  axis(side = 2, at = seq(x[nrow(x),], from = N, by = N * 2), las = 2) # Axes
  axis(side = 4, at = seq(x[nrow(x),], from = 0, by = N), las = 2) 
}
p.plt.dividends(df_portfolio_dividend, N = 10) # Test
