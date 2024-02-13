p.plt.dividends <- function(x, N){ # Plot showing Dividend Accumulation
  
  x <- cumsum(x[,ncol(x)]) # Calculate cumulative sum of dividends
  
  plot(x, main = "Dividend Amount Accumulation", col="red",xlab="Trading Days",
       sub="Data Source: Yahoo! Finance",lwd=3,las=1, ylab = "Amount in USD$")
  
  abline(h = 0) # Add black horizontal line showing 0 and grey line for others
  abline(h = seq(x[nrow(x),], from = N, by = N), col = "grey", lty = 3)
  
  axis(side = 2, at = seq(x[nrow(x),], from = N, by = N * 2), las = 2) # Axis
}
p.plt.dividends(df_portfolio_dividend, N = 10) # Test
