p.plt.dividends <- function(x){ # Plot showing Dividend Accumulation
  
  x <- cumsum(x[,ncol(x)]) # Calculate cumulative sum of dividends
  
  plot(x, main = "Dividend Accumulation", sub = "Data Source: Yahoo! Finance",
       xlab = "Trading Days", las = 1, ylab = "Amount in USD$")
}
p.plt.dividends(df_portfolio_dividend) # Test
