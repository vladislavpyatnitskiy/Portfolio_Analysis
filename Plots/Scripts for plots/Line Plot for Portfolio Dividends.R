p.plt.dividends <- function(x, c = "$US", sub = NULL){ # Dividend Accumulation
  
  x <- cumsum(x[,ncol(x)]) # Calculate cumulative sum of dividends
  
  plot(x, main = sprintf("Dividend Amount Accumulation (in %s)", c), lwd = 3,
       col="red", las=1, ylab = "", xlab = "Trading Days", sub=sub, type = "l")
  
  par(mar = c(5, 3.5, 5, 3.5)) # Define borders of the plot
  
  m <- round(max(x), 0) / 10 ^ (nchar(round(max(x), 0)) - 1) # Axes Interval
  
  if (m > 0 && m < 1){ mn <- 1 * 10 ^ (nchar(m) - 3) }
  
  else if (m > 1 && m < 2){ mn <- 2 * 10 ^ (nchar(m) - 3) }
  
  else if (m > 2 && m < 5){ mn <- 5 * 10 ^ (nchar(m) - 3) }
  
  abline(h = 0) # Add black horizontal line showing 0 and grey line for others
  abline(h = seq(round(max(x), 0), from = mn/2, by = mn/2), col="grey", lty=3)
  
  axis(side = 2, at = seq(round(max(x), 0),  from = mn / 2, by = mn), las = 2) 
  axis(side = 4, las = 1, at = seq(0, round(max(x), 0), mn / 2)) # Both Axes
}
p.plt.dividends(df_portfolio_dividend, sub = "Data Source: Yahoo! Finance")
