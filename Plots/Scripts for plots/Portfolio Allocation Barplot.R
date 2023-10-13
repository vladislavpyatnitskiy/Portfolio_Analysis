# Function to plot portfolio weights via barplot
weights_brplt <- function(x, main = "Portfolio", col = "blue"){
  
  # Subset tickers from price columns
  tickers <- colnames((x[,1 + 3 * seq(31, from = 0)]))
  
  # Calculate portions
  pct <- as.data.frame(x[,3 + 3 * seq(31, from = 0)]) / x[,ncol(x)]
  
  # Select last period and transform portions into percentages
  pct <- as.numeric(((pct[nrow(pct),] * 100)))
  
   # Create barplot
  barplot(pct,
          names.arg = tickers,
          horiz = F,
          col = col,
          main = main,
          ylab = "Percentage (%)",
          ylim = c(0, round(max(pct)) + 0.5),
          las = 2 # Turn tickers 90º left
  )
  # Y axis
  axis(side=2, at=seq(0, round(max(pct)), 0.5), las = 1,
       seq(0.0, round(max(pct),1), 0.5))
  
  # Add grey lines for fast visual percentage calculation
  for (n in seq(0, round(max(pct)), 0.5)){ abline(h = n, col = "grey",
                                                  lty = 2) }
  
  # Add line for mean percentage
  abline(h = mean(pct), col = "red", lwd = 3)
  
  # Add line for median percentage
  abline(h = median(pct), col = "green", lwd = 3)

  # Add legend with mean and median
  legend("topright", legend = c((paste("Mean:",mean(pct))), paste("Median:",
                                                      round(median(pct),3))),
         fill = c("red", "green"), cex = 0.75)
  
  # Set up borders
  box()
}
# Test
weights_brplt(df_portfolio, main = "Portfolio Allocation")
