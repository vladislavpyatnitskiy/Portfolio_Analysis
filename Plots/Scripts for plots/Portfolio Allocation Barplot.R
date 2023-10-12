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
          las = 2 # Turn tickers 90º left
  )
  # Y axis
  axis(side=2, at=seq(0, round(max(pct)), by = 1), las = 1)
  
  # Add grey lines for fast visual percentage calculation
  for (n in 0:round(max(pct))){ abline(h = n, col = "grey") }
  
  # Add line for mean percentage
  abline(h = mean(pct), col = "red", lwd = 3)
  
  # Add line for median percentage
  abline(h = median(pct), col = "green", lwd = 3)
}
# Test
weights_brplt(df_portfolio, main = "Portfolio Allocation")
