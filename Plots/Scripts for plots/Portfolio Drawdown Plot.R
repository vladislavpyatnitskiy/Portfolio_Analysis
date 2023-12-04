p.drawdown.plt <- function(x){ # Drawdown plot for portfolio
  
  x <- apply(x,2,function(col) (exp(cumsum(col))-1) * 100) # Cumulative return
  x[x > 0] <- 0 # Reduce postive values
  
  # For each column in data frame
  for (n in 1:ncol(x)){ security <- x[,n]
  
    # Plot
    plot(security,
         type = "l",
         main = sprintf("%s Drawdown", colnames(security)),
         xlab = "Trading Days",
         ylab = "Negative Return (%)",
         las = 2,
         ylim = c(signif(min(security), 1), 0)) }
  
  # Add horizontal lines
  abline(h = seq(signif(min(security), 1), 0, 1), lty = 3, col = "grey")
}
p.drawdown.plt(returns_df) # Test
