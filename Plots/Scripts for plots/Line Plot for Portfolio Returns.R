library("timeSeries") # Library

p.plt <- function(x, SD = F){ if (isFALSE(SD)){ # Return Plot
  
  x <- apply(x, 2, function(col) (exp(cumsum(col)) - 1) * 100) # Return
  
  if (x[nrow(x),] > 0){ plot(x, main = "Returns of Portfolio Securities",
                             las = 1, xlab = "Trading Days", col = "green4",
                             ylab = "Return (%)", lwd = 3, type="l")
    
  } else { plot(x, main = "Returns of Portfolio Securities", ylab="Return (%)",
                xlab = "Trading Days", type="l", las=1, col = "red3", lwd=3) }
  
  par(mar = c(5, 5, 5, 5)) # Define borders of the plot
  
  m <- round(min(x) * -1 + max(x), 0)
  
  m <- m / 10 ^ (nchar(m) - 1)
  
  if (m > 0 && m < 1){ mn <- 1 * 10 ^ (nchar(m) - 3) }
  
  else if (m > 1 && m < 2){ mn <- 2 * 10 ^ (nchar(m) - 3) }
  
  else if (m > 2 && m < 5){ mn <- 5 * 10 ^ (nchar(m) - 3) }
  
  axis(side = 4, las = 1, at = seq(-100, 100, mn)) # Axes
  
  abline(h = x[nrow(x),], col = "navy", lwd = 2) # Current Return
  
  } else { y <- x * 100
  
    plot(y, main = "Volatility of Portfolio Returns", ylab = "Fluctuations (%)",
         ylim = c(signif(min(y)), signif(max(y))), col = "red", type = "l",
         xlab = "Trading Days", las = 1)  
    
    axis(side = 4, at = seq(-100, 100, 1), las = 1)
    
    for (n in c(-99)){ axis(side = 2, at = seq(n, max(y), 1), las = 1)
      
      mn = 1 } }
    
  abline(h = 0) # Break Even
  for (n in seq(-100, -mn, mn)){ abline(h = n, col = "grey", lty = 3) }
  for (n in seq(mn, 100, mn)){ abline(h = n, col = "grey", lty = 3) }
}
p.plt(returns_df, SD = F) # Test
