p.plt <- function(x, SD = F){ if (isFALSE(SD)){ # Return Plot
  
    x <- apply(x, 2, function(col) (exp(cumsum(col)) - 1) * 100) # Return
    
    if (x[nrow(x),] > 0){ plot(x, main = "Portfolio Performance", type="l",
                               las = 1, xlab = "Trading Days", lwd = 3,
                               ylab = "Return (%)", col = "green4")
      
    } else { plot(x, main = "Portfolio Performance", xlab = "Trading Days", 
                  ylab="Return (%)", type="l", las = 1, col = "red3",
                  lwd = 3) }
    
    abline(h = x[nrow(x),], col = "navy", lwd = 1) # Current Return
    
    for (n in c(-99,-98,-97,-96)){ axis(side = 2, at=seq(n,max(x),5),las=1) }
    
      } else { y <- x * 100
    
    plot(y, main = "Portfolio Return Volatility", xlab="Trading Days",las=1,
         ylab = "Fluctuations (%)", ylim = c(signif(min(y)),signif(max(y))),
         col = "black", type = "l")  
    
    for (n in c(-99)){ axis(side = 2, at = seq(n, max(y), 2), las = 1) } }
    
  abline(h = 0) # Break Even
  for (n in seq(-100, -1, 1)){ abline(h = n, col = "grey", lty = 3) }
  for (n in seq(1, 100, 1)){ abline(h = n, col = "grey", lty = 3) }
}
p.plt(returns_df, SD = F) # Test
