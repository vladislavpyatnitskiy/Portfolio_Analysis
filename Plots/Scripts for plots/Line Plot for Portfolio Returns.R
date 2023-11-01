# Return Plot
p.plt <- function(x){ x <- apply(x, 2, function(col) (exp(cumsum(col))-1))
  
  plot(x,main = "Portfolio Performance",xlab = "Trading Days",ylab = "Return",
       type="l", las=1) # Plot
  
  abline(h = 0, lwd = 3, col = "black") # Break Even
  
  abline(h = x[nrow(x),], col = "red", lwd = 2) # Current Return
  
  axis(side = 2, at=seq(-1, 1, .01), las = 1) # y axis
  
  for (n in seq(-1, 1, .01)){ abline(h = n,col = "grey",lty = 3) } # grey lines
}
# Test
p.plt(returns_df)
