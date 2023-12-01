# Return Plot
p.plt <- function(x, SD = F){ if (isFALSE(SD)){
  
  x <- apply(x,2,function(col) (exp(cumsum(col))-1) * 100)

  plot(x,main="Portfolio Performance",xlab="Trading Days",ylab = "Return (%)",
       type="l", las=1) # Plot
  
  abline(h = 0, lwd = 3, col = "black") # Break Even
  
  abline(h = x[nrow(x),], col = "red", lwd = 2) # Current Return
  
  axis(side = 2, at=seq(-100, 100, 1), las = 1) # y axis
  
  for (n in seq(-100,100,1)){ abline(h=n,col="grey",lty=3) } } else { y<-x*100
    
    plot(y,main="Portfolio Volatility",xlab="Trading Days",type="l",las=1,
         ylab="Fluctuations (%)", ylim = c(signif(min(y)), signif(max(y))))  
    
    abline(h = 0, lwd = 3, col = "black") # Break Even
    
    axis(side = 2, at=seq(-100, 100, 1), las = 1) # y axis
    
    for (n in seq(-100,100,1)){ abline(h = n,col = "grey",lty = 3) } }
}
p.plt(returns_df, SD = T) # Test
