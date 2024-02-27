p.volatility.plt <- function(x, abs = F){ # Plot Security volatility
  
  x <- x * 100 # Multiply by 100
  
  if (isFALSE(abs)){ # Choose between real and absolute values of returns
    
    for (n in 1:ncol(x)){ # Plot volatility with positive and negative returns
      
      plot(x[,n], col = "red", ylab = "Returns (%)", xlab = "Trading Days",
           main = sprintf("Volatility of Portfolio %s", colnames(x[,n])),
           ylim = c(min(x[,n]), max(x[,n])), las = 2)
      
      abline(h = 0)
      
      if (max(x[,n]) < 10){
        
        axis(side = 2, at = seq(-100, 100, 1), las = 2) # Set up axis
        abline(h = seq(-100, -1, 1), col = "grey", lty = 3)
        abline(h = seq(1, 100, 1), col = "grey", lty = 3) }
      
      else if (max(x[,n]) < 15){
        
        axis(side = 2, at = seq(-100, 100, 2), las = 2) # Set up axis
        abline(h = seq(-100, -2, 2), col = "grey", lty = 3)
        abline(h = seq(2, 100, 2), col = "grey", lty = 3) } else {
          
          axis(side = 2, at=seq(100, from = 5, by = 5), las = 2) # Set up axis
          abline(h = seq(-100, -5, 5), col = "grey", lty = 3)
          abline(h = seq(5, 100, 5), col = "grey", lty = 3) } } } else {
        
      x <- abs(x)
        
      for (n in 1:ncol(x)){ # Plot returns fluctuations
          
        plot(x[,n], col = "red", ylab = "Fluctuations (%)", las = 2,
             xlab = "Trading Days", ylim = c(min(x[,n]), max(x[,n])),
             main = sprintf("Portfolio %s Fluctuations", colnames(x[,n])))
        
        abline(h = 0)
        
        if (max(x[,n]) < 10){ axis(side = 2, at = seq(-100, 100, 1), las = 2) 
        
          abline(h = seq(1, 100, 1), col = "grey", lty = 3) }
          
        else if (max(x[,n]) < 15){
            
          axis(side = 2, at = seq(-100, 100, 2), las = 2) # Set up axis
          abline(h = seq(2, 100, 2), col = "grey", lty = 3) } else {
              
          axis(side = 2, at=seq(100, from = 5, by = 5), las = 2) # Set up axis
          abline(h = seq(5, 100, 5), col = "grey", lty = 3) } } }
}
p.volatility.plt(returns_df, abs = F)
