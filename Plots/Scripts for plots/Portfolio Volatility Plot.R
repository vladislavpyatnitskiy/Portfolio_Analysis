library(timeSeries) # Library

p.volatility.plt <- function(x, abs = F){ # Plot Security volatility
  
  x <- x[,3*seq(ncol(x)%/%3,from=1)] # Subtract total values for each stock
  
  x1 <- t(x) # Transpose data frame
  
  colnames(x1) <- rownames(x) # Make dates as column names x1 and x
  
  r <- as.data.frame(0) # Define data frame with value zero
  
  # Loop for portfolio log returns calculation
  for (n in 2:ncol(x1)){ df2p <- x1[,(n-1):n] # x1 # Select two periods
  
    s <- df2p[apply(df2p, 1, function(row) all(row !=0 )),] # Remove zeros & NA
    
    r <- rbind(r, log(colSums(s)[2]/colSums(s)[1])) } # Add value to data frame
    
  colnames(r) <- "Returns" # Give name to column
  rownames(r) <- rownames(x) # Return dates to index
  
  x <- as.timeSeries(r) * 100 # Make it time series & multiply by 100
  
  if (isFALSE(abs)){ # Choose between real and absolute values of returns
    
    for (n in 1:ncol(x)){ # Plot volatility with positive and negative returns
      
      plot(x[,n], col = "red", ylab = "Returns (%)", xlab = "Trading Days",
           main = sprintf("Volatility of Portfolio %s", colnames(x[,n])),
           ylim = c(min(x[,n]), max(x[,n])), las = 2)
      
      } } else { x <- abs(x)
            
      for (n in 1:ncol(x)){ # Plot returns fluctuations
              
        plot(x[,n], col = "red", ylab = "Fluctuations (%)", las = 2,
             xlab = "Trading Days", ylim = c(min(x[,n]), max(x[,n])),
             main = sprintf("Portfolio %s Fluctuations", colnames(x[,n]))) } }
  
  abline(h = 0)
  
  axis(side = 4, las = 2)
  
  grid(nx = 1, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
}
p.volatility.plt(df_portfolio, abs = F)
