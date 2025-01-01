library("timeSeries") # Library

p.plt <- function(x, SD = F){ # Line Plot of Portfolio Returns
  
  x <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Take columns with Total Sum
  
  x1 <- t(x) # Transpose x1 and x
  
  colnames(x1) <- rownames(x) # Make dates as column names x1 and x
  
  r <- as.data.frame(0) # Define dataframe with value zero
  
  # Loop for portfolio log returns calculation
  for (n in 2:ncol(x1)){ df2p <- x1[,(n-1):n] # x1 # Select two periods
  
    s <- df2p[apply(df2p, 1, function(row) all(row !=0 )),] # Remove zeros & NA
    
    # Add newly generated variable to data frame
    r <- rbind(r, log(as.numeric(colSums(s)[2]) / as.numeric(colSums(s)[1]))) }
    
  colnames(r) <- "Returns" # Give name to column
  rownames(r) <- rownames(x) # Return dates to index
  
  x <- as.timeSeries(r) # Make it time series
  
  if (isFALSE(SD)){ x <- apply(x, 2, function(col) (exp(cumsum(col)) - 1)*100) 
    
    plot(x, main = "Returns of Portfolio Securities", las = 1, type = "l",
         col = ifelse(x[nrow(x),] > 0, "green4", "red3"), ylab = "Return (%)",
         lwd = 2, xlab = "Trading Days")
  
    abline(h = x[nrow(x),], col = "navy", lwd = 2) # Current Return
    
    } else { plot(x * 100, main = "Volatility of Portfolio Returns", type = "l",
                  col="red",las=1,ylab="Fluctuations (%)", xlab="Trading Days")  

      abline(h = 0) } # Break Even
  
  grid(nx = 1, ny = NULL, col = "grey", lty = 3, lwd = 1) # Horizontal lines
  
  par(mar = c(5, 5, 5, 5)) # Define borders of the plot
  
  axis(side = 4, las = 2) # Axes
}
p.plt(df_portfolio, SD = F) # Test
