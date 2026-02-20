library(timeSeries) # Library

p.drawdown.plt <- function(x, SD = T){ # Function to plot asset drawdown
  
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
  
  if (!SD){ r <- rownames(x) # Save dates for Cumulative Returns
  
    x <- apply(x, 2, function(col) exp(cumsum(col)) - 1) # Cumulative Returns
    
    rownames(x) <- r } # Return dates as row names as they were vanished 
  
  x <- x * 100 # Multiply returns by 100
  
  x[x > 0] <- 0 # Replace positive values as 0

  par(mar = c(5, rep(4, 3))) # Margins
               
  for (n in 1:ncol(x)){ s <- x[,n] # Plot each column in data frame
  
    plot(
      s,
      type = "l",
      las = 2,
      ylim = c(min(s), 0),
      lwd = 1,
      col = "red",
      xlab = "Trading Days",
      ylab = "Negative Returns (%)",
      main = sprintf("%s Drawdown", colnames(s))
      )
    
    grid(nx = 1, ny = NULL, col = "grey", lty = 3, lwd = 1) # Horizontal lines
    
    abline(h = 0) # Break Even
    
    axis(side = 4, las = 2) } # Right y-axis}
}
p.drawdown.plt(df_portfolio, SD = T) # Test
