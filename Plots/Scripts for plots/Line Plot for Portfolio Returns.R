library("timeSeries") # Library

p.plt <- function(x, SD = F){ # Line Plot of Portfolio Returns
  
  x <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Take columns with Total Sum
  
  x1 <- t(x) # Transpose x1 and x
  
  colnames(x1) <- rownames(x) # Make dates as column names x1 and x
  
  r <- as.data.frame(0) # Define dataframe with value zero
  
  # Loop for portfolio log returns calculation
  for (n in 2:ncol(x1)){ df2p <- x1[,(n-1):n] # Select two periods
    
    s <- df2p[apply(df2p, 1, function(row) all(row !=0 )),] # Remove zeros & NA
    
    # Add newly generated variable to data frame
    r <- rbind(r, log(as.numeric(colSums(s)[2]) / as.numeric(colSums(s)[1]))) }
    
  colnames(r) <- "Returns" # Give name to column
  rownames(r) <- rownames(x) # Return dates to index
  
  x <- as.timeSeries(r) # Make it time series
  
  main = ifelse(SD==T, "Volatility of Portfolio Returns", "Portfolio Returns")
  
  if (SD){ x <- x * 100 } else { x <- (exp(cumsum(x)) - 1) * 100 }
  
  ylab = ifelse(SD == T, "Fluctuations (%)", "Return (%)")
  
  col = ifelse(SD == T, "red", ifelse(x[nrow(x),] > 0, "green4", "red3"))
  
  plot(
    x,
    main = main,
    type = "l",
    col = col,
    las = 1,
    ylab = ylab,
    xlab = "Trading Days"
    ) 
  
  grid(nx = 1, ny = NULL, col = "grey", lty = 3, lwd = 1) # Horizontal lines
  
  if (SD){ abline(h = 0) } else { abline(h = x[nrow(x),], col="navy", lwd=2) }
  
  par(mar = rep(5, 4)) # Define borders of the plot
  
  axis(side = 4, las = 2) # Axes
}
p.plt(df_portfolio, SD = F) # Test
