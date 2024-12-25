library(timeSeries) # Library

p.hist.plt <- function(x){ # Histogram of Portfolio Returns
  
  x <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Take columns with Total Sum
  
  x1 <- t(x) # Transpose x1 and x
  
  colnames(x1) <- rownames(x) # Make dates as column names x1 and x
  
  r <- as.data.frame(0) # Define dataframe with value zero
  
  # Loop for portfolio log returns calculation
  for (n in 2:ncol(x1)){ df2p <- x1[,(n - 1):n] # x1 # Select two periods
  
    v <- df2p[apply(df2p, 1, function(row) all(row !=0 )),] # Remove zeros & NA
    
    # Add newly generated variable to data frame
    r <- rbind(r, log(as.numeric(colSums(v)[2]) / as.numeric(colSums(v)[1]))) }
    
  s <- as.timeSeries(r) # Make it time series
  
  h <- hist(s, ylab = "Likelihood", xlab = "Returns", border = "white",
            main="Histogram & Normal Distribution of Portfolio Returns",
            col = "navy", breaks = 100, las = 1, freq = F)
  
  abline(v = 0, col = "gold", lwd = 2) # Add vertical line at 0
  abline(h = 0)
  
  grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
  
  curve(dnorm(x, mean = mean(s), sd = sd(s)), col = "red", lwd = 3, add = T)
  
  box()
}
p.hist.plt(df_portfolio) # test
