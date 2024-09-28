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
  
  h <- hist(s, main="Histogram & Normal Distribution of Portfolio Returns",
            ylab = "Likelihood", xlab = "Returns", xlim = c(min(s), max(s)),
            col = "navy", border = "white", breaks = 100, las = 1, freq = F)
  
  m <- round(min(s)*-1 + max(s),1)/10^(nchar(round(min(s)*-1 + max(s),1))-2)
  
  d <- c(0,.0001,.0002,.0005,.001,.002,.005,.01,.02,.05,.1,.2,.5,1) 
  
  for (n in 1:length(d) - 1){ if (m >= d[n] && m < d[n + 1]){
    
      mn <- d[n + 1] } else { next } }
  
  M <- round(max(h$density)) / 10 ^ (nchar(round(max(h$density))))
  
  i <- c(0, 1, 2, 5) # Calculate intervals for lines and axes
  
  for (n in 1:length(i) - 1){ if (M >= i[n] && M < i[n + 1]){
    
      mx <- i[n + 1] * 10 ^ (nchar(M) - 3) } else { next } }
  
  abline(v = 0, col = "gold", lwd = 2) # Add vertical line at 0
  for (n in seq(-1, 1, by = mn)[-match(0, seq(-1, 1, by = mn))]){ 
    
    abline(v = n, col = "grey", lty = 3) } # Add Vertical lines
  
  abline(h = 0) # Horizontal line at 0 and other above ones
  for (n in seq(mx, 100, by = mx)){ abline(h = n, col = "grey", lty = 3) }
  
  curve(dnorm(x, mean = mean(s), sd = sd(s)), col = "red", lwd = 3, add = T)
  
  box()
}
p.hist.plt(df_portfolio) # test
