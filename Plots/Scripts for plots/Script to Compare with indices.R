lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Function to plot portfolio performance with indices
comp.plt <- function(x, benchmark = "^GSPC", benchnames = "S&P 500"){
  
  P <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Take columns with Total Sum
  
  P1 <- t(P) # Transpose 
  
  colnames(P1) <- rownames(P) # Make dates as column names 
  
  R <- as.data.frame(0) # Define data frame with value zero
  
  # Loop for portfolio log returns calculation
  for (n in 2:ncol(P1)){ df2p <- P1[,(n-1):n] # x1 # Select two periods
  
    s <- df2p[apply(df2p, 1, function(row) all(row !=0 )),] # Remove zeros & NA
    
    # Add newly generated variable to data frame
    R <- rbind(R, log(as.numeric(colSums(s)[2]) / as.numeric(colSums(s)[1]))) }
  
  colnames(R) <- "Returns" # Give name to column
  rownames(R) <- rownames(P) # Return dates to index
  
  R <- as.timeSeries(R) # Make it time series
  
  s <- rownames(R)[1] # Start Date
  e <- rownames(R)[nrow(R)] # End Date
  R <- apply(R, 2, function(col) exp(cumsum(col)) - 1) * 100 # Returns
  
  p.row.names <- rownames(R) # Subset dates from data set
  
  R <- data.frame(p.row.names, R) # Join dates with logs
  
  colnames(R) <- c("Date", "Portfolio") # Rename columns once again
  rownames(R) <- seq(nrow(R)) # Create index numbers for data set
  
  p <- NULL # Create an empty variable
  
  for (A in benchmark) # Loop for data extraction
    
    p <- cbind(p, getSymbols(A, from=s, to=e, src="yahoo", auto.assign=F)[,4])
  
  colnames(p) <- benchnames # Assign names of indices
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Clean data
  
  r <- diff(log(as.timeSeries(p)))#[-1,] # Make time series and calculate logs
  
  r[1,] <- 0 # Equal first return value to 0
  
  r <- apply(r, 2, function(col) exp(cumsum(col)) - 1) * 100 # total returns
  
  i.names <- rownames(r) # Subset dates from data set
  
  r <- data.frame(i.names, r) # Join dates with logs
  
  colnames(r)[colnames(r) == 'i.names'] <- 'Date' # Rename column Dates
  
  rownames(r) <- seq(nrow(r)) # Create index numbers for data set
  
  i <- as.timeSeries(merge(R, r, by = "Date")) # Merge and make time series
  
  par(mar = c(8, 2.5, 4, 2.5)) # Define borders of the plot
  
  plot(i[,1], ylim = c(min(i), max(i)), lty = 1, type = "l", lwd = 2, las = 1,
       xlab = "Trading Days", ylab = "Returns (%)",
       main = "Performance of Portfolio and Major Benchmarks")
  
  m <- round(min(i) * -1 + max(i), 0) # Set up interval for axes and lines
  
  m <- m / 10 ^ (nchar(m) - 1)
  
  if (m > 0 && m < 1){ mn <- 1 * 10 ^ (nchar(m) - 3) }
  
  else if (m > 1 && m < 2){ mn <- 2 * 10 ^ (nchar(m) - 3) }
  
  else if (m > 2 && m < 5){ mn <- 5 * 10 ^ (nchar(m) - 3) }
  
  axis(side = 4, las = 1, at = seq(-100, 100, mn)) # Axes
  
  # Add grey dotted horizontal lines
  for (n in seq(-100, -mn, mn)){ abline(h = n, col = "grey", lty = 3) }
  for (n in seq(mn, 100, mn)){ abline(h = n, col = "grey", lty = 3) }
  
  abline(h = 0) # Add black horizontal line at break even point
  
  for (n in 2:(ncol(i))){ lines(i[,n], col = n, lwd = 2) } # Plot indices
  
  legend(x = "bottom", inset = c(0, -0.22), legend = c("Portfolio", benchnames),
         col = seq(ncol(i)), lwd = 2, cex = .85, bty = "n", xpd = T, horiz = T)
  
  on.exit(par(par(no.readonly = T))) # Show legend with names
}
comp.plt(df_portfolio, benchmark = c("^GSPC", "^DJI", "^IXIC", "^FTSE"),
         benchnames=c("S&P 500","Dow Jones","NASDAQ","FTSE 100")) # Test
