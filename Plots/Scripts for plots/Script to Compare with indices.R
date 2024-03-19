lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Function to plot portfolio performance with indices
comp.plt <- function(x, benchmark = "^GSPC", benchnames = "S&P 500"){
  
  s <- rownames(x)[1] # Start Date
  e <- rownames(x)[nrow(x)] # End Date
  x <- apply(x,2, function(col) exp(cumsum(col)) - 1) * 100 # Portfolio returns
  
  p.row.names <- rownames(x) # Subset dates from data set
  
  x <- data.frame(p.row.names, x) # Join dates with logs
  
  colnames(x) <- c("Date", "Portfolio") # Rename columns once again
  rownames(x) <- seq(nrow(x)) # Create index numbers for data set
  
  p <- NULL # Create an empty variable
  
  for (A in benchmark) # Loop for data extraction
    
    p <- cbind(p, getSymbols(A, from=s, to=e, src="yahoo", auto.assign=F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Clean data
  
  r <- diff(log(as.timeSeries(p))) # Make time series and calculate logs
  
  r[1,] <- 0 # Equal first return value to 0
  
  r <- apply(r, 2, function(col) exp(cumsum(col)) - 1) * 100 # total returns
  
  i.names <- rownames(r) # Subset dates from data set
  
  r <- data.frame(i.names, r) # Join dates with logs
  
  colnames(r)[colnames(r) == 'i.names'] <- 'Date' # Rename column Dates
  
  rownames(r) <- seq(nrow(r)) # Create index numbers for data set
  
  i <- as.timeSeries(merge(x, r, by = "Date")) # Merge and make time series
  
  par(mar = c(8, 4, 4.1, 2.5)) # Define borders of the plot
  
  plot(i[,1], ylim = c(min(i), max(i)), lty = 1, type = "l", lwd = 2, las = 1,
       xlab = "Trading Days", ylab = "Returns (%)",
       main = "Performance of Portfolio and Major Benchmarks")
  
  axis(side = 4, las = 1, at = seq(-100, 100, 5))
  
  # Add grey dotted horizontal lines
  for (n in seq(-100, -5, 5)){ abline(h = n, col = "grey", lty = 3) }
  for (n in seq(5, 100, 5)){ abline(h = n, col = "grey", lty = 3) }
  
  abline(h = 0) # Add black horizontal line at break even point
  
  for (n in 2:(ncol(i))){ lines(i[,n], col = n, lwd = 2) } # Plot indices
  
  legend(x = "bottom", inset = c(0, -0.3), legend = c("Portfolio", benchnames),
         col = seq(ncol(i)), lwd = 2, cex = .85, bty = "n", xpd = T, horiz = T)
  
  on.exit(par(par(no.readonly = T))) # Show legend with names
}
comp.plt(returns_df, benchmark = c("^GSPC", "^DJI", "^IXIC", "^FTSE"),
         benchnames=c("S&P 500","Dow Jones","NASDAQ","FTSE 100")) # Test
