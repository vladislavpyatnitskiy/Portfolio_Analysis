# Function to plot portfolio performance with indices
comp.plt <- function(x,main=NULL,benchmark="^GSPC",benchnames="S&P 500",lwd=1){
  
  s <- rownames(x)[1] # Start Date
  e <- rownames(x)[nrow(x)] # End Date
  x <- apply(x, 2, function(col) exp(cumsum(col)) - 1) # Portfolio returns
  
  portfolio_r_nms <- rownames(x) # Subset dates from data set
  
  x <- data.frame(portfolio_r_nms, x) # Join dates with logs
  
  colnames(x) <- c("Date", "Portfolio") # Rename columns once again
  
  rownames(x) <- index(portfolio_r_nms) # Create index numbers for data set
  
  tickers_for_indices <- benchmark # Vector with tickers
  
  p <- NULL # Create an empty variable
  
  for (Ticker in tickers_for_indices){ # Loop for data extraction
    
    p <- cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])}
  
  p <- p[apply(p,1,function(x) all(!is.na(x))),] # NA off
  
  colnames(p) <- benchnames # Put the tickers in data set
  
  r <- diff(log(as.timeSeries(p))) # Time Series Returns
  
  r[1,] <- 0 # Equal first return to 0
  
  r <- apply(r, 2, function(col) exp(cumsum(col))-1) # total returns
  
  indices_r_nms <- rownames(r) # Subset dates from data set
  
  r <- data.frame(indices_r_nms, r) # Join dates with logs
  
  colnames(r)[colnames(r) == 'indices_r_nms'] <- 'Date' # Rename column Dates
  
  rownames(r) <- index(indices_r_nms) # Create index numbers for data set
  
  i <- as.timeSeries(merge(x, r, by = "Date")) # Merge and make time series
  
  par(mar = c(3, 3, 3, 3), xpd = F) 
  
  plot(i[,1], ylim = c(min(i), max(i)), main = main, lwd = lwd, las = 1, # Plot
       xlab = "Trading Days", ylab = "Returns (%)", type = "l", lty = 1)
  
  # Add grey dotted horizontal lines
  for (n in seq(-1, 1, .05)){ abline(h = n, col = "grey", lty = 3) }
  
  for (n in 2:(ncol(i))){ lines(i[,n], col = n, lwd = lwd,)} # Plot indices
  
  legend(x = "bottomright",colnames(i),col=1:ncol(i),lty=1,cex =.65,xpd=T)
}
# Test
comp.plt(returns_df, main = "Portfolio vs Major Indices", lwd = 2,
         benchmark = c("^GSPC", "^DJI", "^IXIC", "^FTSE"),
         benchnames=c("S&P 500","Dow Jones","NASDAQ","FTSE 100"))
