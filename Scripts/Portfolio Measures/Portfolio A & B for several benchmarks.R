# Function to get alphas & beta of portfolio for several indices
p.coefficients.indices <- function(x,indices = "^GSPC",benchnames = "S&P 500"){
  
  s <- rownames(x)[1] # First date
  
  e <- rownames(x)[nrow(x)] # Last date
  
  p.rownames <- rownames(x) # Subset dates from data set
  
  x <- data.frame(p.rownames, x) # Join dates with logs
  
  colnames(x) <- c("Date", "Portfolio") # Rename columns once again
  
  rownames(x) <- seq(nrow(x)) # Create index numbers for data set
  
  p <- NULL # Create an empty variable
  
  for (Ticker in indices) # Loop for data extraction
    
    p <- cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- benchnames # Put the tickers in data set
  
  r <- diff(log(as.timeSeries(p))) # Make it time series & Calculate returns
  
  r[1,] <- 0 # Equal first value to zero
  
  b.table <- NULL
  
  for (n in 1:ncol(r)){ r.names <- rownames(r[,n]) # Subset dates from data set
    
    r.i <- data.frame(r.names, r[,n]) # Join dates with logs
    
    colnames(r.i)[colnames(r.i)=='r.names']<-'Date' # Rename column with Dates  
    
    rownames(r.i) <- seq(nrow(p)) # Create index numbers for data set
    
    i <- as.timeSeries(merge(x, r.i, by="Date")) # Merge & make it time series
    
    # Calculate Alphas & Betas, assign row names and Join all values
    v<-rbind(apply(i[,1],2,function(col) ((lm((col)~i[,2]))$coefficients[1])),
             apply(i[,1],2,function(col) ((lm((col)~i[,2]))$coefficients[2])))
    
    rownames(v) <- c("A", "B")
    
    if (is.null(b.table)) { b.table<-v } else { b.table <- cbind(b.table,v) } }
  
  colnames(b.table) <- benchnames # Assign column names
  
  return(b.table) # Round values, make a string with info and display values
}
p.coefficients.indices(returns_df, indices = c("^GSPC", "^DJI", "^IXIC"),
                       benchnames = c("S&P 500", "Dow Jones", "NASDAQ")) # Test
