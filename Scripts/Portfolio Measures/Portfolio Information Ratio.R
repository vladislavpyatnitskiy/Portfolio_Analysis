lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

IR <- function(x, spx = "^GSPC", benchnames = "S&P 500"){ # Information Ratio
  
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
  
  y <- c(spx) # Join treasuries and index data
  
  s <- rownames(x)[1] # Assign first date
  e <- rownames(x)[nrow(x)] # Assign last date
  
  p <- NULL # Create an empty variable
  
  for (A in y) p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo", # Daily data
                                     auto.assign=F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  # Make time series, logs, join portfolio and index data
  x <- merge(x,diff(log(as.timeSeries(p)[,spx])))[-nrow(x),]
  
  l <- NULL # Set list for values
  
  for (n in 2:ncol(x)){ active.return <- x[,1][-1,] - x[,n][-1,]
  
    l <- rbind(l, mean(active.return) / sd(active.return)) }
  
  colnames(l) <- "IR" # Give name
  rownames(l) <- benchnames # Names of indices
  
  l # Display
}
IR(df_portfolio, spx = c("^GSPC", "^IXIC", "^DJI"),
   benchnames = c("S&P 500", "NASDAQ", "Dow Jones")) # Test
