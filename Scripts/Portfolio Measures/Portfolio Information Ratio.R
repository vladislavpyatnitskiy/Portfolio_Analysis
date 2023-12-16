IR <- function(x, spx = "^GSPC", benchnames = "S&P 500"){ # Information Ratio
  
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
  
  for (n in 2:ncol(x)){ active.return <- x[,1][-1,]-x[,n][-1,]
    
    l <- cbind(l, mean(active.return) / sd(active.return)) }
  
  rownames(l) <- "IR" # Give name
  
  colnames(l) <- benchnames # Names of indices
  
  l # Display
}
IR(returns_df, spx = c("^GSPC", "^IXIC", "^DJI"),
   benchnames = c("S&P 500", "NASDAQ", "Dow Jones")) # Test
