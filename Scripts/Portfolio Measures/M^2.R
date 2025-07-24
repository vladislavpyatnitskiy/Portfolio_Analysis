lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

m.squared <- function(x, spx = c("^GSPC", "^IXIC", "^DJI"), tr = "^TNX",
                      benchnames = NULL){ # M squared for portfolio
  
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
  
  y <- c(tr, spx) # Join treasuries and index data
  
  s <- rownames(x)[1] # Assign first date
  e <- rownames(x)[nrow(x)] # Assign last date
  
  p <- NULL # Create an empty variable
  
  for (A in y) p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo", # Daily data
                                     auto.assign=F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  i <- diff(log(p[, -which(names(p) == tr)])) # make logs
  
  i[1,] <- 0 # assign first value which is NA
  
  rf <- apply(p[,tr], 2, function(col) mean(col) / 100) # Risk Free
  
  x <- merge(x, i)[-nrow(x),] # Join portfolio and index data
  
  l <- NULL # Set list for values
  
  for (n in 2:ncol(x)) # Calculate ratios for each benchmark
    
    l = rbind(l, (exp(sum(x[,1]))-rf)*sd(x[,n])/sd(x[,1])-(exp(sum(x[,n])-rf)))
  
  colnames(l) <- "M ^ 2" # Give name
  rownames(l) <- benchnames # Give bench names
  
  l # Display value
}
m.squared(df_portfolio, benchnames=c("S&P 500", "NASDAQ", "Dow Jones")) # Test
