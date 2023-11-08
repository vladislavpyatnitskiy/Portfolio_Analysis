# Function to convert portfolio prices to another currency
p.currency.converter <- function(x, y){

  p <- x[,ncol(x)] # Take column with total sum
  
  trading_days1 <- rownames(p) # Subset dates from time series
  
  p <- data.frame(trading_days1, p) # Join both dates and prices as data frame
  
  colnames(p)[colnames(p) == 'trading_days1'] <- "Date" # Rename column
  
  rownames(p) <- seq(nrow(p)) # Row Names Sequence
  
  s <- rownames(x)[1] # Define start date
  
  prices <- NULL # Create empty variable to contain data
  
  for (c in y){ # For each currency in data frame download data
    
    prices<-cbind(prices,getSymbols(c,from=s,src="yahoo",auto.assign=F)[,4]) }
  
  prices <- prices[apply(prices,1,function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(prices) <- y # Put the tickers in data set
  
  r <-as.timeSeries(prices) # Make it time series
  
  trading_days2 <- rownames(r) # Subset dates from time series
  
  r <- data.frame(trading_days2, r) # Join dates and prices
  
  colnames(r)[colnames(r) == 'trading_days2'] <- "Date" # Rename column
  
  rownames(p) <- seq(nrow(p)) # Put sequence for row names
  
  df.currencies <- merge(p, r, by = "Date") # Join them into one column
  
  df.currencies$currency<-df.currencies[,2]*df.currencies[,3] # New column
  
  rownames(df.currencies) <- df.currencies[,1] # Put sequence for rownames
  
  df.currencies <- as.timeSeries(df.currencies) # Make it time series
  
  colnames(df.currencies)[colnames(df.currencies) == 'currency'] <- y # Rename
  
  return(df.currencies[,3]) # Display column
}
p.currency.converter(x = df_portfolio, y = "EUR=X") # Test
