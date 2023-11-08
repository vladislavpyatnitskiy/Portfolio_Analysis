# Function to convert portfolio prices to another currencies
p.currency.converter2 <- function(x, y){
  
  p <- x[,ncol(x)] # Take column with total sum
  
  trading_days1 <- rownames(p) # Subset dates from time series
  
  p <- data.frame(trading_days1, p) # Join both dates and prices as data frame
  
  colnames(p)[colnames(p) == 'trading_days1'] <- "Date" # Rename column
  
  rownames(p) <- seq(nrow(p)) # Put sequence for row names
  
  s <- rownames(x)[1] # Define start date
  
  prices <- NULL # Create empty variable to contain data
  
  for (c in y){ # For each currency in data frame download data
    
    prices<-cbind(prices,getSymbols(c,from=s,src="yahoo",auto.assign=F)[,4]) }
  
  prices <- prices[apply(prices,1,function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(prices) <- y # Put the tickers in data set
  
  r.currency <-as.timeSeries(prices) # Make it time series
  
  c.ts <- NULL # For portfolio prices values in all currencies
  
  # For each currency in data set
  for (n in 1:ncol(r.currency)){ each_currency <- r.currency[,n]
  
    days <- rownames(each_currency) # Subset dates from time series
    
    e.currency <- data.frame(days, each_currency) # Join dates & prices
    
    colnames(e.currency)[colnames(e.currency)=='days']<-"Date" # Rename column
    
    rownames(e.currency) <- seq(nrow(e.currency)) # Put sequence for row names
    
    c.df <- merge(p, e.currency, by = "Date") # Join them into one column
    
    c.df$new_currency <- c.df[,2] * c.df[,3] # Create new column
    
    rownames(c.df) <- c.df[,1] # Put sequence for row names
    
    c.df <- as.timeSeries(c.df) # Make time series
    
    # Rename to another currency name
    colnames(c.df)[colnames(c.df) == 'new_currency'] <- colnames(each_currency)
    
    # Either fill or join time series
    if (is.null(c.ts)){ c.ts <- c.df[,3] } else { c.ts<-cbind(c.ts,c.df[,3])} }
  
  return(c.ts) # Display column
}
p.currency.converter2(x = df_portfolio, y = c("EUR=X", "GBP=X")) # Test
