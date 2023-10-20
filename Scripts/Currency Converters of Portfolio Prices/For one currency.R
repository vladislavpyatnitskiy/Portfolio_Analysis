# Function to convert portfolio prices to another currency
portfolio.currency.converter <- function(x, y){
  
  # Take column with total sum
  portfolioPrices <- x[,ncol(x)]
  
  # Subset dates from time series
  trading_days1 <- rownames(portfolioPrices)
  
  # Join both dates and prices as data frame
  portfolioPrices <- data.frame(trading_days1, portfolioPrices)
  
  # Rename column with trading dates as Date
  colnames(portfolioPrices)[colnames(portfolioPrices) ==
                              'trading_days1'] <- "Date"

  # Put sequence for row names
  rownames(portfolioPrices) <- seq(nrow(portfolioPrices))
  
  # Define start date
  start_date <- rownames(x)[1]
  
  # Create empty variable to contain data
  currency_prices <- NULL
  
  # For each currency in data frame
  for (currency in y){
    
    # Download data
    currency_prices <- cbind(currency_prices, 
                             getSymbols(currency,
                                        from = start_date,
                                        src = "yahoo",
                                        auto.assign=FALSE)[,4]) }
  # Get rid of NAs
  currency_prices <- currency_prices[apply(currency_prices,1,
                                           function(x) all(!is.na(x))),]
  # Put the tickers in data set
  colnames(currency_prices) <- y
    
  # Make data discrete
  currency_Returns <- ROC(currency_prices, type = "discrete")
    
  # Make it time series
  currency_Returns <-as.timeSeries(currency_prices)
  
  # Subset dates from time series
  trading_days2 <- rownames(currency_Returns)
  
  # Join dates and prices 
  currency_Returns <- data.frame(trading_days2, currency_Returns)
  
  # Rename column with trading dates as Date for currencies
  colnames(currency_Returns)[colnames(currency_Returns) ==
                               'trading_days2'] <- "Date"
  
  # Put sequence for row names
  rownames(portfolioPrices) <- seq(nrow(portfolioPrices))
  
  # Join them into one column
  df_with_currencies <- merge(portfolioPrices, currency_Returns, by = "Date")
  
  # Create new column
  df_with_currencies$new_currency <- df_with_currencies[,2] *
    df_with_currencies[,3]
  
  # Put sequence for rownames
  rownames(df_with_currencies) <- df_with_currencies[,1]
  
  # Make it time series
  df_with_currencies <- as.timeSeries(df_with_currencies)
  
  # Rename to another currency name
  colnames(df_with_currencies)[colnames(df_with_currencies) ==
                               'new_currency'] <- y
  
  # Display column
  return(df_with_currencies[,3])
}
# Test
portfolio.currency.converter(x = df_portfolio, y = "EUR=X")
