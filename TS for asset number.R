# Function
asset_builder1 <- function(a, x, y, z){
  
  # Add everything in one data frame
  experimental_df <- data.frame(x, y, z)
  
  # Create variable to contain ts
  result_ts <- NULL
  
  # For each period
  for (n in 1:length(y)){
    
    # Extend time series
    Date_extension <- seq.Date(from = as.Date(experimental_df[n,1]),
                               to = as.Date(experimental_df[n,2]),
                               by = "day")
    
    # Add number of assets for the following period
    ts_period <- data.frame(Date_extension, experimental_df[n,3])
    
    # Give column names to data set
    colnames(ts_period) <- c("Date", "Number")
    
    # Glue time seried
    result_ts <- rbind(result_ts, ts_period)
  }
  
  # Get data 
  asset_prices <- getSymbols(a,
             from = x[1],
             to = y[length(y)],
             src = "yahoo",
             auto.assign=FALSE)[,4]
  
  # Get rid of NAs
  asset_prices <- asset_prices[apply(asset_prices,1,
                                           function(x) all(!is.na(x))),]
  # Put the tickers in data set
  colnames(asset_prices) <- a
  
  # Make data discrete
  asset_Returns <- ROC(asset_prices, type = "discrete")
  
  # Make it time series
  asset_Returns <-as.timeSeries(asset_prices)
  
  # Subset dates from data set
  dates_fr_yh <- rownames(asset_Returns)
  
  # Transform them into a date format
  dates_fr_yh <- as.Date(dates_fr_yh)
  
  # Merge asset values with its dates
  ds_from_yahoo <- data.frame(dates_fr_yh, asset_Returns)
  
  # Change column name to Date
  colnames(ds_from_yahoo)[colnames(ds_from_yahoo) == 'dates_fr_yh'] <- 'Date'
  
  # Create index numbers for data set
  index_for_fl <- index(ds_from_yahoo)
  
  # Join index as row names
  rownames(ds_from_yahoo) <- index_for_fl
  
  # merge actual ownership period with
  final_merge_test <- merge(x = ds_from_yahoo,
                            y = result_ts,
                            by = c("Date"))

  # Total sum of stock
  final_merge_test$total_sum <- final_merge_test[,2] * final_merge_test$Number
  
  # Display values
  return(final_merge_test)
}
# Other info
start_date_port <- c("2022-07-09", "2022-09-10")
end_date_port <- c("2022-09-09" , "2023-07-09")
number_of_assets <- c(2, 3)

# Test
port_asset1 <- asset_builder1(a = "FL",
               x = start_date_port,
               y = end_date_port,
               z = number_of_assets)
