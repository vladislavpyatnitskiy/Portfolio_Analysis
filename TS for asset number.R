# Start dates
start_date_port <- c("2022-07-09", "2022-09-10")

# End dates
end_date_port <- c("2022-09-09" , "2023-07-09")

# Number of assets 
number_of_assets <- c(2, 3)

# Function
asset_builder <- function(x, y, z){
  
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
  
  # Display series
  return(result_ts)
}
# Test
asset_builder(x = start_date_port,
              y = end_date_port,
              z = number_of_assets)
