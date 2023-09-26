# Function to get portfolio returns
portfolio_returns <- function(x){
  
  # Select columns with total values and join them
  x <- x[,3 + 3 * seq(31, from = 0)]
  
  # Transpose
  x1 <- t(x)
  
  # Make dates as column names
  colnames(x1) <- rownames(x)
  
  # Define dataframe with value zero
  df_p_l_returns <- as.data.frame(0)
  
  # Loop for portfolio log returns calculation
  for (n in 2:ncol(x1)){
    
    # Select two periods
    two_period_df <- x1[,(n-1):n]
    
    # Find zeros in data frame
    f_value1 <- apply(two_period_df, 1, function(row) all(row !=0 ))
    
    # Get rid of rows containing zeros
    s_value1 <- two_period_df[f_value1,]
    
    # Sum values for current period
    num_f_t1 <- as.numeric(colSums(s_value1)[1])
    
    # Sum values for next period
    num_f_t2 <- as.numeric(colSums(s_value1)[2])
    
    # Calculate return
    p_l_value <- log(num_f_t2 / num_f_t1)
    
    # Add newly generated variable to data frame
    df_p_l_returns <- rbind(df_p_l_returns, p_l_value)
    
  }
  
  # Give name to column
  colnames(df_p_l_returns) <- "Returns"
  
  # Return dates to index
  rownames(df_p_l_returns) <- rownames(df_exp_for_logs)
  
  # Make it time series
  df_p_l_returns <- as.timeSeries(df_p_l_returns)
  
  # Display data frame
  return(df_p_l_returns)
}
# Test
returns_df <- portfolio_returns(df_portfolio)
