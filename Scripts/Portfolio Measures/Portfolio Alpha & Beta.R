# Function to get beta of portfolio
coefs_for_portfolio <- function(x, bnchmrk = "^GSPC"){
  
  # First date
  start_date <- rownames(x)[1]
  
  # Last date
  end_date <- rownames(x)[nrow(x)]
  
  # Subset dates from data set
  portfolio_r_nms <- rownames(x)
  
  # Join dates with logs
  x <- data.frame(portfolio_r_nms, x)
  
  # Rename columns once again
  colnames(x) <- c("Date", "Portfolio")
  
  # Create index numbers for data set
  index_for_prtfl <- index(portfolio_r_nms)
  
  # Join index as row names
  rownames(x) <- index_for_prtfl
  
  # Vector with tickers
  tickers_for_indices <- c(bnchmrk)
  
  # Create an empty variable
  portfolioPrices <- NULL
  
  # Loop for data extraction
  for (Ticker in tickers_for_indices)
    
    # When both start date and end date are defined
    portfolioPrices <- cbind(portfolioPrices,
                             getSymbols(Ticker,
                                        from = start_date,
                                        to = end_date,
                                        src = "yahoo", 
                                        auto.assign=FALSE)[,4])
  # Get rid of NAs
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!is.na(x))),]
  # Put the tickers in data set
  colnames(portfolioPrices) <- c(bnchmrk)
  
  # Make data discrete
  portfolioReturns <- ROC(portfolioPrices, type = "discrete")
  
  # Make it time series
  portfolioReturns <-as.timeSeries(portfolioPrices)
  
  # Calculate returns
  portfolioReturns <- diff(log(portfolioReturns))
  
  # Equal first value to zero
  portfolioReturns[1,] <- 0
  
  # Subset dates from data set
  indices_r_nms <- rownames(portfolioReturns)
  
  # Join dates with logs
  portfolioReturns <- data.frame(indices_r_nms, portfolioReturns)
  
  # Rename column containing Dates
  colnames(portfolioReturns)[colnames(portfolioReturns) ==
                               'indices_r_nms'] <- 'Date'
  
  # Create index numbers for data set
  index_for_indcs <- index(indices_r_nms)
  
  # Join index as row names
  rownames(portfolioReturns) <- index_for_indcs
  
  # Merge 
  df_x_indcs <- merge(x, portfolioReturns, by = "Date")
  
  # Make it time series
  df_x_indcs <- as.timeSeries(df_x_indcs)

  # Calculate Betas
  beta_value <- apply(df_x_indcs[,1],
             2,
             function(col) ((lm((col) ~ df_x_indcs[,2]))$coefficients[2]))
  
  # Calculate Betas
  alpha_value <- apply(df_x_indcs[,1],
                      2,
                      function(col) ((lm((col) ~
                                           df_x_indcs[,2]))$coefficients[1]))
  # Round values
  beta_value <- round(beta_value, 2)
  alpha_value <- round(alpha_value * 100, 2)
  
  # Make a string with info
  p_info <- sprintf("Portfolio Alpha is %s %%, Beta is %s",
          alpha_value, beta_value)
  
  # Display values
  return(p_info)
}
# Test
coefs_for_portfolio(returns_df)
