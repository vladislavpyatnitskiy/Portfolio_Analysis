# Function to get alpha and beta of portfolio
coefs_for_portfolio <- function(x, bnchmrk = "^GSPC", string = F){
  
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
  
  # Create index numbers for data set and join index as row names
  rownames(x) <- seq(nrow(x))
  
  # Create an empty variable
  portfolioPrices <- NULL
  
  # Loop for data extraction
  for (Ticker in bnchmrk)
    
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
  colnames(portfolioPrices) <- bnchmrk
  
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
  
  # Create index numbers for data set and join index as row names
  rownames(portfolioReturns) <- seq(nrow(portfolioPrices))
  
  # Merge and make it time series
  df_x_indcs <- as.timeSeries(merge(x, portfolioReturns, by = "Date"))
  
  # Calculate Betas
  beta_value <- apply(df_x_indcs[,1],2,
                      function(col) ((lm((col) ~
                                           df_x_indcs[,2]))$coefficients[2]))
  # Calculate Betas
  alpha_value <- apply(df_x_indcs[,1],2,
                       function(col) ((lm((col) ~
                                            df_x_indcs[,2]))$coefficients[1]))
  # Choose either string or table
  if (isTRUE(string)) { 
    
    # Round values, make a string with info and display values
    return(sprintf("Portfolio Alpha is %s %%, Beta is %s",
                   round(alpha_value * 100, 2), round(beta_value, 2))) } else {
                     
    # Round values, make a table with info and display values                   
    return(rbind(alpha_value, beta_value)) }
}
# Test
coefs_for_portfolio(returns_df, string = T)
