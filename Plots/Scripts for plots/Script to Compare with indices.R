# Function to plot portfolio performance with indices
comp_plot_with_indices <- function(x, main = NULL, benchmark = "^GSPC",
                                   benchnames = "S&P 500", lwd = 1){
  
  # First date
  start_date <- rownames(x)[1]
  
  # Last date
  end_date <- rownames(x)[nrow(x)]
  
  # Calculate total returns for portfolio
  x <- apply(x, 2, function(col) (exp(cumsum(col))-1))
  
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
  tickers_for_indices <- benchmark
  
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
  colnames(portfolioPrices) <- benchnames
  
  # Make data discrete
  portfolioReturns <- ROC(portfolioPrices, type = "discrete")
  
  # Make it time series
  portfolioReturns <-as.timeSeries(portfolioPrices)
  
  # Calculate returns
  portfolioReturns <- diff(log(portfolioReturns))
  
  # Equal first return to 0
  portfolioReturns[1,] <- 0
  
  # Calculate total returns
  portfolioReturns <-apply(portfolioReturns,
                           2, function(col) (exp(cumsum(col))-1))
  
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
  
  # Plot
  plot(df_x_indcs[,1],
       ylim = c(min(df_x_indcs), max(df_x_indcs)),
       main = main,
       xlab = "Trading Days",
       ylab = "Returns (%)",
       las = 1,
       lwd = lwd)
  
  # Add grey dotted horizontal lines
  for (n in seq(-1, 1, 0.05)){ abline(h = n, col = "grey", lty = 2) }
  
  # Plot indices
  for (n in 2:(ncol(df_x_indcs))){
    lines(df_x_indcs[,n], col = n,  lwd = lwd,)}
  
  # Add legend
  legend("bottomright", colnames(df_x_indcs),
         col=1:ncol(df_x_indcs), lty=1, cex=.65)
}
# Test
comp_plot_with_indices(returns_df,
                       main = "Portfolio vs Major Indices",
                       benchmark = c("^GSPC", "^DJI", "^IXIC", "^FTSE"),
                       benchnames = c("S&P 500", "Dow Jones", "NASDAQ",
                                      "FTSE 100"),
                       lwd = 2)
