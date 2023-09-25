# Function to generate heatmap for portfolio's correlations
heatmap_for_correlation <- function(values_for_cor,
                                    start_date = as.Date(Sys.Date()) - 365,
                                    end_date = Sys.Date()){
  
  # Select columns with prices
  portfolio_tickers <- colnames(values_for_cor[,1 + 3 * seq(31, from = 0)])
  
  # Create an empty variable
  asset_prices <- NULL
  
  # Loop for data extraction
  for (Ticker in portfolio_tickers)
    
    # When neither start date nor end date are defined
    asset_prices <- cbind(asset_prices,
                               getSymbols(Ticker,
                               from = start_date,
                               to = end_date,
                               src = "yahoo",
                               auto.assign=FALSE)[,4]
    )
  
  # Get rid of NAs
  asset_prices <- asset_prices[apply(asset_prices,1,
                                     function(x) all(!is.na(x))),]
  # Put the tickers in data set
  colnames(asset_prices) <- portfolio_tickers
  
  # Make data discrete
  asset_Returns <- ROC(asset_prices, type = "discrete")
  
  # Make it time series
  asset_Returns <-as.timeSeries(asset_prices)
  
  # Calculate asset returns
  asset_Returns <- diff(log(asset_Returns))[-1,]
  
  # Convert data into matrix
  matrix_for_correlation = as.matrix(asset_Returns)
  
  # Get number of columns
  column_for_correlation = ncol(matrix_for_correlation)
  
  # Cut column and row names if there are longer than 4 characters
  names_for_correlation = abbreviate(colnames(matrix_for_correlation), 4)
  
  # Calculate correlation coefficients
  new_cor <- cor(matrix_for_correlation)
  
  # Create appropriate colour for each pair of correlation for heatmap
  ncolors_for_cor <- 10 * length(unique(as.vector(new_cor)))
  k_for_corr <- round(ncolors_for_cor/2)
  r_for_corr <- c(rep(0, k_for_corr), seq(0, 1, length = k_for_corr))
  g_for_corr <- c(rev(seq(0, 1, length = k_for_corr)), rep(0, k_for_corr))
  b_for_corr <- rep(0, 2 * k_for_corr)
  corrColorMatrix <- (rgb(r_for_corr, g_for_corr, b_for_corr))
  
  # Display heatmap
  image(x = 1:column_for_correlation,
        y = 1:column_for_correlation,
        z = new_cor[, column_for_correlation:1],
        col = corrColorMatrix,
        axes = FALSE, main = "",
        xlab = "",
        ylab = "")
  
  # Add labels for both axis
  axis(2, at = column_for_correlation:1,
       labels = colnames(matrix_for_correlation), las = 2)
  
  axis(1, at = 1:column_for_correlation,
       labels = colnames(matrix_for_correlation), las = 2)
  
  # Add title for heatmap
  title(main = "Heatmap for Portfolio's Correlations")
  
  # Box heatmap
  box()
  
  # Add correlation values as text strings to each heatmap cell
  x = y = 1:column_for_correlation
  n_x = n_y = length(y)
  xoy = cbind(rep(x, n_y),
              as.vector(matrix(y,
                               n_x,
                               n_y,
                               byrow = TRUE)))
  coord_for_corr = matrix(xoy,
                          n_x * n_y,
                          2,
                          byrow = FALSE)
  X_for_corr = t(new_cor)
  for (i in 1:(column_for_correlation * column_for_correlation)) {
    text(coord_for_corr[i, 1],
         coord_for_corr[column_for_correlation *
                          column_for_correlation + 1 - i,
                        2],
         round(X_for_corr[coord_for_corr[i,1],
                          coord_for_corr[i,2]],
               digits = 2),
         col = "white",
         cex = 0.5)
  }
}
# Test
heatmap_for_correlation(values_for_cor = df_portfolio)
