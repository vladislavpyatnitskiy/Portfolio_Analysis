# Function to calculate Multi-Asset Value-at-Risk
MVaR <- function(x, VaR = 0.01, portfolio_weights, lg = T, ndays = 1){
  
  # Make logs if needed
  if (isTRUE(lg)) { x = diff(log(x))[-1,] }
  
  # Calculate means
  test_mean <- as.matrix(apply(x, 2, function(col) mean(col)))
  
  # Calculate sum of products for means and weights
  return_prod <- crossprod(x = test_mean, y = portfolio_weights)
  
  # Calculate variances
  test_variance <- as.matrix(apply(x, 2, function(col) var(col)))
  
  # Calculate sum of products for variances and square weights
  var_prod <- crossprod(x = test_variance, y = portfolio_weights ^ 2)
  
  # Assign names
  asset_names <- colnames(x)
  
  # Create a matrix to store the products of weights
  weight_product_matrix <- matrix(0, nrow = length(asset_names),
                                  ncol = length(asset_names))
  
  # Fill the matrix with weight products
  for (i in 1:length(asset_names)) {
    for (j in 1:length(asset_names)) {
      weight_product_matrix[i, j] <- portfolio_weights[i] *
        portfolio_weights[j] } }
  
  # Set row and column names
  rownames(weight_product_matrix) <- asset_names
  colnames(weight_product_matrix) <- asset_names
  
  # Put weights and covariances in nested list
  list_wghts_cov <- list(weight_product_matrix, cov(x))
  
  # Create an empty variable to put covariances and weights
  wghts_cov <- NULL
  
  # For each table in tables
  for (n in 1:length(list_wghts_cov)){
    
    # Extract unique pairs and their correlations
    cor_pairs <- which(upper.tri(list_wghts_cov[[n]],
                                 diag = TRUE), arr.ind = TRUE)
    
    # Put them into one data frame
    unique_pairs <- data.frame(
      Variable1 = rownames(list_wghts_cov[[n]])[cor_pairs[, 1]],
      Variable2 = rownames(list_wghts_cov[[n]])[cor_pairs[, 2]],
      Correlation = (list_wghts_cov[[n]])[cor_pairs]
    )
    
    # Filter out pairs where the tickers are not the same
    different_tickers_pairs <- unique_pairs[unique_pairs$Variable1 !=
                                              unique_pairs$Variable2, ]
    
    # Concatenate First_Name and Last_Name with a space in between
    different_tickers_pairs$Pair <- paste(different_tickers_pairs$Variable1,
                                          different_tickers_pairs$Variable2)
    
    # Create new tables where put pairs in first column and values in second
    new_data_set <- different_tickers_pairs[,4:3]
    
    # Transform into data frame format
    new_data_set <- as.data.frame(new_data_set)
    
    # When working with first table assign to empty variable
    if (is.null(wghts_cov)){ wghts_cov <- new_data_set } else {
      
      # Otherwise perform inner join
      wghts_cov_join <- merge(x = wghts_cov, y = new_data_set, by = "Pair")  }
  }
  
  # Sum of products for covariances and weights
  cov_prod <- crossprod(x = wghts_cov_join[,2], y = wghts_cov_join[,3]) * 2
  
  # Portfolio's standard deviation
  portfolio_risk <- (var_prod + cov_prod) ^ 0.5
  
  # Final calculation
  VaR_value <- (1 - qnorm(VaR, mean = 1 + return_prod, sd = portfolio_risk)) *
    (ndays ^ 0.5)
  
  # Prepare it for presentation
  VaR_display <- sprintf("Portfolio MVaR is %s%% for the next %s days.",
                         round(VaR_value, 4) * 100, ndays)
  
  # Display value
  return(VaR_display)
}
# Test
MVaR(x = stock_data, VaR = 0.01, portfolio_weights = c(0.4, 0.3, 0.2, 0.1),
     lg = T, ndays = 10)
