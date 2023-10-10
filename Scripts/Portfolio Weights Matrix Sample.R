# Function to make matrix of portfolio weights
pw_calculator <- function(asset_names, portfolio_weights){
  
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
  
  # Print the matrix
  return(weight_product_matrix)
}
# Test
pw_calculator(asset_names = c("AAPL", "MSFT", "GOOGL"),
              portfolio_weights = c(0.4, 0.3, 0.2))
