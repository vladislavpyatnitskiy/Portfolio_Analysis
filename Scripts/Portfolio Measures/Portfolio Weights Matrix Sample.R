p.weights.mix.calculator <- function(x){ # Matrix of portfolio weights
  
  a.names <- colnames(x[,1 + 3 * seq(31, from = 0)]) # Assign tickers
  
  # Take values of last observations
  p.weights <- x[,3+3*seq(31,from=0)][nrow(x),]/as.numeric(x[nrow(x),ncol(x)])
  
  x <- x[,3 + 3 * seq(31, from = 0)] # Assign total sum as main data frame
  
  colnames(x) <- a.names # Assign tickers to this data frame
  
  # Create a matrix to store the products of weights
  weight_product_matrix <- matrix(0,nrow=length(a.names),ncol=length(a.names))
  
  # Fill the matrix with weight products
  for (i in 1:length(a.names)) { for (j in 1:length(a.names)) {
    weight_product_matrix[i, j] <- p.weights[i] * p.weights[j] } }
  
  rownames(weight_product_matrix) <- a.names # Set row and column names
  colnames(weight_product_matrix) <- a.names
  
  return(weight_product_matrix) # Print the matrix
}
p.weights.mix.calculator(df_portfolio) # Test
