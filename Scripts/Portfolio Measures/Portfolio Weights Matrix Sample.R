p.weights.mix.calculator <- function(x){ # Matrix of portfolio weights
  
  tickers <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)])
  
  # Take values of last observations
  w <- x[,3*seq(ncol(x)%/%3,from=1)][nrow(x),]/as.numeric(x[nrow(x),ncol(x)])
  
  x <- x[,3 + 3 * seq(31, from = 0)] # Assign total sum as main data frame
  
  colnames(x) <- tickers # Assign tickers to this data frame
  
  # Create a matrix to store the products of weights
  weight_product_matrix <- matrix(0,nrow=length(tickers),ncol=length(tickers))
  
  # Fill the matrix with weight products
  for (i in 1:length(tickers)) { for (j in 1:length(tickers)) {
      weight_product_matrix[i, j] <- w[i] * w[j] } }
  
  rownames(weight_product_matrix) <- tickers # Set row and column names
  colnames(weight_product_matrix) <- tickers
  
  return(weight_product_matrix) # Print the matrix
}
p.weights.mix.calculator(df_portfolio) # Test
