p.weights.mix.calculator <- function(x){ # Matrix of portfolio weights
  
  A <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,1:(ncol(x) %/% 3)])
  
  # Take values of last observations
  w <- x[,3*seq(ncol(x)%/%3,from=1)][nrow(x),]/as.numeric(x[nrow(x),ncol(x)])
  
  x <- x[,3 + 3 * seq(31, from = 0)] # Assign total sum as main data frame
  
  colnames(x) <- A # Assign tickers to this data frame
  
  # Create a matrix to store the products of weights
  W <- matrix(0, nrow = length(A), ncol = length(A))
  
  # Fill the matrix with weight products
  for (i in 1:length(A)){ for (j in 1:length(A)){ W[i, j] <- w[i] * w[j] } }
  
  rownames(W) <- A # Set row and column names
  colnames(W) <- A
  
  W # Print the matrix
}
p.weights.mix.calculator(df_portfolio) # Test
