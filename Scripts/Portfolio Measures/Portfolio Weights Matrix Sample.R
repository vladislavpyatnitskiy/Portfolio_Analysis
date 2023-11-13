# Function to make matrix of portfolio weights
p.weights.calculator <- function(a.names, p.weights){
  
  # Create a matrix to store the products of weights
  weight_product_matrix <- matrix(0,nrow=length(a.names),ncol=length(a.names))
  
  # Fill the matrix with weight products
  for (i in 1:length(a.names)) { for (j in 1:length(a.names)) {
      weight_product_matrix[i, j] <- p.weights[i] * p.weights[j] } }
  
  rownames(weight_product_matrix) <- a.names # Set row and column names
  colnames(weight_product_matrix) <- a.names
  
  return(weight_product_matrix) # Print the matrix
}
# Test
p.weights.calculator(a.names = c("AAPL","MSFT","GOOGL"),p.weights=c(.4,.3,.2))
