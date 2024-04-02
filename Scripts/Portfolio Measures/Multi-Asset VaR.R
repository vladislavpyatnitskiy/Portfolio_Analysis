p.MVaR <- function(x, VaR = 0.01, ndays = 1){ # MVaR for portfolio
  
  p.weights <- x[,3*seq(ncol(x)%/%3,from=1)][nrow(x),]/
    as.numeric(x[nrow(x),ncol(x)])
  
  x <- x[,1 + 3*seq(ncol(x) %/% 3, from = 0)][,1:(ncol(x)%/%3)] # Subset data
  
  x <- diff(log(x[apply(x, 1, function(row) all(row !=0 )),]))[-1,]
  
  # Calculate sum of products for means and weights
  return_prod <- crossprod(x = as.matrix(apply(x, 2, function(col) mean(col))),
                           y = as.numeric(p.weights))
  
  # Calculate sum of products for variances and square weights
  var_prod <- crossprod(x = as.matrix(apply(x, 2, function(col) var(col))),
                        y = as.numeric(p.weights) ^ 2)
  
  a.names <- colnames(x) # Assign names
  
  # Create a matrix to store the products of weights
  weight_product_matrix <- matrix(0,nrow=length(a.names),ncol=length(a.names))
  
  # Fill the matrix with weight products
  for (i in 1:length(a.names)) { for (j in 1:length(a.names)) {
    weight_product_matrix[i, j] <- p.weights[i] * p.weights[j] } }
  
  rownames(weight_product_matrix) <- a.names # Set row and column names
  colnames(weight_product_matrix) <- a.names
  
  # List with weights and covariances
  list_wghts_cov <- list(weight_product_matrix, cov(x)) 
  
  wghts_cov <- NULL # Create an empty variable to put covariances and weights
  
  for (n in 1:length(list_wghts_cov)){ # For each table in tables
    
    # Extract unique pairs and their correlations
    cor_pairs <- which(upper.tri(list_wghts_cov[[n]], diag = T), arr.ind = T)
    
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
    df <- as.data.frame(different_tickers_pairs[,4:3]) 
    
    if (is.null(wghts_cov)){ wghts_cov <- df } else { # Join
      
      wghts_cov_join <- merge(x = wghts_cov, y = df, by = "Pair")  } }
  
  # Sum of products for covariances and weights
  cov_prod <- crossprod(x = wghts_cov_join[,2], y = wghts_cov_join[,3]) * 2
  
  risk <- (var_prod + cov_prod) ^ 0.5 # Portfolio standard deviation
  
  # Final calculation
  v.VaR <- (1 - qnorm(VaR, mean = 1 + return_prod, sd = risk)) * (ndays ^ 0.5)
  
  # Prepare it for presentation
  sprintf("Portfolio MVaR is %s%% for next %s days.",round(v.VaR,4)*100,ndays)
}
p.MVaR(df_portfolio, VaR = 0.01, ndays = 10) # Test
