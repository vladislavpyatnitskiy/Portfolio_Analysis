# Function to generate barplot
brplt3 <- function(x, main = NULL, col = "blue"){
  
  # Subset values with secuirities' values
  x <- x[,1 + 3 * seq(31, from = 0)]
  
  # Create empty variable to contain values
  values_for_brplt <- NULL
  
  # For each column in data set
  for (n in 1:ncol(x)){
    
    # Values for security
    security <- x[,n]
    
    # Find zeros in data set
    security1 <- apply(security, 1, function(row) all(row !=0 ))
    
    # Reduce rows with zeros
    security2 <- security[security1,]
    
    # Calculate logs
    security2 <- diff(log(security2))[-1,]
    
    # Calculate return for the ownership period
    security2 <- exp(sum(security2)) - 1
    
    # Put in newly created variable
    values_for_brplt <- cbind(values_for_brplt, security2)
  }
  
  # Give column names
  colnames(values_for_brplt) <- colnames(x)
  
  # make data frame for column names
  x1 <- as.data.frame(values_for_brplt)
  
  # sort values for column names
  x1 <- sort(x1, decreasing = T)
  
  # Move names to new variable 
  tickers_for_barplot <- colnames(x1)
  
  # Make data numeric for barplot
  values_for_brplt <- as.numeric(values_for_brplt)
  
  # Sort values for barplot
  values_for_brplt <- sort(values_for_brplt, decreasing = T)
  
  # Create barplot
  barplot(values_for_brplt,
          names.arg = tickers_for_barplot,
          horiz = T,
          las=1,
          col = col,
          main = main,
          sub = "Source: Yahoo! Finance",
          xlab = "Returns",
          ylab = ""
  )
}
# Test
brplt3(df_portfolio, main = "Securities Performance")