# Function to create Boxplot for Portfolio
p.box.plt <- function(x){ x <- x[,1 + 3 * seq(31, from = 0)] # Extract data
  
  v.box.plt <- NULL # Variable for values
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column in data set
  
    # Clean data from NA & zeros and calculate logs
    security <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    v.box.plt <- cbind(v.box.plt, security) } # Put it in data frame

  colnames(v.box.plt) <- colnames(x) # Give column names
  
  boxplot.matrix(v.box.plt, main = "Securities Performance", title = F, las = 2,
                 col = "steelblue", ylab = "Returns") 
  
  abline(h = 0, col = "grey", lty = 3) # Add horizontal line
}
p.box.plt(df_portfolio) # Test
