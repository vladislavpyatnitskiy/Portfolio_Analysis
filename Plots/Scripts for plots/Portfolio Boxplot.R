# Function to create Boxplot for Portfolio
p.box.plt <- function(x){ x <- x[,1 + 3 * seq(31, from = 0)] # Extract data

  v <- NULL # Variable for values
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column in data set
  
    # Clean data from NA & zeros and calculate logs & Put it in data frame
    v <- cbind(v, diff(log(s[apply(s,1,function(row) all(row !=0 )),]))[-1,]) } 
    
  colnames(v) <- colnames(x) # Give column names & generate plot
  
  boxplot.matrix(v, main = "Securities Returns", col = "steelblue", title = F,
                 las = 2, ylab = "Returns") 
  
  abline(h = 0, col = "grey", lty = 3) # Add horizontal line
}
p.box.plt(df_portfolio) # Test
