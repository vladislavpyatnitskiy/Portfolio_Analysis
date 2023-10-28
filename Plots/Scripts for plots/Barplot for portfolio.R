# Function to generate barplot
p.bar.plt <- function(x, main = NULL, col = "blue", v.bar.plt = NULL){
  
  # Subset values with secuirities' values
  x <- x[,1 + 3 * seq(31, from = 0)]
  
  # For each column in data set
  for (n in 1:ncol(x)){ s <- x[,n]
    
    # Clean data from NA & zeros and calculate logs
    security <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    # Calculate return for the ownership period and put it in data frame
    v.bar.plt <- cbind(v.bar.plt, exp(sum(security)) - 1) }
  
  colnames(v.bar.plt) <- colnames(x) # Give column names
  
  # Sort values for column names and move names to new variable 
  p.tickers <- colnames(sort(as.data.frame(v.bar.plt), decreasing = T))
  
  # Make data numeric for barplot and sort values for barplot
  v.bar.plt <- sort(as.numeric(v.bar.plt), decreasing = T)
  
  # Create barplot
  barplot(v.bar.plt,
          names.arg = p.tickers,
          horiz = T,
          las= 1,
          col = col,
          main = main,
          xlab = "Returns",
          ylab = "",
          xlim = c(round(min(v.bar.plt), 1), round(max(v.bar.plt), 1)))
  
  # Create line going through bars to facilitate link between bar & ticker
  abline(h = barplot(v.bar.plt,
                     names.arg = p.tickers,
                     horiz = T,
                     las=1,
                     col = col,
                     main = main,
                     xlab = "Returns",
                     ylab = "",
                     xlim = c(round(min(v.bar.plt), 1),
                              round(max(v.bar.plt), 1))),col = "grey",lty = 3)
  
  # Add grey lines for fast visual percentage calculation
  for (n in seq(round(min(v.bar.plt), 1), round(max(v.bar.plt), 1), by = .1)){ 
    abline(v = n, col = "grey", lty = 3) }
  
  # Modify axis so intervals between them is 0.1
  axis(side = 1, at = seq(round(min(v.bar.plt), 1),
                          round(max(v.bar.plt), 1), by = .1), las = 1)
  
  box() # Make borders for plot
}
# Test
p.bar.plt(df_portfolio, main = "Securities Performance")
