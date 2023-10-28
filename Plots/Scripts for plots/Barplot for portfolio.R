# Function to generate barplot
p.bar.plt <- function(x, main = NULL, col = "blue", v.bar.plt = NULL){
  
  x <- x[,1 + 3 * seq(31, from = 0)] # Subset values with secuirities' values
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column in data set
    
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
  bar.plt.script <- barplot(v.bar.plt,
                            names.arg = p.tickers,
                            horiz = T,
                            las= 1,
                            col = col,
                            main = main,
                            xlab = "Returns",
                            ylab = "",
                            xlim = c(round(min(v.bar.plt), 1),
                                     round(max(v.bar.plt), 1)))
  # Add grey dotted lines
  abline(h = bar.plt.script, col = "grey",lty = 3) # through bars
  
  p.seq <- seq(round(min(v.bar.plt), 1), round(max(v.bar.plt), 1), by = .1)
  
  for (n in p.seq){ abline(v = n, col = "grey", lty = 3) } # vertical bars
  
  axis(side = 1, at = p.seq, las = 1) # configure x axis
  
  box() # Make borders for plot
}
# Test
p.bar.plt(df_portfolio, main = "Securities Performance")
