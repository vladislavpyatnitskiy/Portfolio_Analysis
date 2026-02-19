p.box.plt <- function(x){ # Box Plot for Portfolio
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x) %/% 3 + 1)] # Data
  
  v <- NULL # Variable for values
  
  for (n in 1:ncol(x)){ s <- x[,n] # Clean data and calculate logs
  
    v <- cbind(v, diff(log(s[apply(s,1,function(row) all(row !=0 )),]))[-1,]) } 
  
  colnames(v) <- colnames(x) # Give column names & generate plot

  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
                                   
  boxplot.matrix(v, main = "Fluctuations of Portfolio Securities", title = F,
                 col = "steelblue", las = 2, ylab = "Returns (%)") 
  
  abline(v = seq(ncol(v)), col = "grey", lty = 3) # Add vertical lines
  
  grid(nx = 1, ny = NULL, col = "grey", lty = "dotted", lwd = 1) # grid lines
  
  abline(h = 0, col = "black", lty = 3, lwd = 1) # Add horizontal line at 0
  
  axis(side = 4, las = 2) # Right y-axis
}
p.box.plt(df_portfolio) # Test
