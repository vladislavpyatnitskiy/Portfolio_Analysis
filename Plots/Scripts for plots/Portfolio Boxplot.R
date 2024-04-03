p.box.plt <- function(x){ # Box Plot for Portfolio
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x)%/%3+1)] # Data

  v <- NULL # Variable for values
  
  for (n in 1:ncol(x)){ s <- x[,n] # Clean data and calculate logs
  
    v <- cbind(v, diff(log(s[apply(s,1,function(row) all(row !=0 )),]))[-1,]) } 
  
  colnames(v) <- colnames(x) # Give column names & generate plot
  
  boxplot.matrix(v, main = "Fluctuations of Portfolio Securities", title = F,
                 col = "steelblue", las = 2, ylab = "Returns (%)") 
  
  for (n in 1:2){ axis(side = n * 2, at = seq(-1, 1, .1), las = 1) } # y-axis
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
  
  abline(v = seq(ncol(v)), col = "grey", lty = 3) # Add vertical lines
  abline(h = 0, col = "black", lty = 3) # Add horizontal line at 0
  abline(h = seq(-1, -.1, .1), col = "grey", lty = 3) # horizontal line < 0
  abline(h = seq(.1, 1, .1), col = "grey", lty = 3) # horizontal line > 0
}
p.box.plt(df_portfolio) # Test
