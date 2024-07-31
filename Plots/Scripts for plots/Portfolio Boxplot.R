p.box.plt <- function(x){ # Box Plot for Portfolio
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x) %/% 3 + 1)] # Data
  
  v <- NULL # Variable for values
  
  for (n in 1:ncol(x)){ s <- x[,n] # Clean data and calculate logs
  
    v <- cbind(v, diff(log(s[apply(s,1,function(row) all(row !=0 )),]))[-1,]) } 
    
  colnames(v) <- colnames(x) # Give column names & generate plot
  
  boxplot.matrix(v, main = "Fluctuations of Portfolio Securities", title = F,
                 col = "steelblue", las = 2, ylab = "Returns (%)") 
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
  
  abline(v = seq(ncol(v)), col = "grey", lty = 3) # Add vertical lines
  abline(h = 0, col = "black", lty = 3) # Add horizontal line at 0
  
  l <- as.numeric(v)[!is.na(as.numeric(v))] # Get values & define intervals
  
  m <- round(min(l) * -1 + max(l),0)/10^(nchar(round(min(l) * -1 + max(l),0)))
  
  i <- c(0, 1, 2, 5) # Calculate intervals for lines and axes
  
  for (n in 1:length(i) - 1){ if (m > i[n] && m < i[n + 1]){
    
      mn <- i[n + 1] * 10 ^ (nchar(m) - 4) } else { next } }
  
  abline(h = seq(-1,1,by=mn)[-match(0, seq(-1,1,by=mn))], col="grey", lty=3) 
  
  for (n in 1:2){ axis(side=2*n, at=seq(round(min(l),1)-mn*n,
                                        round(max(l),1)+mn*n, mn*2/n), las=1) }
}
p.box.plt(df_portfolio) # Test
