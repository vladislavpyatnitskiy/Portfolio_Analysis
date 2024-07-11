p.box.plt <- function(x){ # Box Plot for Portfolio
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x)%/%3+1)] # Data
  
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
  
  if (m > 0 && m < 1){ mn <- 1 * 10 ^ (nchar(m) - 4) }
  
  else if (m > 1 && m < 2){ mn <- 2 * 10 ^ (nchar(m) - 4) }
  
  else if (m > 2 && m < 5){ mn <- 5 * 10 ^ (nchar(m) - 4) }
  
  abline(h = seq(-1,1,by=mn)[-match(0, seq(-1,1,by=mn))], col="grey", lty=3) 
  
  axis(side=2, at=seq(round(min(l), 1)-mn, round(max(l), 1)+mn, mn*2), las = 1)
  axis(side=4, at=seq(round(min(l), 1)-2*mn, round(max(l), 1)+2*mn, mn), las=1)
}
p.box.plt(df_portfolio) # Test
