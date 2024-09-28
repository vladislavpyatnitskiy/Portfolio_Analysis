lapply(c("quantmod","timeSeries"),require,character.only=T) # Libraries

p.hist.plt.cor <- function(x){ # Histogram with Portfolio Correlation values
  
  p <- NULL # Create an empty variable & Loop for data extraction
  
  for (a in colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]) 
    
    p <- cbind(p, getSymbols(a, src = "yahoo", auto.assign = F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Eliminate NAs
  
  colnames(p) <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  # Calculate correlation matrix
  cor_matrix <- cor(as.matrix(diff(log(as.timeSeries(p)))[-1,]))
  
  # Extract unique pairs and their correlations
  cor_pairs <- which(upper.tri(cor_matrix, diag = T), arr.ind = T)
  
  # Put them into one data frame
  unique_pairs <- data.frame(Variable1 = rownames(cor_matrix)[cor_pairs[, 1]],
                             Variable2 = rownames(cor_matrix)[cor_pairs[, 2]],
                             Correlation = cor_matrix[cor_pairs]
  )
  # Filter out pairs with correlation equal to 1
  filtered_pairs <- unique_pairs[unique_pairs$Correlation != 1, ]
  
  rownames(filtered_pairs) <- seq(nrow(filtered_pairs)) # Row numbers
  
  colnames(filtered_pairs) <- c("Security 1", "Security 2", "Correlation")
  
  s <- filtered_pairs[,3]
  
  h <- hist(s, main="Histogram of Portfolio Correlations", ylab = "Likelihood",
            xlab = "Unique Correlation Values", xlim = c(min(s), max(s)),
            col = "navy", las = 1, border = "white", breaks = 100, freq = F)
  
  m <- round(min(s)*-1 + max(s),1)/10^(nchar(round(min(s)*-1 + max(s),1))-2)
  
  d <- c(0,.0001,.0002,.0005,.001,.002,.005,.01,.02,.05,.1,.2,.5,1) 
  
  for (n in 1:length(d) - 1){ if (m > d[n] && m < d[n + 1]){
    
      mn <- d[n + 1] } else { next } }
  
  M <- round(max(h$density)) / 10 ^ (nchar(round(max(h$density))))
  
  i <- c(0, 1, 2, 5) # Calculate intervals for lines and axes
  
  for (n in 1:length(i) - 1){ if (M >= i[n] && M < i[n + 1]){
    
      mx <- i[n + 1] * 10 ^ (nchar(M) - 3) } else { next } }
  
  abline(v = mean(s), col = "lightblue", lwd = 2) # Add vertical lines
  for (n in seq(-1, 1, by = mn)){  abline(v = n, col = "grey", lty = 3) } 
  
  abline(h = 0) # Horizontal line at 0 and other above ones
  for (n in seq(mx, 100, by = mx)){ abline(h = n, col = "grey", lty = 3) }
  
  curve(dnorm(x, mean = mean(s), sd = sd(s)), col = "red", lwd = 3, add = T)
  
  axis(side = 1, at = seq(-1, 1, 0.1)) # Horizontal axis values
  
  box()
}
p.hist.plt.cor(df_portfolio) # Test
