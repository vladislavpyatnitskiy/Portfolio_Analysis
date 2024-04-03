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
  cor_pairs <- which(upper.tri(cor_matrix, diag = TRUE), arr.ind = TRUE)
  
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
  
  s.min <- min(s) # Minimum value
  s.max <- max(s) # Maximum value
  
  # Parameters
  hist(s, main = "Portfolio Correlations Histogram", freq = F, breaks=100,
       ylab = "Probability", xlab = " Unique Correlation Values", las = 1,
       xlim = c(s.min, s.max), col = "navy", border = "white")
  
  for (n in seq(round(s.min, 1), round(s.max, 1), by=.05)){ # Add grey lines
    
    abline(v = n, col = "grey", lty = 3) } # Add Vertical lines
  
  abline(v = 0, col = "black", lwd = 2) # Add vertical line at x = 0
  
  lines(seq(round(s.min, 2), round(s.max, 2), by=.0001), # Normal Distribution
        dnorm(seq(round(s.min, 2), round(s.max, 2), by = .0001),
              mean(s), sd(s)), col = "red", lwd = 2)
  
  for (n in seq(0,100,1)){ abline(h=n,col="grey",lty=3) } # Horizontal lines
  
  axis(side = 1, at = seq(-1, 1, 0.1)) # Horizontal axis values
  
  box() # Define plot borders
}
p.hist.plt.cor(df_portfolio) # Test
