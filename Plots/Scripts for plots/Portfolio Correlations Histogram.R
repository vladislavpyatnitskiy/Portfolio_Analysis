lapply(c("quantmod","timeSeries"),require,character.only=T) # Libraries

p.hist.plt.cor <- function(x){ # Histogram with Portfolio Correlation values
  
  p <- NULL # Create an empty variable & Loop for data extraction
  
  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  x <- x[-grep("VSTO", x)] 
  x <- x[-grep("ARCH", x)] 
  
  for (a in x){ p <- cbind(p, getSymbols(a, src="yahoo", auto.assign=F)[,4]) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Eliminate NAs
  
  colnames(p) <- x
  
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
  
  abline(v = mean(s), col = "lightblue", lwd = 2) # Add vertical lines
  
  grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
  
  abline(h = 0) # Horizontal line at 0 and other above ones
  
  curve(dnorm(x, mean = mean(s), sd = sd(s)), col = "red", lwd = 3, add = T)
  
  box()
}
p.hist.plt.cor(df_portfolio) # Test
