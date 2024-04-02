lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

p.cor.pairs <- function(x, details = F){ # Unique Portfolio Correlation values
  
  p <- NULL # Create an empty variable
  
  for (a in colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)]))
    
    p <- cbind(p, getSymbols(a, src = "yahoo", auto.assign = F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Eliminate NAs
  
  colnames(p) <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)])
  
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
  
  if (isFALSE(details)){ rownames(filtered_pairs) <- seq(nrow(filtered_pairs))
  
    colnames(filtered_pairs) <- c("Security 1", "Security 2", "Correlation")
    
    return(filtered_pairs) } else { # Descriptive Statistics
      
      d <- apply(as.data.frame(filtered_pairs[,3]), 2,
                 function(x) c(min(x), median(x), max(x), mean(x), sd(x)))
      
      rownames(d) <- c("Min","Median 50%","Max", "Mean", "Standard Deviation")
      colnames(d) <- "Summary"
      
      d }
}
p.cor.pairs(df_portfolio, details = T) # Test
