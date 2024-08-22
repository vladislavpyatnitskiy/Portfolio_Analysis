lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

p.cor.advices <- function(x){ # Bar Plot with Median Correlation Values
  
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
  f <- unique_pairs[unique_pairs$Correlation != 1, ]
  
  rownames(f) <- seq(nrow(f)) # Put ascending numbers into row names
  
  colnames(f) <- c("Security 1", "Security 2", "Correlation") # Column names
  
  cor.names <- unique(c(f[,1], f[,2])) # Show only unqiue tickers
  
  l <- NULL # Calculate median and mean correlation for each security
  
  for (n in 1:length(cor.names)){ k <- cor.names[n] # Select ticker
  
    v <- f[f$`Security 1` == k | f$`Security 2` == k, ] # ticker's correlations
    
    l <- rbind.data.frame(l, cbind(median(v[,3]), mean(v[,3]))) } # Join
  
  rownames(l) <- cor.names
  colnames(l) <- c("Median", "Average") # Column names
  
  l <- l[order(l$Median), ] # Sort in an ascending way
  
  h <- l[,1] # Column for median values
  
  names(h) <- rownames(l) # Assign tickers to vector
  
  m <- NULL # Write advices about securities according to correlations
  
  j <- list(list(.5, 1, "Sell these Assets:"),
            list(.45, .5, "Sell one of these Assets:"), 
            list(.4, .45, "Consider to sell one of these Assets:"),
            list(.35, .4, "Check these Assets:"),
            list(.3, .35, "OK to keep Assets:"), list(.25, .3, "Good Assets:"),
            list(.2, .25, "Great Assets:"), list(-1, .2, "Best Assets:")) 
  
  for (n in 1:length(j)){ # Messages indicating correlation levels for stocks
    
    if (isFALSE(identical(names(which(h > j[[n]][[1]] & h < j[[n]][[2]])),
                          character(0)))){
      m <- c(m,
             paste(j[[n]][[3]],
                   toString(names(which(h>j[[n]][[1]] & h<j[[n]][[2]]))))) } }
  m # Display
}
p.cor.advices(df_portfolio) # Test
