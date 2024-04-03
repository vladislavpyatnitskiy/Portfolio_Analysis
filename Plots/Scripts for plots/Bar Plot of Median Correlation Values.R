lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

p.bar.plt.cor <- function(x){ # Bar Plot with Median Correlation Values
  
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
  
  # Add colour range
  colors37 = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d",
               "#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c",
               "#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d",
               "#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3",
               "#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d",
               "#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b",
               "#bd5975")
  
  # Create barplot
  bar.plt.script <- barplot(l[,1],
                            names.arg = rownames(l),
                            horiz = F,
                            col = colors37,
                            main = "Median Correlations of Portfolio Stocks",
                            ylab = "Median Correlation Level",
                            ylim = c(0, ceiling(max(l[,1]))),
                            las = 2)
  # Y axis
  p.seq <- seq(0, .95, .05)
  axis(side = 2, at = p.seq, las = 1, labels = p.seq)
  axis(side = 4, at = seq(0, 1, .05), las = 1, labels = seq(0, 1, .05))
  
  for (n in p.seq){ abline(h = n, col ="grey", lty = 3) } # Horizontal lines
  abline(v = bar.plt.script, col = "grey", lty = 3) # Vertical lines
  
  c <- c("black", "red", "orange", "gold", "greenyellow", "green", "limegreen")
  v <- c(0.5, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2)
  
  for (n in 1:length(v)){ abline(h = v[n], col = c[n], lwd = 2) } # Lines
  
  par(mar = c(5, 5, 5, 5)) # Define borders of the plot
  
  box() # Add box
  
  h <- l[,1] # Column for median values
  
  names(h) <- rownames(l) # Assign tickers to vector
  
  m <- NULL # Write advices about securities according to correlations
  
  if (isFALSE(identical(names(which(h > 0.5)), character(0)))){
    
    m <- c(m, paste("Sell these Assets:", toString(names(which(h > .5))))) }
  
  if (isFALSE(identical(names(which(h > .45 & h < .5)), character(0)))){
    
    m <- c(m, paste("Sell one of these Assets:",
                    toString(names(which(h > .45 & h < .5))))) }
  
  if (isFALSE(identical(names(which(h > .4 & h < .45)), character(0)))){
    
    m <- c(m, paste("Consider to sell one of these Assets:",
                    toString(names(which(h > .4 & h < .45))))) }
  
  if (isFALSE(identical(names(which(h > .35 & h < .4)), character(0)))){
    
    m <- c(m, paste("Check these Assets:",
                    toString(names(which(h > .35 & h < .4))))) }
  
  if (isFALSE(identical(names(which(h > .3 & h < .35)), character(0)))){
    
    m <- c(m, paste("OK to keep Assets:",
                    toString(names(which(h > .3 & h < .35))))) }
  
  if (isFALSE(identical(names(which(h > .25 & h < .3)), character(0)))){
    
    m <- c(m, paste("Good Assets:",
                    toString(names(which(h > .25 & h < .3))))) }
  
  if (isFALSE(identical(names(which(h > .2 & h < .25)), character(0)))){
    
    m <- c(m, paste("Great Assets:",
                    toString(names(which(h > .2 & h < .25))))) }
  
  if (isFALSE(identical(names(which(h < .2)), character(0)))){
    
    m <- c(m, paste("Best Assets:", toString(names(which(h < .2))))) }
  
  m # Display
}
p.bar.plt.cor(df_portfolio) # Test
