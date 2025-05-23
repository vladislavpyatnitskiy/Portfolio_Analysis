p.bar.plt.weights.dividend <- function(x, sort = T, decreasing = T){ 
  
  tickers <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  x <- cumsum(x[,3 * seq(ncol(x) %/% 3, from = 1)]) # Calculate Cumulative Divs
  
  colnames(x) <- tickers # Assign tickers
  
  x <- cbind(x, rowSums(x, na.rm = T)) # Join with Total Sum
  
  colnames(x)[ncol(x)] <- "Total" # Give column name to total sum
  
  x <- x[,colSums(x) !=0] # Reduce column without dividends
  
  tickers <- colnames(x) # Assign tickers of securities without dividends
  
  x <- as.numeric(x[nrow(x),]) / as.numeric(x[nrow(x),ncol(x)]) # Find %
  
  x <- x[-length(x)] # Reduce column with total sum (100%)
  
  tickers <- tickers[-length(tickers)] # Reduce name with total sum (100%)
  
  v <- c(round(x * 100, 2)) # Data Frame with tickers & %
  
  names(v) <- tickers # Assign tickers
  
  if (isTRUE(sort)) { v <- sort(v, decreasing = decreasing) } # Sort
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975") # Add colour range
  
  B <- barplot(v, names.arg = names(v), horiz = F, ylab = "Percentage (%)",
               main="Dividend Allocation of Portfolio Securities", las = 2,
               ylim = c(0, ceiling(max(v))), col = C) # Create Bar Plot
  
  axis(side = 4, las = 2) # y-axes
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
  
  abline(v = B, col = "grey", lty = 3) # grid lines
  grid(nx = 1, ny = NULL, col = "grey", lty = "dotted", lwd = 1) 
  
  vs <- list(c(mean(v), median(v)), c("red", "green"))
  for (n in 1:2){ abline(h = vs[[1]][n], col = vs[[2]][n], lwd = 3) }
  
  legend(x="bottom",inset=c(0,-.22),cex=.85,bty="n",horiz=T,xpd=T,pch=15,
         legend=c((sprintf("Mean: %s %%", round(mean(v), 3))),
                  sprintf("Median: %s %%", round(median(v), 3))), col=vs[[2]])
  box() # Box
}
p.bar.plt.weights.dividend(df_portfolio_dividend) # Test
