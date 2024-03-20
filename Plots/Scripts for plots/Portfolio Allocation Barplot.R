# Function to plot portfolio weights via barplot
p.bar.plt.weights <- function(x, sort = F, decreasing = T){
  
  pct <- as.data.frame(x[,3+3*seq(31,from=0)])/x[,ncol(x)] # Calculate weights
  
  # Select last period and transform portions into percentages
  pct <- as.numeric(((pct[nrow(pct),] * 100)))
  
  names(pct) <- colnames((x[,1 + 3 * seq(31, from = 0)])) # Tickers
  
  if (isTRUE(sort)) { pct <- sort(pct, decreasing = decreasing) }
  
  # Add colour range
  colors37 = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d",
               "#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c",
               "#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d",
               "#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3",
               "#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d",
               "#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b",
               "#bd5975")
  
  # Create barplot
  bar.plt.script <- barplot(pct,
                            names.arg = names(pct),
                            horiz = F,
                            col = colors37,
                            main = "Allocation of Securities in Portfolio",
                            ylab = "Percentage (%)",
                            ylim = c(0, ceiling(max(pct))),
                            las = 2)

  p.seq <- seq(0, ceiling(max(pct)), .5) # Y axis
  
  for (n in 1:2){ axis(side = n*2, at=p.seq, las=1, labels=p.seq) } # y-axes

  # Add grey lines for fast visual percentage calculation
  for (n in seq(0, ceiling(max(pct)), .5)){ abline(h = n, col ="grey",lty = 3)}
  abline(v = bar.plt.script, col = "grey", lty = 3)
  
  abline(h = mean(pct), col = "red", lwd = 3) # Mean percentage line
  abline(h = median(pct), col = "green", lwd = 3) # Median percentage line
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
  
  if (isTRUE(decreasing)){ # Box with legend with mean and median on top right
    
    legend("topright", legend=c((sprintf("Mean:    %s %%", mean(pct))),
                                sprintf("Median: %s %%",round(median(pct),3))),
           fill = c("red", "green"), cex =.75, bty = "n")
    
    } else { # Box with legend with mean and median on top left
      
      legend("topleft", legend=c((sprintf("Mean:    %s %%",mean(pct))),
                                 sprintf("Median: %s %%",
                                         round(median(pct),3))),
             fill = c("red", "green"), cex = .75, bty = "n") }
  box()                       
}
p.bar.plt.weights(df_portfolio, sort = T, decreasing = T) # Test
