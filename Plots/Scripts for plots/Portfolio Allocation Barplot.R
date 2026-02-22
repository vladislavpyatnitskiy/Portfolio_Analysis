p.bar.plt.weights <- function(x, sort = F, decreasing = T){ # Weights Bar Plot
  
  sec_sum = x[,3 * seq(ncol(x) %/% 3, from=1)][nrow(x),] # Sum of security
  
  total_sum = as.numeric(x[nrow(x),ncol(x)]) # Total sum of all portfolio 
  
  pct <- as.data.frame(sec_sum / total_sum) # Percentages for stocks
  
  pct <- as.numeric(((pct[nrow(pct),] * 100))) # Percentage of last observation
  
  names(pct) <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  if (sort) pct <- sort(pct, decreasing = decreasing)
  
  С = c(
    "#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
    "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
    "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
    "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
    "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
    "#895c8b","#bd5975"
    ) # Add colour range

  par(mar = c(5, rep(4, 3))) # Define borders of the plot to fit right y-axis
  
  B <- barplot(
    pct, 
    names.arg = names(pct), 
    horiz = F, 
    col = С, 
    las = 2,
    main = "Allocation of Portfolio Securities (%)",
    ylim = c(0, ceiling(max(pct)))
    ) # Bar plot
  
  grid(nx = 1, ny = NULL, col = "grey", lty = 3, lwd = 1) # Horizontal lines
  
  abline(v = B, col = "grey", lty = 3) # Vertical lines of grid
  
  vals <- list(c(mean(pct), median(pct)), c("red", "green"))
  for (n in 1:2){ abline(h = vals[[1]][n], col = vals[[2]][n], lwd = 3) }
  
  legend(
    x = "bottom",
    inset = c(0, -.2),
    cex = .85,
    bty = "n",
    horiz = T,
    col = vals[[2]],
    xpd = T,
    pch = 15,
    legend = c(
      (
        sprintf("Mean: %s %%", round(mean(pct), 3))),
        sprintf("Median: %s %%", round(median(pct), 3))
      ),
    )
  
  axis(side = 4, las = 2) # Right y-axis
  
  box()                       
}
p.bar.plt.weights(df_portfolio, sort = T, decreasing = T) # Test
