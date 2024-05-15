p.bar.plt.weights <- function(x, sort=F, decreasing=T){ # Weights via Bar Plot
  
  pct <- as.data.frame(x[,3 * seq(ncol(x) %/% 3, from = 1)][nrow(x),]/
                         as.numeric(x[nrow(x),ncol(x)]))
  
  pct <- as.numeric(((pct[nrow(pct),] * 100))) # Percentage of last observation
  
  names(pct) <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  if (isTRUE(sort)) { pct <- sort(pct, decreasing = decreasing) }
  
  С = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975") # Add colour range
  
  # Create bar plot
  B <- barplot(pct, names.arg = names(pct), horiz = F, col = С, las = 2,
               main = "Allocation of Portfolio Securities",
               ylab = "Percentage (%)", ylim = c(0, ceiling(max(pct))))
  
  p.seq <- seq(0, ceiling(max(pct)), .5) # Y axis
  
  for (n in 1:2){ axis(side = n*2, at=p.seq, las=1, labels=p.seq) } # y-axes
  
  # Add grey lines for fast visual percentage calculation
  for (n in seq(0, ceiling(max(pct)), .5)){ abline(h = n, col ="grey",lty = 3)}
  abline(v = B, col = "grey", lty = 3)
  
  abline(h = mean(pct), col = "red", lwd = 3) # Mean percentage line
  abline(h = median(pct), col = "green", lwd = 3) # Median percentage line
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
  
  legend(x = "bottom", inset = c(0, -.22), cex = .85, bty = "n", horiz = T,
         legend = c((sprintf("Mean: %s %%", round(mean(pct), 3))),
                    sprintf("Median: %s %%", round(median(pct), 3))),
         col = c("red", "green"), xpd = T, pch = 15)
  box()                       
}
p.bar.plt.weights(df_portfolio, sort = T, decreasing = T) # Test
