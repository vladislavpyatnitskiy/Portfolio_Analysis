p.pie.plt <- function(x, main = NULL, radius = 1){ # Pie Plot of Portfolio
  
  # Subset tickers from price columns
  A <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)])[-(ncol(x) %/% 3 + 1)]
  
  # Calculate portions
  P <- as.data.frame(x[,3 * seq(ncol(x) %/% 3, from = 1)]) / x[,ncol(x)]
  
  # Select last period and transform portions into percentages
  P <- as.numeric((round(P[nrow(P),] * 100)))
  
  # Add colour range
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975")
  
  pie(P, labels=c(sprintf("%s %s%%", A, P)), col=C, main=main, radius=radius) 
}
p.pie.plt(df_portfolio, main = "Portfolio", radius = 3) # Test
