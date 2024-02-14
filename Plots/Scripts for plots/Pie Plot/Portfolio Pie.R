# Function for Pie Plot generation
p.pie.plt <- function(x, main = NULL, sub = NULL, radius = 1){
  
  # Subset tickers from price columns
  tickers <- colnames((x[,1 + 3 * seq(31, from = 0)]))
  
  # Calculate portions
  pct <- as.data.frame(x[,3 + 3 * seq(31, from = 0)]) / x[,ncol(x)]
  
  # Select last period and transform portions into percentages
  pct <- as.numeric((round(pct[nrow(pct),] * 100)))
  
  # Add colour range
  colors37 = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d",
               "#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c",
               "#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d",
               "#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3",
               "#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d",
               "#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b",
               "#bd5975")
  # Pie Chart
  pie(pct, labels = c(sprintf("%s %s%%", tickers, pct)), col = colors37,
      main = main, sub = sub, radius = radius) 
}
p.pie.plt(df_portfolio, main = "Portfolio", radius = 1.5) # Test
