library("rvest") # Library

p.pie.plt.index <- function(x){ # Pie of Portfolio Securitites by Index
  
  tickers <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)])[-(ncol(x)%/%3+1)]
  
  pct <- as.data.frame(x[,3 * seq(ncol(x) %/% 3, from = 1)]) / x[,ncol(x)]
  
  colnames(pct) <- tickers # Assign tickers
  
  pct <- t(round(pct[nrow(pct),] * 100)) # Transform last period into percents
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975") # Add colour range
  
  colnames(pct) <- "Portion" # Assign column name
  
  l <- NULL # Create list
  
  for (n in 1:length(tickers)){ # Read HTML & extract necessary info
    
    p<-read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d",tickers[n])) 
    
    price.yahoo1 <- p %>% html_nodes('table') %>% .[[10]] -> tab # Assign Table 
    
    y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    l <- rbind(l, y[grep("Index", y) + 1]) } # Add to list
  
  colnames(l) <- "Index" # Assign column name
  rownames(l) <- tickers # Assign tickers
  
  for (n in 1:nrow(l)){ l[n,1]<-gsub("-","Not Included in Any Index",l[n,1]) }
  
  pie.df <- data.frame(l, pct) # Form data frame
  pie.df <- aggregate(Portion ~ Index, data=pie.df, sum) # Conditional sum
  
  pie(pie.df[,2], labels=c(sprintf("%s : %s%%", pie.df[,1], pie.df[,2])), col=C,
      main="Portions of Portfolio Securities by Index",radius=1.5) # Pie Chart
}
p.pie.plt.index(df_portfolio) # Test
