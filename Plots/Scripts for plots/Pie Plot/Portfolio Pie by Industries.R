library("rvest") # Library

p.pie.plt.industry <- function(x, main = NULL, sub = NULL){
  
  tickers <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)])[-(ncol(x)%/%3+1)]
  
  pct <- as.data.frame(x[,3 * seq(ncol(x) %/% 3, from = 1)]) / x[,ncol(x)]
  
  colnames(pct) <- tickers # Assign tickers
  
  pct <- t(round(pct[nrow(pct),] * 100)) # Transform last period into percents
  
  # Add colour range
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975")
  
  colnames(pct) <- "Portion" # Assign column name
  
  l <- NULL # Create list
  
  for (n in 1:length(tickers)){ s <- tickers[n] # For each security find sector
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", s, s)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
    
    y <- tab11 %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    l <- rbind(l, y[4]) } # Add to list
    
  colnames(l) <- "Industry" # Assign column name
  rownames(l) <- tickers # Assign tickers
  
  l # Display
  
  pie.df <- data.frame(l, pct) # Form data frame
  pie.df <- aggregate(Portion ~ Industry, data=pie.df, sum) # Conditional sum
  
  pie(pie.df[,2],labels=c(sprintf("%s %s%%",pie.df[,1],pie.df[,2])), col=C,
      main=main,sub=sub, radius=1.5) # Pie Chart  
}
p.pie.plt.industry(df_portfolio, main = "Portfolio by Industries") # Test
