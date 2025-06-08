library("rvest") # Library

p.pie.plt.country <- function(x){ # Portfolio Pie Plot by Countries
  
  tickers <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from=0)])[-(ncol(x)%/%3+1)]
  
  df <- NULL
  
  for (n in 1:length(tickers)){ i <- tickers[n] # Subset ticker
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/company/",
                           tolower(i))) %>% html_nodes('table') %>%
      .[[1]] %>% html_nodes('tr') %>% html_nodes('td') %>% html_text() 
    
    df <- rbind.data.frame(df, p[grep("Country", p) + 1]) } # Data Frame 
    
  y <- as.data.frame(x[,3 * seq(ncol(x) %/% 3, from = 1)]) / x[,ncol(x)]
  
  y <- t(round(y[nrow(y),] * 100))
  
  rownames(y) <- tickers
  rownames(df) <- tickers # Tickers
  
  df <- cbind.data.frame(df, y) # Join Countries Info with Portions
  
  colnames(df) <- c("Country", "Portion") # column names
  
  df <- aggregate(Portion ~ Country, data = df, sum) # Conditional sum
  
  C = c(
    "#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
    "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
    "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
    "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
    "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
    "#895c8b","#bd5975"
    )
  
  pie(
    df[,"Portion"],
    labels = c(sprintf("%s %s%%", df[,"Country"], df[,"Portion"])),
    col = C,
    radius = 1.7,
    main = "Portfolio Securities by Countries"
    ) # Plot
}
p.pie.plt.country(df_portfolio) # Test
