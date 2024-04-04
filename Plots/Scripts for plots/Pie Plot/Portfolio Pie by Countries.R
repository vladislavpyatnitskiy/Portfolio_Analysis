library("rvest") # Library

p.pie.plt.country <- function(x){ # Portfolio Pie Plot by Countries
  
  tickers <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)])[-(ncol(x)%/%3+1)]
  
  df <- NULL
  
  for (n in 1:length(tickers)){ v <- tickers[n] # Subset ticker
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", v, v)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    c <- tab %>% html_nodes('p') # Find country character in elements
    
    l <- strsplit(toString(c), "<br>")[[1]][length(strsplit(toString(c),
                                                            "<br>")[[1]])-4]
    df <- rbind.data.frame(df, l) } # Data Frame 
    
  # Calculate Weights for each security
  y <- round(as.data.frame(t(as.data.frame(x[,3*seq(ncol(x)%/%3,
                                                    from=1)][nrow(x),]/
                                             as.numeric(x[nrow(x),
                                                          ncol(x)])))),2)*100
  rownames(y) <- tickers
  rownames(df) <- tickers # Tickers
  
  df <- cbind.data.frame(df, y) # Join Countries Info with Portions
  
  colnames(df) <- c("Country", "Portion") # column names
  
  df <- aggregate(Portion ~ Country, data=df, sum) # Conditional sum
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975")
  
  pie(df[,2], labels=c(sprintf("%s %s%%", df[,1], df[,2])), col=C, radius=1.7,
      main = "Portfolio Securities by Countries") # Plot
}
p.pie.plt.country(df_portfolio) # Test
