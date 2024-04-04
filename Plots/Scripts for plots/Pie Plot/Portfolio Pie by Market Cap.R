library("rvest") # Library

p.pie.plt.marketcap <- function(x){ # Portfolio Pie Plot by Market Cap
  
  tickers <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)])[-(ncol(x)%/%3+1)]
  
  df <- NULL
  
  for (n in 1:length(tickers)){ v <- tickers[n] # Subset ticker
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",v,v)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s <- i[grep("Market Cap", i) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 1, 1),
                  colClasses = "character")
    
    if (s[1,2] == "M"){ s <- as.numeric(s[1,1])/1000 } else if (s[1,2] == "T"){
      
      s <- as.numeric(s[1,1]) * 1000 } else { s <- as.numeric(s[1,1]) }
    
    if (s < .3){ l <- "Micro-Cap" } # if < $300 million => Micro-Cap
    
    else if (s > .3 && s < 2) { l <- "Small-Cap" } # Small-Cap
    
    else if (s > 2 && s < 10) { l <- "Mid-Cap" } # Mid-Cap
    
    else if (s > 10 && s < 200) { l <- "Large-Cap" } # Large-Cap
    
    else { l <- "Mega-Cap" } # if > $200 billion => Mega-Cap
    
    df <- rbind.data.frame(df, l) } # Data Frame 
    
  # Calculate Weights for each security
  y <- round(as.data.frame(t(as.data.frame(x[,3*seq(ncol(x)%/%3,
                                                    from=1)][nrow(x),]/
                                             as.numeric(x[nrow(x),
                                                          ncol(x)])))),2)*100
  rownames(y) <- tickers
  rownames(df) <- tickers # Tickers
  
  df <- cbind.data.frame(df, y) # Join
  
  colnames(df) <- c("Level", "Portion") # column names
  
  df <- aggregate(Portion ~ Level, data=df, sum) # Conditional sum
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975")
  
  pie(df[,2], labels=c(sprintf("%s %s%%", df[,1], df[,2])), col=C, radius=1.6,
      main = "Portfolio Securities by Market Capitalisation") # Plot
}
p.pie.plt.marketcap(df_portfolio) # Test
