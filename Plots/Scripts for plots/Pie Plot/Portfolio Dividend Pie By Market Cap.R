lapply(c("rvest", "plotly", "timeSeries"), require, character.only = T) # Libs

p.pie.plt.dividend.marketcap <- function(x){ # Plot of Portfolio Dividends
  
  tickers <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x) %/% 3+1)]
  
  x <- cumsum(x[,3 * seq(ncol(x) %/% 3, from = 1)]) # Calculate Cumulative Divs
  
  colnames(x) <- tickers # Assign tickers
  
  x <- cbind(x, as.timeSeries(rowSums(x, na.rm = T))) # Join with Total Sum
  
  colnames(x)[ncol(x)] <- "Total" # Give column name to total sum
  
  x <- x[,colSums(x) !=0] # Reduce column without dividends
  
  tickers <- colnames(x) # Assign tickers of securities without dividends
  
  x <- as.numeric(x[nrow(x),]) / as.numeric(x[nrow(x),ncol(x)]) # Find %
  
  x <- x[-length(x)] # Reduce column with total sum (100%)
  
  tickers <- tickers[-length(tickers)] # Reduce name with total sum (100%)
  
  v <- data.frame(c(round(x * 100, 2))) # Data Frame with tickers & %
  
  rownames(v) <- tickers # Assign tickers as row names
  
  l <- NULL
  
  for (n in 1:length(tickers)){ u <- tickers[n] # Subset ticker
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",u,u)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s <- i[grep("Market Cap", i) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 1, 1),
                  colClasses = "character")
    
    if (s[1,2] == "M"){ s <- as.numeric(s[1,1])/1000 } else if (s[1,2] == "T"){
      
      s <- as.numeric(s[1,1]) * 1000 } else { s <- as.numeric(s[1,1]) }
    
    if (s < .3){ m <- "Micro-Cap" } # if < $300 million => Micro-Cap
    
    else if (s > .3 && s < 2) { m <- "Small-Cap" } # Small-Cap
    
    else if (s > 2 && s < 10) { m <- "Mid-Cap" } # Mid-Cap
    
    else if (s > 10 && s < 200) { m <- "Large-Cap" } # Large-Cap
    
    else { m <- "Mega-Cap" } # if > $200 billion => Mega-Cap
    
    l <- rbind.data.frame(l, m) } # Data Frame with Market Cap Levels
    
  rownames(l) <- tickers # Assign tickers
  
  df <- data.frame(l, v) # Join Market Cap and Portions info
  
  colnames(df) <- c("Level", "Portion") # Assign column name
  
  df <- aggregate(Portion ~ Level, data=df, sum) # Conditional sum
  
  plot_ly(df, labels = ~df[,1], values = ~df[,2] , type = 'pie',
          textposition = 'outside',textinfo = 'percent') %>%
    layout(title="Dividends of Portfolio Securities by Market Capitalisation",
           margin = list(l = 20, r = 20, t = 120),
           xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
}
p.pie.plt.dividend.marketcap(df_portfolio_dividend) # Test
