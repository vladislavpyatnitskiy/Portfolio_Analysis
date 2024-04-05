lapply(c("rvest", "plotly", "timeSeries"), require, character.only = T) # Libs

p.pie.plt.dividend.industry <- function(x){ # Plot of Portfolio Dividends
  
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
  
  l <- NULL # Create list
  
  for (n in 1:length(tickers)){ s <- tickers[n] # For each security find sector
  
    p <- read_html(sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s",
                           s, s)) # Read HTML & extract necessary info
    
    price.yahoo1 <- p %>% html_nodes('div') %>% .[[1]] -> tab
    
    y <- tab %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    l <- rbind(l, y[4]) } # Add to list
    
  rownames(l) <- tickers # Assign tickers
  
  df <- data.frame(l, v) # Join Industries and portions info
  
  colnames(df) <- c("Industry", "Portion") # Assign column name
  
  df <- aggregate(Portion ~ Industry, data=df, sum) # Conditional sum
  
  plot_ly(df, labels = ~df[,1], values = ~df[,2] , type = 'pie',
          textposition = 'outside',textinfo = 'percent') %>%
    layout(title="Dividends of Portfolio Securities by Industries",
           margin = list(l = 20, r = 20, t = 120),
           xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
}
p.pie.plt.dividend.industry(df_portfolio_dividend) # Test
