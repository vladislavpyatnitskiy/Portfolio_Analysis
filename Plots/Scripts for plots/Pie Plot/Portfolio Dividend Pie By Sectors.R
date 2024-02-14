lapply(c("rvest", "plotly"), require, character.only = T) # Libs

p.pie.plt.dividend.sector <- function(x){ # Plot of Portfolio Dividends
  
  s.names <- colnames(x[,1 + 3 * seq(31, from = 0)]) # Subset tickers
  
  x <- cumsum(x[,3 + 3 * seq(31, from = 0)]) # Calculate Cumulative Divs
  
  colnames(x) <- s.names # Assign tickers
  
  x <- cbind(x, as.timeSeries(rowSums(x, na.rm = T))) # Join with Total Sum
  
  colnames(x)[ncol(x)] <- "Total" # Give column name to total sum
  
  x <- x[,colSums(x) !=0] # Reduce column without dividends
  
  s.names <- colnames(x) # Assign tickers of securities without dividends
  
  x <- as.numeric(x[nrow(x),]) / as.numeric(x[nrow(x),ncol(x)]) # Find %
  
  x <- x[-length(x)] # Reduce column with total sum (100%)
  
  tickers <- s.names[-length(s.names)] # Reduce name with total sum (100%)
  
  v <- data.frame(c(round(x * 100, 2))) # Data Frame with tickers & %
  
  rownames(v) <- tickers # Assign tickers as row names
  
  l <- NULL # Create list
  
  for (n in 1:length(tickers)){ s <- tickers[n] # For each security find sector
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", s, s)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    y <- tab %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    l <- rbind(l, y[2])} # Add to list
  
  rownames(l) <- tickers # Assign tickers
  
  df <- data.frame(l, v) # Join Sectors and portions info
  
  colnames(df) <- c("Sector", "Portion") # Assign column name
  
  df <- aggregate(Portion ~ Sector, data=df, sum) # Conditional sum
  
  plot_ly(df, labels = ~df[,1], values = ~df[,2] , type = 'pie',
          textposition = 'outside',textinfo = 'percent') %>%
    layout(title = "Portfolio Dividends by Sectors",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
}
p.pie.plt.dividend.sector(df_portfolio_dividend) # Test
