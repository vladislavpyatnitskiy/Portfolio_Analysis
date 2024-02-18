lapply(c("rvest", "plotly", "timeSeries"), require, character.only = T) # Libs

p.pie.plt.dividend.country <- function(x){ # Plot of Portfolio Dividends
  
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
  
  l <- NULL
  
  for (n in 1:length(tickers)){ u <- tickers[n] # Subset ticker
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", u, u)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    c <- tab %>% html_nodes('p') # Find country character in elements
    
    C <- strsplit(toString(c), "<br>")[[1]][length(strsplit(toString(c),
                                                            "<br>")[[1]])-4]
    l <- rbind.data.frame(l, C) } # Data Frame 
  
  rownames(l) <- tickers # Assign tickers
  
  df <- data.frame(l, v) # Join Country and Portions info
  
  colnames(df) <- c("Country", "Portion") # Assign column name
  
  df <- aggregate(Portion ~ Country, data = df, sum) # Conditional sum
  
  plot_ly(df, labels = ~df[,1], values = ~df[,2] , type = 'pie',
          textposition = 'outside',textinfo = 'percent') %>%
    layout(title = "Portfolio Dividends by Country",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
}
p.pie.plt.dividend.country(df_portfolio_dividend) # Test
