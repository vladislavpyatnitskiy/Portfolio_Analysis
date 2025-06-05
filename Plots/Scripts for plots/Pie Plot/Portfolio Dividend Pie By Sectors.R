lapply(c("rvest", "plotly", "timeSeries", "httr", "xml2"), require,
       character.only = T) # Libs

p.pie.plt.dividend.sector <- function(x){ # Plot of Portfolio Dividends
  
  A <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)])[-(ncol(x) %/% 3 + 1)]
  
  x <- cumsum(x[,3 * seq(ncol(x) %/% 3, from = 1)]) # Calculate Cumulative Divs
  
  colnames(x) <- A # Assign tickers
  
  x <- cbind(x, as.timeSeries(rowSums(x, na.rm = T))) # Join with Total Sum
  
  colnames(x)[ncol(x)] <- "Total" # Give column name to total sum
  
  x <- x[,colSums(x) != 0] # Reduce column without dividends
  
  A <- colnames(x) # Assign tickers of securities without dividends
  
  x <- as.numeric(x[nrow(x),]) / as.numeric(x[nrow(x), ncol(x)]) # Find %
  
  x <- x[-length(x)] # Reduce column with total sum (100%)
  A <- A[-length(A)] # Reduce name with total sum (100%)
  
  v <- data.frame(c(round(x * 100, 2))) # Data Frame with tickers & %
  
  rownames(v) <- A # Assign tickers as row names
  
  l <- NULL # Create list
  
  for (n in 1:length(A)){ k <- A[n] # For each security find sector
    
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/profile", k),
                    add_headers(`User-Agent` = B))
    
    f <- read_html(content(response, as = "text", encoding = "UTF-8")) %>%
      html_nodes('div') %>% html_nodes('dl') %>%
      html_nodes('dd') %>% html_nodes('strong') %>% html_text() %>% .[1]
    
    l <- rbind(l, f) } # Add to list
    
  rownames(l) <- A # Assign tickers
  
  df <- data.frame(l, v) # Join Sectors and portions info
  
  colnames(df) <- c("Sector", "Portion") # Assign column name
  
  df <- aggregate(Portion ~ Sector, data = df, sum) # Conditional sum
  
  plot_ly(
    df,
    labels = ~df[,"Sector"],
    values = ~df[,"Portion"] ,
    type = 'pie',
    textposition = 'outside',
    textinfo = 'percent'
    ) %>%
    layout(
      title = "Dividends of Portfolio Securities by Sectors",
      margin = list(l = 20, r = 20, t = 120),
      xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
      yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
      )
}
p.pie.plt.dividend.sector(df_portfolio_dividend) # Test
