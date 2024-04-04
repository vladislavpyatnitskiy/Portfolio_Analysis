lapply(c("plotly", "timeSeries"), require, character.only = T) # Libraries

p.pie.plt.dividend <- function(x){ # Plot of Portfolio Dividends
  
  tickers <- colnames(x[,1+3*seq(ncol(x) %/% 3, from=0)])[-(ncol(x)%/%3+1)]
  
  x <- cumsum(x[,3 * seq(ncol(x) %/% 3, from = 1)]) # Calculate Cumulative Divs
  
  colnames(x) <- tickers # Assign tickers
  
  x <- cbind(x, as.timeSeries(rowSums(x, na.rm = T))) # Join with Total Sum
  
  colnames(x)[ncol(x)] <- "Total" # Give column name to total sum
  
  x <- x[,colSums(x) !=0] # Reduce column without dividends
  
  tickers <- colnames(x) # Assign tickers of securities without dividends
  
  x <- as.numeric(x[nrow(x),]) / as.numeric(x[nrow(x),ncol(x)]) # Find %
  
  x <- x[-length(x)] # Reduce column with total sum (100%)
  
  tickers <- tickers[-length(tickers)] # Reduce name with total sum (100%)
  
  v <- data.frame(tickers, c(round(x * 100, 2))) # Data Frame with tickers & %
  
  plot_ly(v, labels = ~v[,1], values = ~v[,2] , type = 'pie',
          textposition = 'outside',textinfo = 'percent') %>%
    layout(title = "Dividends of Portfolio Securities",
           margin = list(l=20, r=20, t=120),
           xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
}
p.pie.plt.dividend(df_portfolio_dividend) # Test
