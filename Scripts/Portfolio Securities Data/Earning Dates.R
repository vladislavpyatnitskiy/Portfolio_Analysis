library(rvest) # Library

p.earnings <- function(x, sort=T, desc=F){ # Earning Dates for Portfolio Stocks
  
  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  l <- NULL
  
  for (n in 1:length(x)){ y <- x[n]
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                           tolower(y))) %>% html_nodes('table') %>% .[[2]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    l <- c(l, format(as.Date(p[grep("Earnings Date", p) + 1],
                             format = "%B %d, %Y"), "%Y/%m/%d")) }
  names(l) <- x # Tickers
  
  if (isTRUE(sort)){ # If needed sorted, choose between asc and desc
    
    if (isTRUE(desc)){ l <- sort(l, decreasing = T) } else { l <- sort(l) } }
  
  l <- as.data.frame(l)
  
  colnames(l) <- "Earnings Date" # Earning Date column name
  
  l # Display
}
p.earnings(df_portfolio) # Test
