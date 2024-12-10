library(rvest) # Library

p.rating <- function(x){ # Company Ratings for portfolio
  
  x <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)])[-(ncol(x) %/% 3 + 1)] 
  
  D <- NULL
  
  for (n in 1:length(x)){ l <- c("Price Target", "Upside")
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/ratings/",
                           tolower(x[n]))) %>% html_nodes('main') %>%
      html_nodes('div') %>% html_nodes('div') %>% html_text()
    
    d <- NULL
    
    for (m in 1:2){ f <- unlist(strsplit(p[grepl(l[m], p)][1], " "))
      
      d <- c(d, f[length(f)]) }
    
    D <- rbind.data.frame(D, cbind(p[9], d[1], d[2])) } # Merge values
  
  colnames(D) <- c("Current Price", "Predicted Price", "Upside") # Column names
  rownames(D) <- x # Tickers
  
  D # Display
}
p.rating(df_portfolio) # Test
