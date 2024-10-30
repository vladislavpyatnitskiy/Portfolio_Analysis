library(rvest) # Library

p.rating <- function(x){ # Company Ratings for portfolio
  
  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  D <- NULL
  
  for (n in 1:length(x)){ 
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/ratings/",
                           tolower(x[n])))
    
    p <- p %>% html_nodes('main') %>% html_nodes('div') %>%
      html_nodes('div') %>% html_text()
    
    f <- unlist(strsplit(p[grepl("Price Target", p)][1], " "))
    f <- f[length(f)]
    
    u <- unlist(strsplit(p[grepl("Upside", p)][1], " "))
    u <- u[length(u)]
    
    D <- rbind.data.frame(D, cbind(p[9], f, u)) } # Merge values
  
  colnames(D) <- c("Current Price", "Predicted Price", "Upside") # Column names
  rownames(D) <- x # Tickers
  
  D # Display
}
p.rating(df_portfolio) # Test
