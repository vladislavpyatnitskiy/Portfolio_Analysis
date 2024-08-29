library(rvest) # Library

p.ratios.sa <- function(x){ # Ratios data from stockanalysis website
 
  x <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3,from = 0)])[-(ncol(x) %/% 3 + 1)]
  
  L <- NULL
  
  for (n in 1:length(x)){ # Get data
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/statistics/",
                           tolower(x[n]))) %>% html_nodes('table') %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    R <- p[seq(from = 1, to = length(p), by = 2)] # row names
    C <- as.data.frame(p[seq(from = 2, to = length(p), by = 2)]) # stock data
    
    rownames(C) <- R # tickers as row names  
    
    if (is.null(L)){ L <- C } else { L <- cbind.data.frame(L, C) } }
  
  colnames(L) <- x # Column name
  
  L # Display 
}
p.ratios.sa(df_portfolio) # Test
