library("rvest") # Library

p.country <- function(x){ l <- NULL # Create list

  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  for (n in 1:length(x)){ s <- x[n] # Read HTML & extract necessary info
  
    p <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/profile", s))
    
    tab <- p %>% html_nodes('div') %>% .[[1]]
    
    y <- tab %>% html_nodes('p') # Find country character in elements
    
    l <- rbind(l, strsplit(toString(y),
                           "<br>")[[1]][length(strsplit(toString(y),
                                                        "<br>")[[1]])-4]) }
  colnames(l) <- "Country"
  rownames(l) <- x
  
  l # Display
}
p.country(df_portfolio) # Test
