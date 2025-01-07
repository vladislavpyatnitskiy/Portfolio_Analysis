library("rvest") # Library

p.country <- function(x){ l <- NULL # Create list

  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  x[grep("VSTO", x)] <- "GEAR" # Substitute expired ticker with new one
  
  for (n in 1:length(x)){ s <- x[n] # Read HTML & extract necessary info
    
    C<-read_html(sprintf("https://stockanalysis.com/stocks/%s/company/",s)) %>%
      html_nodes('table') %>% .[[1]] %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text() 
  
    l <- rbind.data.frame(l, C[grep("Country", C) + 1]) } # Data Frame 
  
  colnames(l) <- "Country"
  rownames(l) <- x
  
  l # Display
}
p.country(df_portfolio) # Test
