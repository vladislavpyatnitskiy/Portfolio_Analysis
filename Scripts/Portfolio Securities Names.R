library("rvest") # Library

p.names <- function(x){ # Company names from Stock Analysis Website
  
  A <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  L <- NULL # Variable for tickers
  
  for (n in 1:length(A)){ a <- A[n] # Tickers
  
    s <- read_html(sprintf("https://stockanalysis.com/stocks/%s/", a)) %>%
      html_nodes('main') %>% .[[1]] %>% html_nodes('div') %>%
      html_nodes('h1') %>% html_text() # Get values
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - nchar(a) - 3, 1),
                  colClasses = "character")[1] # Reduce excessive elements
    
    L <- rbind.data.frame(L, s) } # Join names
    
  colnames(L) <- "Company Name" # Assign column name
  rownames(L) <- A
  
  L # Display
}
p.names(df_portfolio) # Test
