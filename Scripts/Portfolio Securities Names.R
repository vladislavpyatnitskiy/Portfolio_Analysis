library("rvest") # Library

p.names <- function(x){ # Data Frame with tickers and names
  
  A <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  l <- NULL
  
  for (n in 1:length(A)){ a <- A[n]
  
    s <- sprintf("https://finance.yahoo.com/quote/%s?p=%s&.tsrc=fin-srch",a,a)
    
    s <- read_html(s) # Read html info
    
    s.yahoo <- s %>% html_nodes('body') %>% .[[1]] -> tab # Assign Body
    
    y <- tab %>% html_nodes('div') %>% html_nodes('h1') %>% html_text()
    
    y <- read.fwf(textConnection(y), widths = c(nchar(y) - nchar(a) - 3, 1),
                  colClasses = "character") # Reduce excessive elements
    
    l <- rbind(l, y[,-ncol(y)]) } # Join names
    
  p.list <- data.frame(A, l) # Join tickers with names
  
  rownames(p.list) <- seq(nrow(p.list)) # row names
  colnames(p.list) <- c("Ticker", "Name") # Column names
  
  p.list # Display
}
p.names(df_portfolio) # Test
