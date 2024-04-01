library("rvest") # Library

p.fundamentals <- function(x, transpose = F, l = 1){ # portfolio fundamentals
  
  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  df.s <- NULL # Set up list for infos
  
  for (n in 1:length(x)){ j <- x[n] # For each security get info
  
    s <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",j,j)
    
    s.page <- read_html(s) # Read HTML of page
    
    s.yahoo <- s.page %>% html_nodes('table') %>% .[[l]] -> tab # Assign Table 
    
    y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    df <- NULL # Create list for ratio names and values
    
    for (n in 0:(length(y)/2)){ df <- rbind(df, cbind(y[(1+n*2)], y[(2+n*2)]))} 
    
    df <- df[-nrow(df),] # Delete excessive row
    
    rownames(df) <- df[,1] # Assign row names
    
    df <- subset(df, select = -c(1)) # Reduce excess column
    
    colnames(df) <- j # Assign column name
    
    if (is.null(df.s)){ df.s <- df } else { df.s <- cbind(df.s, df) } }
    
  if (isTRUE(transpose)){ t(df.s) } else { df.s } # Display
}
p.fundamentals(df_portfolio, transpose = T, l = 2) # Test
