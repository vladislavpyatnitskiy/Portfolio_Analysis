# Function to download fundamentals data for portfolio

p.fundamentals <- function(x, transpose = F){ # portfolio fundamentals
  
  x <- colnames(x[,1 + 3 * seq(31, from = 0)]) # portfolio tickers
  
  df.s <- NULL # Set up list for infos
  
  for (n in 1:length(x)){ j <- x[n] # For each security get info
  
    s <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",j,j)
    
    s.page <- read_html(s) # Read HTML of page
    
    s.yahoo <- s.page %>% html_nodes('table') %>% .[[1]] -> tab1 # Assign Table 
    
    s.header <- tab1 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    df.f1 <- NULL # Create lists for ratio names and values
    df.f2 <- NULL
    
    for (n in 0:(length(s.header) / 2)){ 
      
      df.f1 <- rbind(df.f1, s.header[(1 + n * 2)]) # Ratio names
      
      df.f2 <- rbind(df.f2, s.header[(2 + n * 2)]) } # Ratio values
    
    df.f3 <- data.frame(df.f1, df.f2) # Join 
    
    df.f3 <- df.f3[-nrow(df.f3),] # Display
    
    rownames(df.f3) <- df.f3[,1] # Assign row names
    
    df.f3 <- subset(df.f3, select = -c(1)) # Reduce excess column
    
    colnames(df.f3) <- j # Assign column name
    
    if (is.null(df.s)){ df.s <- df.f3 } else { df.s <- cbind(df.s, df.f3) } }
  
  if (isTRUE(transpose)){ t(df.s) } else { df.s } # Display
}
p.fundamentals(df_portfolio, transpose = T) # Test
