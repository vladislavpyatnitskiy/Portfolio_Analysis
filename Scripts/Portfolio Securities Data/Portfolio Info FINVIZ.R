library("rvest") # Library 

p.finviz.ratios <- function(x){ # Get info about tickers from FINVIZ
  
  x <- colnames(x[,1 + 3 * seq(31, from = 0)]) # portfolio tickers
  
  df_ <- NULL
  
  for (n in 1:length(x)){ j <- x[n] # Assign security and read HTML of page
  
    s <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", j))
    
    s.yahoo <- s %>% html_nodes('table') %>% .[[10]] -> tab # Assign Table 
    
    y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    df <- NULL # Create lists to contain values
    
    for (n in 0:(length(y) / 2)){ # Ratio values
      
      df <- rbind(df, cbind(y[(1 + n * 2)], y[(2 + n * 2)])) } 
    
    df <- df[-nrow(df),] # Reduce last column
    
    df[27,1] <- "EPS next Y (%)" # Change name
    
    rownames(df) <- df[,1] # Assign row names
    
    df <- subset(df, select = -c(1)) # Reduce excess column
    
    colnames(df) <- j # Assign column name
    
    if (is.null(df_)){ df_ <- df } else { df_ <- cbind(df_, df) } }
  
  df_ # Display
}
p.finviz.ratios(df_portfolio) # Test
