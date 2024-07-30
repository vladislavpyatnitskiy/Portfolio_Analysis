library("rvest") # Library 

p.finviz.ratios <- function(x){ # Get info about tickers from FINVIZ
  
  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  D <- NULL
  
  for (n in 1:length(x)){ j <- x[n] # Assign security and read HTML of page
    
    y <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", j)) %>%
      html_nodes('table') %>% .[[10]] %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text() # Get info from FINVIZ website
    
    d <- NULL # Create lists to contain values
    
    for (n in 0:(length(y)/2)){ d <- rbind(d, cbind(y[(1+n*2)], y[(2+n*2)])) } 
    
    d <- d[-nrow(d),] # Reduce last column
      
    d[27,1] <- "EPS next Y (%)" # Change name
      
    rownames(d) <- d[,1] # Assign row names
      
    d <- subset(d, select = -c(1)) # Reduce excess column
      
    colnames(d) <- j # Assign column name
      
    if (is.null(D)){ D <- d } else { D <- cbind(D, d) } } # Join
      
  D # Display
}
p.finviz.ratios(df_portfolio) # Test
