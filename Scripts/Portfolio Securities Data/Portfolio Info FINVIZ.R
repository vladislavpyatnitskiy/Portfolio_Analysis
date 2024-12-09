library("rvest") # Library 

p.finviz.ratios <- function(x){ # Get info about tickers from FINVIZ
  
  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  x[grep("VSTO", x)] <- "GEAR" # Substitute expired ticker with new one
  
  D <- NULL
  
  for (n in 1:length(x)){ j <- x[n] # Assign security and read HTML of page
  
    y <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", j)) %>%
      html_nodes('table') %>% .[[10]] %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text() # Get info from FINVIZ website
    
    d <- data.frame(y[seq(from=1,to=length(y),by=2)],
                    y[seq(from=2,to=length(y),by=2)]) # Put into data frame
    
    d[27,1] <- "EPS next Y (%)" # Change name
    
    rownames(d) <- d[,1] # Assign row names
    
    d <- subset(d, select = -c(1)) # Reduce excess column
    
    colnames(d) <- j # Assign column name
    
    if (is.null(D)){ D <- d } else { D <- cbind(D, d) } } # Join
    
  D # Display
}
p.finviz.ratios(df_portfolio) # Test
