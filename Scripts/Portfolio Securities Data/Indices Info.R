library("rvest") # Library

p.index <- function(x, agg = F){ # Indices of portfolio securities
  
  A <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)])[-(ncol(x) %/% 3 + 1)]
  
  A <- A[-grep("VSTO", A)] # Reduce obsolete tickers
  A <- A[-grep("ARCH", A)] 
  
  l <- NULL # Create list
  
  for (n in 1:length(A)){ # Read HTML & extract necessary info
    
    p <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d",
                           A[n])) %>% html_nodes('table') %>% .[[10]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    l <- rbind(l, p[grep("Index", p) + 1]) } # Add to list
  
  colnames(l) <- "Index" # Assign column name
  rownames(l) <- A # Assign tickers
  
  l[,1] <- gsub("-", "Not Included in Any Index", l[,1]) # Substitution
  
  if (agg){ df <- cbind.data.frame(rownames(l), l) # Merge tickers with indices
    
    rownames(df) <- seq(nrow(df)) # Assign default row names
    colnames(df) <- c("Ticker", "Index") # Assign column names
    
    # Group by Index and combine tickers
    aggregated <- aggregate(Ticker ~ Index, data = df,
      FUN = function(tickers) { paste(sort(tickers), collapse = ", ") })
    
    l <- aggregated[order(aggregated$Index), ] } # Sort by Index
  
  l # Display
}
p.index(df_portfolio, T) # Test
