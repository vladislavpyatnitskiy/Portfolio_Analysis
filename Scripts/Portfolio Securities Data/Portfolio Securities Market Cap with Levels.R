library("rvest") # Library

p.marketcap <- function(x){ # Market Cap Info
  
  x <- colnames(x[,1 + 3 * seq(31, from = 0)]) # tickers
  
  df <- NULL
  
  for (n in 1:length(x)){ v <- x[n] # Subset ticker
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",v,v)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s <- i[grep("Market Cap", i) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 1, 1),
                  colClasses = "character")
    
    if (s[1,2] == "M"){ s <- as.numeric(s[1,1])/1000 } else if (s[1,2] == "T"){
      
      s <- as.numeric(s[1,1]) * 1000 } else { s <- as.numeric(s[1,1]) }
    
    if (s < .3){ l <- "Micro-Cap" } # if < $300 million => Micro-Cap
    
    else if (s > .3 && s < 2) { l <- "Small-Cap" } # Small-Cap
    
    else if (s > 2 && s < 10) { l <- "Mid-Cap" } # Mid-Cap
    
    else if (s > 10 && s < 200) { l <- "Large-Cap" } # Large-Cap
    
    else { l <- "Mega-Cap" } # if > $200 billion => Mega-Cap
    
    df <- rbind.data.frame(df, cbind(l, s)) } # Data Frame 
    
  rownames(df) <- x # Tickers
  colnames(df) <- c("Level", "Marker Cap ($billions)") # colnames
  
  df # Display
}
p.marketcap(df_portfolio) # Test
