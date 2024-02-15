library("rvest") # Library

p.marketcap <- function(x, info = F){ # Market Cap Info
  
  x <- colnames(x[,1 + 3 * seq(31, from = 0)]) # tickers
  
  df <- NULL # Data Frame for Market Cap Levels and Values
  
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
    
    if (isFALSE(info)){ # Market Cap Levels
    
      if (s < .3){ l <- "Micro-Cap" } # if < $300 million => Micro-Cap
      
      else if (s > .3 && s < 2) { l <- "Small-Cap" } # Small-Cap
      
      else if (s > 2 && s < 10) { l <- "Mid-Cap" } # Mid-Cap
      
      else if (s > 10 && s < 200) { l <- "Large-Cap" } # Large-Cap
      
      else { l <- "Mega-Cap" } # if > $200 billion => Mega-Cap
      
      df <- rbind.data.frame(df, cbind(l, s)) } # Market Cap Level with values
    
    else { df <- rbind(df, s) } } # Market Cap values only
    
    if (isFALSE(info)){ # Create Data Frame
    
      rownames(df) <- x # Tickers
      colnames(df) <- c("Level", "Marker Cap ($billions)") # colnames
      
      df } else { c <- as.numeric(df) # Make available values to numeric format
    
    names(c) <- x # Assign names to them
    
    l <- NULL # Find securities without debt/ebitda values
    
    for (n in 1:length(c)){ if (is.na(c[n])){ l <- c(l, names(c)[n])} }
    
    c <- sort(c, decreasing = T) # Sort in a descending way
    
    m <- NULL # Write advices about securities according to Market Cap
    
    if (isFALSE(identical(names(which(c > 200)), character(0)))){
      
      m <- c(m, paste("Mega-Cap Companies:",
                      toString(names(which(c > 200))))) }
    
    if (isFALSE(identical(names(which(c > 10 & c < 200)), character(0)))){
      
      m <- c(m, paste("Large-Cap Companies:",
                      toString(names(which(c > 10 & c < 200))))) }
    
    if (isFALSE(identical(names(which(c > 2 & c < 10)), character(0)))){
      
      m <- c(m, paste("Medium-Cap Companies:",
                      toString(names(which(c > 2 & c < 10))))) }
    
    if (isFALSE(identical(names(which(c > .3 & c < 2)), character(0)))){
      
      m <- c(m, paste("Small-Cap Companies:",
                      toString(names(which(c > .3 & c < 2))))) }
    
    if (isFALSE(identical(names(which(c < .3)), character(0)))){
      
      m <- c(m, paste("Micro-Cap Companies:", toString(names(which(c < .3)))))}
    
    m } # Display
}
p.marketcap(df_portfolio, info = T) # Test
