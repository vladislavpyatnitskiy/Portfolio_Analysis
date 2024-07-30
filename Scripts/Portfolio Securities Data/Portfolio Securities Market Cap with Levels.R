library("rvest") # Library

p.marketcap <- function(x, info = F){ # Market Cap info via string or table
  
  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)] # Tickers
  
  j <- list(list(10, 200, "Large-Cap Companies:", "Large-Cap"), # > 10 & < 200
            list(2, 10, "Medium-Cap Companies:", "Medium-Cap"), # > 2 & < 10
            list(0.3, 2, "Small-Cap Companies:", "Small-Cap"), # > 0.3 & < 2
            list(0, 0.3, "Micro-Cap Companies:", "Micro-Cap")) # > 0 & < 0.3
  
  df <- NULL # Data Frame for Market Cap Levels and Values
  
  for (n in 1:length(x)){ # Read HTML & extract necessary info
    
    p <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/%s", x[n],
                           "key-statistics")) 
    
    i <- p %>% html_nodes('div') %>% .[[1]] %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text()
    
    s <- i[grep("Market cap", i) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 1, 1),
                  colClasses = "character")
    
    if (s[1,2] == "M"){ s <- as.numeric(s[1,1])/1000 } else if (s[1,2] == "T"){
      
      s <- as.numeric(s[1,1]) * 1000 } else { s <- as.numeric(s[1,1]) }
    
    if (isFALSE(info)){ for (n in 1:length(j)){ # Market Cap Levels
        
        if (s > j[[n]][[1]] && s < j[[n]][[2]]){ l <- j[[n]][[4]] 
        
        } else if (s > 200){ l <- "Mega-Cap" } else { next } } 
      
      # Market Cap Level with values OR Market Cap values only
      df <- rbind.data.frame(df, cbind(l, s)) } else { df <- rbind(df, s) } }
  
  if (isFALSE(info)){ # Create Data Frame
    
    rownames(df) <- x # Tickers
    colnames(df) <- c("Level", "Marker Cap ($billions)") # column names
    
    df } else { c <- as.numeric(df) # Make available values to numeric format
    
    names(c) <- x # Assign names to them
    
    c <- sort(c, decreasing = T) # Sort in a descending way
    
    m <- NULL # Write advices about securities according to Market Cap
    
    if (isFALSE(identical(names(which(c > 200)), character(0)))){
      
      m <- c(m, paste("Mega-Cap Companies:", toString(names(which(c > 200)))))}
    
    for (n in 1:length(j)){ #
      
      if (isFALSE(identical(names(which(c > j[[n]][[1]] & c < j[[n]][[2]])),
                            character(0)))){
        m <- c(m,
               paste(j[[n]][[3]],
                     toString(names(which(c>j[[n]][[1]] & c<j[[n]][[2]])))))} }
    m } # Display
}
p.marketcap(df_portfolio, info = T) # Test
