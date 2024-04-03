library("rvest") # Library

p.hist.plt.marketcap <- function(x){ # Histogram of Market Caps
  
  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  df <- NULL # Data Frame for Market Cap values
  
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
    
    df <- rbind(df, s) }
  
  s.min <- min(df) # Minimum Market Cap value
  s.max <- max(df) # Maximum Market Cap value
  
  # Parameters
  hist(df, main = "Portfolio Market Cap Histogram", freq = F, breaks = 100,
       ylab = "Probability", xlab = "Market Cap Values", las = 1, col = "navy",
       xlim = c(s.min, s.max), border = "white")
  
  abline(h = 0) # Add vertical line at x = 0
  
  box() # Define plot borders
}
p.hist.plt.marketcap(df_portfolio)
