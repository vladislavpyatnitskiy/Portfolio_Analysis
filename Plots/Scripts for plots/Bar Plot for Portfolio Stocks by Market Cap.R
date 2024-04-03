library("rvest") # Library

p.bar.plt.marketcap <- function(x){ # Bar Plot with Market Capitalisations
  
  tickers <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  df <- NULL # Collect market cap data
  
  for (n in 1:length(tickers)){ v <- tickers[n] # Subset ticker
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",v,v)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s <- i[grep("Market Cap", i) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 1, 1),
                  colClasses = "character")
    
    if (s[1,2] == "M"){ s <- as.numeric(s[1,1])/1000 } else if (s[1,2] == "T"){
      
      s <- as.numeric(s[1,1]) * 1000 } else { s <- as.numeric(s[1,1]) }
    
    df <- rbind(df, s) } # Data Frame 
  
  c <- as.numeric(df) # Make available values to numeric format
  
  names(c) <- tickers # Assign names to them
  
  l <- NULL # Find securities without available market cap values
  
  for (n in 1:nrow(df)){ if (is.na(df[n,])){ l <- c(l, rownames(df)[n])} }
  
  c <- sort(c, decreasing = T) # Sort in a descending way
  
  colors37 = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d",
               "#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c",
               "#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d",
               "#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3",
               "#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d",
               "#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b",
               "#bd5975")
  
  bar.plt.script <- barplot(c,
                            names.arg = names(c),
                            horiz = F,
                            col = colors37,
                            main = "Portfolio Stocks by Market Capitalisation",
                            ylab = "Market Capitalisation Level",
                            ylim = c(0, ceiling(max(c))),
                            las = 2)
  
  abline(v = bar.plt.script, col = "grey", lty = 3) # Vertical lines
  
  box() # Add box
  
  m <- NULL # Write advices about securities according to Market Capitalisation
  
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
    
    m <- c(m, paste("Micro-Cap Companies:", toString(names(which(c < .3))))) }
  
  if (isFALSE(identical(l, character(0))) | !is.null(l)){
    
    m <- c(m, paste("Market Cap Info is unavailable:", toString(l))) }
  
  m # Display
}
p.bar.plt.marketcap(df_portfolio) # Test
