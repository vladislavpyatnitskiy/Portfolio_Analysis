lapply(c("rvest", "httr", "xml2"), require, character.only = T) # Libs

p.bar.plt.marketcap <- function(x){ # Market Cap info via string or table
  
  x <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  x <- x[-grep("VSTO", x)] # Reduce obsolete tickers
  x <- x[-grep("ARCH", x)] 
  
  j <- list(list(10, 200, "Large-Cap Companies:", "Large-Cap"), # > 10 & < 200
            list(2, 10, "Medium-Cap Companies:", "Medium-Cap"), # > 2 & < 10
            list(0.3, 2, "Small-Cap Companies:", "Small-Cap"), # > 0.3 & < 2
            list(0, 0.3, "Micro-Cap Companies:", "Micro-Cap")) # > 0 & < 0.3
  
  D <- NULL # Data Frame for Market Cap Levels and Values
  
  for (n in 1:length(x)){ # Read HTML & extract necessary info
    
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/%s/",
                            x[n], "key-statistics"),
                    add_headers(`User-Agent` = B))
    
    i <- read_html(content(response, as = "text", encoding = "UTF-8")) %>%
      html_nodes('table') %>% .[[1]] %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text()
    
    s <- i[grep("Market cap", i) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 2, 1),
                  colClasses = "character")
    
    v <- as.numeric(s[1,1]) # Make data numeric
    
    D <- c(D, switch(s[1,2], "M" = v / 1000, "B" =  v, "T" = v * 1000)) }
  
  names(D) <- x # Assign tickers to vector
  
  l <- NULL # Find securities without available market cap values
  
  for (n in 1:length(D)){ if (is.na(D[n])){ l <- c(l, names(D)[n]) } }
  
  D <- sort(D, decreasing = T) # Sort in a descending way
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975")
  
  options(scipen = 999)  # Turn off scientific notation globally
  
  B <- barplot(
    D,
    horiz = F,
    log = "y",
    col = C,
    yaxt = "n",
    main = "Portfolio Stocks by Market Capitalisation in $billions",
    las = 2
    )
  
  nums <- c(1, 10, 100, 1000) # Values for axes
  
  for (n in 1:2){ axis(n * 2, at = nums, labels = nums, las = 2) }
  
  grid(nx = 1, ny = NULL, col = "grey", lty = 3, lwd = 1) # Horizontal lines
  
  abline(v = B, col = "grey", lty = 3) # Vertical lines
  
  par(mar = rep(5, 4)) # Define borders of the plot
  
  box() # Add box
  
  m <- NULL # Write advices about securities according to Market Cap
  
  if (isFALSE(identical(names(which(D > 200)), character(0)))){
    
    m <- c(m, paste("Mega-Cap Companies:", toString(names(which(D > 200)))))}
  
  for (n in 1:length(j)){ #
    
    if (isFALSE(identical(names(which(D > j[[n]][[1]] & D < j[[n]][[2]])),
                          character(0)))){
      m <- c(m,
             paste(j[[n]][[3]],
                   toString(names(which(D>j[[n]][[1]] & D<j[[n]][[2]])))))} }
  
  if (!is.null(l)){ if (isFALSE(identical(l, character(0)))){
    
      m <- c(m, paste("Market Cap Info is unavailable:", toString(l))) } }
  
  m # Display
}
p.bar.plt.marketcap(df_portfolio)
