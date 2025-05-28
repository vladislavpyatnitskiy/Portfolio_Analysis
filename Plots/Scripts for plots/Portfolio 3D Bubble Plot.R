lapply(c("rvest","plotly","tidyverse", "quantmod", "timeSeries", "httr",
         "xml2"), require, character.only = T) # Libs

p.bubble.plt.3d <- function(x, title = "Stocks"){ # 3D Bubble Plot
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x) %/% 3 + 1)] 
  
  x <- x[,-which(names(x) == "VSTO")]
  x <- x[,-which(names(x) == "ARCH")]
  
  d <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(x)){ c <- sprintf("https://uk.finance.yahoo.com/quote/%s",
                                     colnames(x)[n])
  
    r <- diff(log(x[,n][apply(x[,n], 1, function(row) all(row !=0 )),]))[-1,]
    
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    l <- paste(c, "/%s/", sep = "") # Paste %s to add profile or statistics
    
    R1 <- GET(sprintf(l, "profile"), add_headers(`User-Agent` = B))
    R2 <- GET(sprintf(l, "key-statistics"), add_headers(`User-Agent` = B))
    
    f <- read_html(content(R1, as = "text", encoding = "UTF-8")) %>%
      html_nodes('div') %>% html_nodes('dl') %>% html_nodes('dd') %>%
      html_nodes('strong') %>% html_text() %>% .[1]
    
    j <- read_html(content(R2, as = "text", encoding = "UTF-8")) %>%
      html_nodes('table')
    
    y <- j %>% .[[8]] %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    s <- j %>% .[[1]] %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    b <- as.numeric(y[grep("Beta ", y) + 1]) # Select Beta value
    
    if (is.na(b)){ l <- c(v, "^GSPC") # When Beta is not available
    
      b <- NULL # Get Stock Price Data and calculate Beta yourself
      
      for (m in l){ b <- cbind(b, getSymbols(m, from=as.Date(Sys.Date())-1825,
                                             to=Sys.Date(), src="yahoo",
                                             auto.assign=F)[,4]) }
      
      b <- b[apply(b, 1, function(x) all(!is.na(x))),] # Get rid of NA
      
      b = diff(log(as.timeSeries(b)))[-1,] # Calculate Returns and Beta
      
      b <- as.numeric(apply(b[,1], 2,
                            function(col) ((lm((col) ~
                                                 b[,2]))$coefficients[2]))) }
    
    s <- s[grep("Market cap", s) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 2, 1),
                  colClasses = "character")
    
    v <- as.numeric(s[1,1]) # Make data numeric
    
    s <- switch(s[1,2], "M" = v / 1000, "B" =  v, "T" = v * 1000)
    
    d <- rbind.data.frame(d, cbind(sd(r)*1000, (exp(sum(r))-1)*100, s, f, b)) } 
    
  rownames(d) <- colnames(x) # Row names
  colnames(d) <- c("SD", "Return", "Market Cap", "Sector", "Beta")
  
  d[,3] <- as.numeric(d[,3])
  
  # Plot 3D Bubble Plot
  plot_ly(
    d,
    x = ~d[,"SD"],
    y = ~d[,"Beta"],
    z = ~d[,"Return"],
    size = ~d[,"Market Cap"],
    color = ~d[,"Sector"],
    marker = list(
      symbol='circle',
      sizemode='diameter'),
    sizes = c(5, 150),
    text = ~paste('Ticker:', rownames(d))) %>%
    layout(
      title = title,
      scene = list(
        xaxis = list(title = 'Standard Deviation'),
        yaxis = list(title='Beta'),
        zaxis = list(title = 'Return (%)')
        )
      )
}
p.bubble.plt.3d(df_portfolio) # Test
