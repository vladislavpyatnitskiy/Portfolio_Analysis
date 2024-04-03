lapply(c("rvest","plotly","tidyverse"), require, character.only=T) # Libraries

p.bubble.plt.3d <- function(x, title = "Stocks"){ # 3D Bubble Plot
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x)%/%3+1)] 
  
  d <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(x)){ c <- colnames(x)[n] # names of securities
  
    r <- diff(log(x[,n][apply(x[,n],1,function(row) all(row !=0 )),]))[-1,]
    
    p <- read_html(sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s",
                           c, c)) # Read HTML & extract necessary info
    
    price.yahoo1 <- p %>% html_nodes('div') %>% .[[1]] -> tab11
    
    m <- tab11 %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    j <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",c,c)
    
    page.p <- read_html(j) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    b <- data.frame(i[grep("Beta ", i) + 1]) # Scrape Beta value
    
    s <- read.fwf(textConnection(i[2]), widths = c(nchar(i[2]) - 1, 1),
                  colClasses = "character") 
    
    if (s[1,2] == "M"){ s <- as.numeric(s[1,1])/1000 } else if (s[1,2] == "T"){
      
      s <- as.numeric(s[1, 1]) * 1000 } else s <- as.numeric(s[1, 1]) 
    
    d <- rbind.data.frame(d, cbind(sd(r) * 1000, (exp(sum(r)) - 1) * 100, s,
                                   m[2], b)) } # Join
    
  rownames(d) <- colnames(x) # Row names
  
  # Plot 3D Bubble Plot
  fig <- plot_ly(d, x = ~d[,1], y = ~d[,5], z = ~d[,2], size=~d[,3], 
                 color=~d[,4],marker=list(symbol='circle',sizemode='diameter'),
                 sizes = c(5, 150), text=~paste('Ticker:',rownames(d)))
  # Rename axes
  fig %>% layout(title=title,scene=list(xaxis=list(title='Standard Deviation'),
                                        yaxis=list(title='Beta'),
                                        zaxis=list(title = 'Return (%)')))
}
p.bubble.plt.3d(df_portfolio) # Test
