lapply(c("ggplot2","ggrepel","rvest"),require,character.only=T) # Libraries

p.bubble.plt <- function(x){ # Bubble Plot of Portfolio Securities
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x)%/%3+1)] # Data
  
  d <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(x)){ c <- colnames(x[,n]) # Take ticker and clean data
  
    s.adj <- diff(log(x[,n][apply(x[,n],1,function(row) all(row !=0 )),]))[-1,]
    
    v <- cbind(sd(s.adj)*1000, (exp(sum(s.adj)) - 1)*100) # Join sd and return
    
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", c, c)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
    
    y <- tab11 %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    j <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",c,c)
    
    s.page <- read_html(j) # Read HTML of page
    
    s.yahoo <- s.page %>% html_nodes('table') %>% .[[1]] -> tab1 # Assign Table 
    
    m <- tab1 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    m <- read.fwf(textConnection(m[2]), widths = c(nchar(m[2]) - 1, 1),
                  colClasses="character")
    
    if (m[1,2] == "M"){ m <- as.numeric(m[1,1])/1000 } else if (m[1,2] == "T"){ 
      
      m <- as.numeric(m[1,1]) * 1000 } else m <- as.numeric(m[1,1]) # Format 
    
    new.info <- data.frame(m, y[2]) # Join market cap data with sector info
    
    rownames(new.info) <- c
    rownames(v) <- c # Give row names to data frame
    
    d <- rbind.data.frame(d, cbind(v, new.info)) } # Join stats with other info
    
  # Plot
  ggplot(data = d, mapping = aes(x = d[,1], y = d[,2], size = d[,3],
                                 color = d[,4], label=d[,4])) + geom_point() +
    labs(title = "Bubble Plot of Portfolio Securities by Risk and Return",
         x = "Risk (Standard Deviation)", y = "Return (%)",
         size = "Market Capitalisation (US$ Billions)", color = "Sector") + 
    geom_text_repel(aes(label = rownames(d), fill = d[,4], size = NULL,
                        color = NULL), nudge_y = .0125) + theme_minimal() +
    scale_size_continuous(breaks = c(1,2,5,10,20,50,100,200,500,1000,2000)) +
    theme(plot.title = element_text(hjust = .5)) +
    guides(fill=guide_legend(title = "Sector", override.aes = aes(label = "")))
}
p.bubble.plt(df_portfolio) # Test
