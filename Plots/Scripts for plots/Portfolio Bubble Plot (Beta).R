# Library
lapply(c("quantmod", "timeSeries", "ggplot2", "ggrepel", "rvest"), require,
       character.only=T)

p.bubble.plt.beta <- function(x){ # Bubble of Portfolio Securities (Beta)
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x)%/%3+1)] # Data
  
  d <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(x)){ c <- colnames(x[,n]) # Take ticker and clean data
    
    s.adj <- diff(log(x[,n][apply(x[,n],1,function(row) all(row !=0 )),]))[-1,]
    
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", c, c)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
    
    y <- tab11 %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    j <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",c,c)
    
    s.page <- read_html(j) # Read HTML of page
    
    s.yahoo <- s.page %>% html_nodes('table') %>% .[[1]] -> tab1 # Assign Table 
    s1 <- s.page %>% html_nodes('table') %>% .[[2]] -> tab2 # Assign Table
    
    i <- tab1 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    h <- tab2 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    m <- read.fwf(textConnection(i[2]), widths = c(nchar(i[2]) - 1, 1),
                  colClasses = "character")
    
    if (m[1,2] == "M"){ m <- as.numeric(m[1,1])/1000 } else if (m[1,2] == "T"){ 
      
      m <- as.numeric(m[1,1]) * 1000 } else m <- as.numeric(m[1,1]) # Format 
    
    new.info <- data.frame(m, y[2]) # Join market cap data with sector info
    
    b <- as.numeric(h[grep("Beta ", h) + 1]) # Join betas
    
    if (is.na(b)){ l <- c(c, "^GSPC") # When Beta is not available
    
      b <- NULL #
      
      for (m in l){ b <- cbind(b, getSymbols(m, from=as.Date(Sys.Date())-365*5,
                                             to = Sys.Date(), src = "yahoo",
                                             auto.assign = F)[,4])}
      
      b <- b[apply(b, 1, function(x) all(!is.na(x))),] # Get rid of NA
      
      b = diff(log(as.timeSeries(b)))[-1,] # Calculate Returns
      
      b <- as.numeric(apply(b[,1], 2,
                            function(col) ((lm((col) ~
                                                 b[,2]))$coefficients[2]))) }
      
    v <- cbind(b, (exp(sum(s.adj)) - 1) * 100) # Join sd and return
    
    rownames(new.info) <- c
    rownames(v) <- c # Give row names to data frame
    
    d <- rbind.data.frame(d, cbind(v, new.info)) } # Join
    
  # Plot
  ggplot(data = d, mapping = aes(x = d[,1], y = d[,2], size = d[,3],
                                 color = d[,4], label=d[,4])) + geom_point() +
    labs(title = "Bubble Plot of Portfolio Securities by Risk and Return",
         x = "Risk (Beta)", y = "Return (%)",
         size = "Market Capitalisation (US$ Billions)", color = "Sector") +
    theme_minimal() +
    geom_text_repel(aes(label = rownames(d), fill = d[,4], size = NULL,
                        color = NULL), nudge_y = .0125) +
    scale_size_continuous(breaks = c(1,2,5,10,20,50,100,200,500,1000, 2000)) +
    theme(plot.title = element_text(hjust = .5)) +
    guides(fill=guide_legend(title = "Sector", override.aes = aes(label = "")))
}
p.bubble.plt.beta(df_portfolio) # Test
