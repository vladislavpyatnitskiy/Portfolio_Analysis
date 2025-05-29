lapply(c("rvest","plotly","tidyverse", "quantmod", "timeSeries", "httr",
         "xml2", "ggplot2", "ggrepel"), require, character.only = T) # Libs

p.bubble.plt.beta <- function(x){ # Bubble of Portfolio Securities (Beta)
  
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
    
    d <- rbind.data.frame(d, cbind((exp(sum(r)) - 1) * 100, s, b, f)) } 
    
  rownames(d) <- colnames(x) # Row names
  colnames(d) <- c("Return", "Market Cap", "Beta", "Sector")
  
  for (n in 1:(ncol(d) - 1)){ d[,n] <- as.numeric(d[,n]) }
  
  # Plot
  ggplot(
    data = d,
    mapping = aes(
      x = d[,"Beta"],
      y = d[,"Return"],
      size = d[,"Market Cap"],
      color = d[,"Sector"],
      label = d[,"Sector"])
    ) +
    geom_point() +
    labs(
      title = "Bubble Plot of Portfolio Securities by Risk and Return",
      x = "Risk (Beta)",
      y = "Return (%)",
      size = "Market Capitalisation (US$ Billions)",
      color = "Sector"
      ) +
    theme_minimal() +
    geom_text_repel(
      aes(label = rownames(d), fill = d[,4], size = NULL, color = NULL),
      nudge_y = .0125
      ) +
    scale_size_continuous(
      breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)
      ) +
    theme(
      plot.title = element_text(hjust = .5)
      ) +
    guides(
      fill = guide_legend(
        title = "Sector", override.aes = aes(label = "")
        )
      )
}
p.bubble.plt.beta(df_portfolio) # Test
