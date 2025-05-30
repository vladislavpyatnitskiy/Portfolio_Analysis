lapply(c("ggplot2", "ggrepel", "rvest", "xml2", "httr"),
       require, character.only = T) # Libraries

p.bubble.plt <- function(x){ # Bubble Plot of Portfolio Securities
  
  x <- x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x)%/%3+1)] # Data
  
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
    
    s <- j %>% .[[1]] %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s <- s[grep("Market cap", s) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 2, 1),
                  colClasses = "character")
    
    v <- as.numeric(s[1,1]) # Make data numeric
    
    s <- switch(s[1,2], "M" = v / 1000, "B" =  v, "T" = v * 1000)
    
    d <- rbind.data.frame(d, cbind(sd(r) * 1000, (exp(sum(r))-1)*100, s, f)) } 
  
  rownames(d) <- colnames(x) # Row names
  colnames(d) <- c("SD", "Return", "Market Cap", "Sector")
    
  for (n in 1:(ncol(d) - 1)){ d[,n] <- as.numeric(d[,n]) }
  
  # Plot
  ggplot(
    data = d,
    mapping = aes(
      x = d[,"SD"],
      y = d[,"Return"],
      size = d[,"Market Cap"],
      color = d[,"Sector"],
      label=d[,"Sector"])
    ) +
    geom_point() +
    labs(
      title = "Bubble Plot of Portfolio Securities by Risk and Return",
      x = "Risk (Standard Deviation)",
      y = "Return (%)",
      size = "Market Capitalisation (US$ Billions)",
      color = "Sector"
      ) + 
    geom_text_repel(
      aes(
        label = rownames(d),
        fill = d[,4],
        size = NULL,
        color = NULL),
      nudge_y = .0125
      ) +
    theme_minimal() +
    scale_size_continuous(
      breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)
      ) +
    theme(plot.title = element_text(hjust = .5)) +
    guides(
      fill=guide_legend(title = "Sector", override.aes = aes(label = ""))
      )
}
p.bubble.plt(df_portfolio) # Test
