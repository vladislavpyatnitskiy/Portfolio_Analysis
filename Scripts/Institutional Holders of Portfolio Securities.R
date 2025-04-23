lapply(c("rvest", "httr", "xml2"), require, character.only = T) # Libs

p.holders <- function(x){ # info about holders of large positions
  
  x <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3,from = 0)])[-(ncol(x) %/% 3 + 1)] 
  
  x <- x[-grep("VSTO", x)] 
  x <- x[-grep("ARCH", x)] 
  
  d <- NULL
  
  for (m in 1:length(x)){ s <- x[m]
    
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/holders/",
                            s), add_headers(`User-Agent` = B))
    
    f <- read_html(content(response, as = "text", encoding = "UTF-8")) %>%
      html_nodes('table') %>% .[[2]] %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text() # Get values
    
    v <- cbind.data.frame(f[seq(from = 1, to = length(f), by = 5)],
                          f[seq(from = 4, to = length(f), by = 5)])
    
    for (n in 1:nrow(v)){ v[n,2] <- strsplit(v[n,2], split = "%") }
    
    v[,2] <- as.numeric(v[,2]) # Change format to numeric
    
    v[nrow(v) + 1,] = c("Others", 100 - sum(v[,2])) # Numbers for others
    
    colnames(v) <- c("Top Institutional Holders", s) 
    rownames(v) <- seq(nrow(v)) # Row names
    
    if (is.null(d)){ d <- v } else { # Full Join
      
      d <- merge(x = d, y = v, by = "Top Institutional Holders", all = T) } }
    
  d # Display
}
View(p.holders(df_portfolio)) # Test
