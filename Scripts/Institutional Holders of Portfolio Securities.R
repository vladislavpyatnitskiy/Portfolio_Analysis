library("rvest") # Library

p.holders <- function(x){ # info about holders of large positions
  
  x <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3,from = 0)])[-(ncol(x) %/% 3 + 1)] 
  
  d <- NULL
  
  for (m in 1:length(x)){ c <- x[m]
    
    y <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/holders",
                           c)) %>% html_nodes('table') %>% .[[2]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text() # Get values
    
    v <- cbind.data.frame(y[seq(from = 1, to = length(y), by = 5)],
                          y[seq(from = 4, to = length(y), by = 5)])
  
    for (n in 1:nrow(v)){ v[n,2] <- strsplit(v[n,2], split = "%") }
    
    v[,2] <- as.numeric(v[,2]) # Change format to numeric
      
    v[nrow(v) + 1,] = c("Others", 100 - sum(v[,2])) # Numbers for others
      
    colnames(v) <- c("Top Institutional Holders", c) # Column names
    rownames(v) <- seq(nrow(v)) # Row names
    
    if (is.null(d)){ d <- v } else { # Full Join
      
      d <- merge(x = d, y = v, by = "Top Institutional Holders", all = T) } }
  
  d # Display
}
View(p.holders(df_portfolio)) # Test
