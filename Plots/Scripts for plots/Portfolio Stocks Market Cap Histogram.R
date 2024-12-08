library("rvest") # Library

p.hist.plt.marketcap <- function(x){ # Histogram of Market Caps
  
  x <- colnames(x[, 1 + 3 * seq(ncol(x) %/% 3, from = 0)])[-(ncol(x) %/% 3 + 1)]
  
  D <- NULL # Data Frame for Market Cap values
  
  for (n in 1:length(x)){ v <- x[n] # Subset ticker
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/statistics/",
                           tolower(v))) %>% html_nodes('div') %>% .[[1]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s <- p[grep("Market Cap", p) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 1, 1),
                  colClasses = "character")
  
    l <- as.numeric(s[1,1])
    
    D <- rbind(D, switch(s[1,2], "M" = l / 1000, "B" =  l, "T" = l * 1000)) }

  hist(D, main = "Portfolio Market Cap Histogram", freq = F, breaks = 100,
       ylab = "Probability", xlab = "Market Cap Values in $Billions", las = 1,
       col = "navy", xlim = c(min(D), max(D)), border = "white") # Parameters
  
  abline(h = 0) # Add vertical line at x = 0
  
  box() # Define plot borders
}
p.hist.plt.marketcap(df_portfolio) # Test
