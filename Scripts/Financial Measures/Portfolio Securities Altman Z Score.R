library("rvest") # Library

p.altmanz.score <- function(x){ # Function to get Altman Z Score
  
  x <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)])
  
  l <- NULL 
  
  for (n in 1:length(x)){ a <- x[n]
    
    bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s",a,a)
    re <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",a,a)
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", a, a)
    
    page.bs <- read_html(bs) # Read HTML & extract Balance Sheet info
    page.is <- read_html(is) # Income Statement
    page.re <- read_html(re) # Payout Ratio
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    price.yahoo3 <- page.re %>% html_nodes('div') %>% .[[1]] -> tab.re
    price.yahoo4 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab4
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    w <- tab.re %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    p <- tab4 %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    if (isFALSE(any(y=="Working Capital"))){ # If Working Capital is off
      
      w <- cbind(p[grep("Sector", p) + 1], "NA", "NA")
      
      colnames(w) <- c("Sector" ,"Value", "Status")
      
      l <- rbind.data.frame(l, w) # Add Sector with NA
      
    } else { b <- w[grep("Payout Ratio ", w) + 1] # Payout Ratio
    
    b <- as.numeric(read.fwf(textConnection(b), widths = c(nchar(b) - 1, 1),
                             colClasses = "character")[, 1]) / 100
    c <- NULL
    h <- NULL
    
    k <- c("Total Assets", "Total Liabilities Net Minority Interest",
           "Total Equity Gross Minority Interest", "Working Capital")
    
    r <- c("EBIT", "Total Revenue", "Net Income Common Stockholders")
    
    for (m in 1:length(r)){ c <- rbind(c, u[grep(r[m], u) + 1][1]) }
    for (m in 1:length(k)){ h <- rbind(h, y[grep(k[m], y) + 1][1]) }
    
    c <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)))
    h <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)))
    
    A <- h[4] / h[1]  # Working Capital/Total Assets
    B <- (1 - b) * c[3] / h[1] # Retention/Total Assets  
    C <- c[1] / h[1] # EBIT / Total Assets
    D <- h[3] / h[2] # Equity / Total Liabilities
    E <- c[2] / h[1] # Total Revenue / Total Assets
    
    if (isTRUE(p[grep("Sector", p) + 1] == "Technology")){ 
      
      a.r = round(6.56 * A + 3.26 * B + 6.72 * C + 1.05 * D, 2)
      
    } else { a.r = round(1.2 * A + 1.4 * B + 3.3 * C + .6 * D + E, 2) }
    
    if (a.r > 2.6){ S <- "Safe" } else if (a.r < 2.6 && a.r > 1.1){
      
      S <- "Grey" } else { S <- "Distress" } # Financial Health Status
    
    w <- cbind(p[grep("Sector", p) + 1], a.r, S)
    
    colnames(w) <- c("Sector" ,"Value", "Status") 
    
    l <- rbind.data.frame(l, w) } } # Add to data frame
    
  rownames(l) <- x # Assign row names
  
  l # Display
}
p.altmanz.score(df_portfolio) # Test
