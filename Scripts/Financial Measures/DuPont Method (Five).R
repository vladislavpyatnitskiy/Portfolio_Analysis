library("rvest") # library

p.DuPont.five <- function(x){ # DuPont Method ratios
  
  x <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)])
  
  dupont <- NULL # List for DuPont Method values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
    
    bs<-sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is<-sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    c <- NULL
    h <- NULL
    
    p <- c("Total Assets", "Total Equity Gross Minority Interest")
    r <- c("Net Income Common Stockholders", "Pretax Income", "EBIT",
           "Total Revenue")
    
    for (m in 1:length(r)){ c <- rbind(c, u[grep(r[m], u) + 1][1]) }
    for (m in 1:length(p)){ h <- rbind(h, y[grep(p[m], y) + 1][1]) }
    
    c <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)))
    h <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)))
    
    d.ratios <- cbind(c[1] / h[2], # Return on Equity
                      c[1] / c[2], # Tax Burden
                      c[2] / c[3], # Interest Burden
                      c[3] / c[4], # Margin
                      c[4] / h[1], # Turnover
                      h[1] / h[2]) # Equity Multiplier
    
    dupont <- rbind(dupont, d.ratios) } # DuPont Method
    
  rownames(dupont) <- x # Ticker names
  colnames(dupont) <- c("ROE (%)", "Tax Burden (%)", "Interest Burden (%)",
                        "Margin (%)","Turnover (%)","Equity Multiplier (%)") 
  
  round(dupont * 100, 2) # Display
}
p.DuPont.five(df_portfolio) # Test
