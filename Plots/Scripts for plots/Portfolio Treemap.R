lapply(c("treemap", "rvest"), require, character.only = T) # Libraries

p.treemap <- function(x, sector = F){  v <- x[,3+3*seq(31,from=0)] # Total sum

  v <- c(v[nrow(v),]) # Create vector to contain total sum of securities
  
  r <- x[,3 + 3 * seq(31, from = 0)]/x[,2 + 3 * seq(31, from = 0)] # Mean Sum
  
  ticker <- colnames(x[, 1 + 3 * seq(31, from = 0)]) # Assign tickers
  
  if (isFALSE(sector)){ l <- NULL # Create empty variable to contain values
  
  for (n in 1:ncol(r)){ s <- r[,n] # Return calculation
  
    l <- rbind(l, round((exp(sum(diff(log(s[!is.nan(s)]))[-1]))-1)*100, 2)) }
  
  p.data <- data.frame(v, l) # Join vectors into data set
  
  p.data$label <- sprintf("%s: %s%%", ticker, p.data$l) # Tickers with return
  
  # Plot
  treemap(p.data,index=c("label"),vSize="v",type="manual",palette = "RdYlGn",
          vColor="l",title="Portfolio Securities",title.legend="Return (%)")
  
  } else { l <- NULL
    
    k <- NULL
    
    for (n in 1:ncol(r)){ s <- r[,n] # Calculate Return
      
      l <- rbind(l, round((exp(sum(diff(log(s[!is.nan(s)]))[-1]))-1)*100, 2))
    
      c <- ticker[n] # Get info about sector
      
      p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", c, c)
      
      page.p <- read_html(p) # Read HTML & extract necessary info
      
      price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
      
      y <- tab11 %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
      
      k <- rbind(k, y[2]) } # Join
    
    p.data <- data.frame(v, l, k) # Join vectors into data set
    
    p.data$label <- sprintf("%s: %s%%", ticker, p.data$l) # Tickers with return
    
    # Plot
    treemap(p.data, index=c("k","label"), vSize="v", type="manual",
            palette = "RdYlGn", vColor="l", bg.labels=c("transparent"),
            align.labels=list(c("center", "center"), c("right", "bottom")),
            title = "Portfolio Securities", title.legend = "Return (%)") }
}
p.treemap(df_portfolio, sector = T) # Test
