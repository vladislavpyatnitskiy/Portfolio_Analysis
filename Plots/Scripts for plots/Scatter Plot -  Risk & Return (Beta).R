lapply(c("quantmod","timeSeries","ggplot2","ggrepel", "rvest"),
       require,character.only=T)

# Scatter plot of Portfolio Securities' Risk & Return (Beta) 
p.scatter.plt.beta <- function(x, main = NULL, xlab = NULL, ylab = NULL){
  
  x <- x[,1 + 3 * seq(31, from = 0)] # Select securities columns
  
  d <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each security in data frame
    
    c <- colnames(s) #
    
    p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",c,c)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    b <- as.numeric(i[grep("Beta ", i) + 1]) # Join betas
    
    if (is.na(b)){ l <- c(c, "^GSPC") # 
    
      b <- NULL #
      
      for (m in l){ b <- cbind(b, getSymbols(m, from=as.Date(Sys.Date())-365*5,
                                             to=Sys.Date(), src="yahoo",
                                             auto.assign=F)[,4])}
    
      b <- b[apply(b, 1, function(x) all(!is.na(x))),] # Get rid of NA
      
      b = diff(log(as.timeSeries(b)))[-1,] # Calculate Returns
      
      b <- as.numeric(apply(b[,1], 2,
                            function(col) ((lm((col) ~
                                                 b[,2]))$coefficients[2]))) }
      
    # Clean data to reduce NA and calculate return for ownership period  
    j <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    d <- rbind.data.frame(d, cbind(b, (exp(sum(j)) - 1) * 100)) } # Join
  
  # Plot
  ggplot(d, mapping = aes(x=d[,1], y=d[,2])) + geom_point() +
    theme_minimal() + geom_smooth(method = 'lm', se = F, col = "red") +
    geom_text_repel(aes(label=colnames(x))) + labs(title=main,x=xlab,y=ylab)
}
# Test
p.scatter.plt.beta(df_portfolio, main = "Securities Performance",
                   xlab = "Risk (Beta)", ylab = "Return (%)")
