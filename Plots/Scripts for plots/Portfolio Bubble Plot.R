lapply(c("ggplot2","ggrepel","rvest"),require,character.only=T) # Libraries

p.bubble.plt <- function(x, main = NULL, xlab = NULL, ylab = NULL){
  
  x <- x[,1 + 3 * seq(31, from = 0)] # Select securities columns
  
  df.scatter <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each security in data frame
  
    c <- colnames(s)
    
    # Clean data to reduce NA and calculate return for ownership period  
    s.adj <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    # Join standard deviation and return
    v.scatter <- cbind(sd(s.adj) * 1000, (exp(sum(s.adj)) - 1) * 100)
    
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", c, c)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
    
    yahoo.header1 <- tab11 %>% html_nodes('p') %>% html_nodes('span') %>%
      html_text()
    
    j <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",c,c)
    
    s.page <- read_html(j) # Read HTML of page
    
    s.yahoo <- s.page %>% html_nodes('table') %>% .[[1]] -> tab1 # Assign Table 
    
    s.header <- tab1 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s.header <- s.header[2] # Info about market capitalisation
    
    s.header<-read.fwf(textConnection(s.header),widths=c(nchar(s.header)-1,1),
                       colClasses = "character")
    
    if (s.header[1,2] == "M"){ s.header <- as.numeric(s.header[1,1]) / 1000 }
    
    else if (s.header[1,2] == "T"){ s.header<-as.numeric(s.header[1,1])*1000 }
    
    else s.header <- as.numeric(s.header[1,1]) # Format to billion format
    
    new.info <- data.frame(s.header, yahoo.header1[2])
    
    rownames(new.info) <- colnames(s)
    rownames(v.scatter) <- colnames(s) # Give row names to data frame
    
    n.scatter <- cbind.data.frame(v.scatter,new.info) # Join data about company
    
    df.scatter<-rbind.data.frame(df.scatter,n.scatter) } # Join all companies
  
  # Plot
  ggplot(data = df.scatter, mapping = aes(x = df.scatter[,1],
                                          y = df.scatter[,2],
                                          size = df.scatter[,3],
                                          color = df.scatter[,4],
                                          label=df.scatter[,4])) +
    geom_point() +
    labs(title=main,x=xlab,y=ylab,size="Market Capitalisation",
         color="Sector") + theme_minimal() +
    geom_text_repel(aes(label = rownames(df.scatter), fill = df.scatter[,4],
                        size = NULL, color = NULL), nudge_y = .0125) + 
    guides(fill=guide_legend(title = "Sector", override.aes = aes(label = "")))
}
# Test
p.bubble.plt(df_portfolio, main = "Portfolio Securities",
             xlab = "Risk (Standard Deviation)", ylab = "Return (%)")
