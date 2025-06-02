lapply(c("rvest", "xml2", "httr"), require, character.only = T) # Libs

p.pie.plt.marketcap <- function(x){ # Portfolio Pie Plot by Market Cap
  
  A <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)])[-(ncol(x) %/% 3 + 1)]
  
  A <- A[-grep("VSTO", A)] # Reduce obsolete tickers
  A <- A[-grep("ARCH", A)] 
  
  j <- list(
    list(10, 200,"Large-Cap"), # > 10 & < 200
    list(2, 10, "Medium-Cap"), # > 2 & < 10
    list(0.3, 2, "Small-Cap"), # > 0.3 & < 2
    list(0, 0.3, "Micro-Cap") # > 0 & < 0.3
    ) 
  
  df <- NULL
  
  for (n in 1:length(A)){ 
  
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/%s/",
                            A[n], "key-statistics"),
                    add_headers(`User-Agent` = B))
    
    i <- read_html(content(response, as = "text", encoding = "UTF-8")) %>%
      html_nodes('table') %>% .[[1]] %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text()
    
    s <- i[grep("Market cap", i) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 2, 1),
                  colClasses = "character")
    
    v <- as.numeric(s[1,1]) # Make data numeric
    
    s <- as.numeric(switch(s[1,2], "M" = v / 1000, "B" =  v, "T" = v * 1000))
  
    if (s > 200){ l <- "Mega-Cap" }
    
    for (m in 1:length(j)){
      
      if (s > j[[m]][[1]] & s < j[[m]][[2]]){ l <- j[[m]][[3]] } }
    
    df <- rbind.data.frame(df, l) } # Data Frame 
  
  # Calculate portions
  y <- as.data.frame(x[,3 * seq(ncol(x) %/% 3, from = 1)]) / x[,ncol(x)]
  
  colnames(y) <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)])[-(ncol(x)%/%3+1)]
  
  y <- y[,-which(names(y) == "VSTO")]
  y <- y[,-which(names(y) == "ARCH")]
  
  # Select last period and transform portions into percentages
  y <- t(round(y[nrow(y),] * 100))
  
  rownames(df) <- A # Tickers
  
  df <- cbind.data.frame(df, y) # Join
  
  colnames(df) <- c("Level", "Portion") # column names
  
  df <- aggregate(Portion ~ Level, data = df, sum) # Conditional sum
  
  C = c("#d49f36", "#df462a", "#c76c2d", "#dc4555", "#82702d", "#8c3025")
  
  pie(
    df[,"Portion"],
    labels = c(sprintf("%s %s%%", df[,"Level"], df[,"Portion"])),
    col = C,
    radius = 1.6,
    main = "Portfolio Securities by Market Capitalisation"
    ) # Plot
}
p.pie.plt.marketcap(df_portfolio) # Test
