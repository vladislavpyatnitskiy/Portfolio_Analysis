lapply(c("quantmod", "timeSeries", "rvest"), require, character.only = T) #Libs 

p.pie.plt.beta <- function(x){ # Portfolio Pie by Betas 
  
  y <- round(as.data.frame(t(as.data.frame(x[,3*seq(ncol(x)%/%3,
                                                    from=1)][nrow(x),]/
                                             as.numeric(x[nrow(x),
                                                          ncol(x)])))),2)*100
  
  x <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,1:(ncol(x) %/% 3)])
  
  df <- NULL # Create list to contain values
  
  for (n in 1:length(x)){ # Read HTML & extract necessary info
    
    p <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/%s", x[n],
                           "key-statistics")) 
    
    tab <- p %>% html_nodes('div') %>% .[[1]]
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    b <- as.numeric(i[grep("Beta ", i) + 1]) # Select Beta value
    
    if (is.na(b)){ l <- c(x[n], "^GSPC") # When Beta is not available
      
      b <- NULL # Download data from Yahoo! Finance directly
      
      for (m in l){ b <- cbind(b, getSymbols(m, from=as.Date(Sys.Date())-365*5,
                                             to=Sys.Date(), src="yahoo",
                                             auto.assign=F)[,4])}
      
      b <- b[apply(b, 1, function(x) all(!is.na(x))),] # Get rid of NA
      
      b = diff(log(as.timeSeries(b)))[-1,] # Calculate Returns
      
      b <- as.numeric(apply(b[,1], 2,
                            function(col) ((lm((col) ~
                                                 b[,2]))$coefficients[2]))) }
      
    if (b < 0){ l <- "Negative" } # Negative Beta
    
    else if (b > 0 && b < .5) { l <- "Low" } # Low Beta
    
    else if (b > .5 && b < 1) { l <- "Moderate" } # Moderate Beta
    
    else if (b > 1 && b < 1.5) { l <- "Market Level" } # Market Level Beta
    
    else if (b > 1.5 && b < 2) { l <- "Volatile" } # Volatile Beta
    
    else { l <- "Extreme" } # Extreme Beta
    
    df <- rbind(df, cbind(round(b, 2), l)) } # Join betas
  
  rownames(df) <- x # Assign row names
  rownames(y) <- x
  
  df <- cbind(df, y)
  
  colnames(df) <- c("Beta", "Level", "Portion") # Assign column names
  
  df <- df[,-1]
  
  df <- aggregate(Portion ~ Level, data=df, sum) # Conditional sum
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc")
  
  pie(df[,2], labels=c(sprintf("%s %s%%", df[,1], df[,2])), col=C, radius=1.2,
      main = "Portfolio Securities by Beta") # Plot
}
p.pie.plt.beta(df_portfolio) # Test
