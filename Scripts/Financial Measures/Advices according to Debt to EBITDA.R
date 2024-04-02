library("rvest") # Library

p.Debt.EBITDA.advices <- function(x){
  
  x <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)])
  
  db <- NULL # List for Debt to EBITDA values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    # Find values of Debt and EBITDA  
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", u[grep("EBITDA", u)+1][1])) 
    h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", y[grep("Total Debt",y)+1])) 
    
    db <- rbind(db, as.numeric(h) / as.numeric(c)) } # Add values
    
    rownames(db) <- x # Assign tickers as row names to debt/ebitda values
    
    l <- NULL # Find securities without debt/ebitda values
    
    for (n in 1:nrow(db)){ if (is.na(db[n,])){ l <- c(l, rownames(db)[n])} }
    
  v <- as.numeric(db) # Make available values to numeric format
  
  names(v) <- x # Assign names to them
  
  v <- sort(v, decreasing = T) # Sort in a descending way
  
  extreme_values <- subset(v, v < 0) # Put unconventional values to new list
  
  v <- subset(v, v > 0) # Put conventional values to different list
  
  m <- NULL # Write advices about securities according to Debt/EBITDA ratios
  
  if (isFALSE(identical(names(which(v > 4)), character(0)))){
    
    m <- c(m, paste("Dangerously indebted companies:",
                    toString(names(which(v > 4))))) }
  
  if (isFALSE(identical(names(which(v > 3.5 & v < 4)), character(0)))){
    
    m <- c(m, paste("Overly indebted companies:",
                    toString(names(which(v > 3.5 & v < 4))))) }
  
  if (isFALSE(identical(names(which(v > 3 & v < 3.5)), character(0)))){
    
    m <- c(m, paste("Highly indebted companies:",
                    toString(names(which(v > 3 & v < 3.5))))) }
  
  if (isFALSE(identical(names(which(v > 2.5 & v < 3)), character(0)))){
    
    m <- c(m, paste("Indebted Companies:",
                    toString(names(which(v > 2.5 & v < 3))))) }
  
  if (isFALSE(identical(names(which(v > 2 & v < 2.5)), character(0)))){
    
    m <- c(m, paste("OK Debt Level:",
                    toString(names(which(v > 2 & v < 2.5))))) }
  
  if (isFALSE(identical(names(which(v > 1.5 & v < 2)), character(0)))){
    
    m <- c(m, paste("Comfort Debt Level:",
                    toString(names(which(v > 1.5 & v < 2))))) }
  
  if (isFALSE(identical(names(which(v > 1 & v < 1.5)), character(0)))){
    
    m <- c(m, paste("Perfect Debt Level:",
                    toString(names(which(v > 1 & v < 1.5))))) }
  
  if (isFALSE(identical(names(which(v < 1)), character(0)))){
    
    m <- c(m, paste("Ideal Debt Level:", toString(names(which(v < 1))))) }
  
  if (isFALSE(identical(l, character(0)))){
    
    m <- c(m, paste("Debt Level is unavailable:", toString(l))) }
  
  if (isFALSE(identical(names(which(extreme_values < 0)), character(0)))){
    
    m <- c(m, paste("Check EBITDA value of these companies:",
                    toString(names(which(extreme_values < 0))))) }
  m # Display
}
p.Debt.EBITDA.advices(df_portfolio) # Test
