library("rvest") # Library

p.Debt.EBITDA.advices <- function(x){
  
  x <- colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,1:(ncol(x) %/% 3)])
  
  L <- NULL
  
  for (n in 1:length(x)){ # Get data
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/statistics/",
                           tolower(x[n]))) %>% html_nodes('table') %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    L <- rbind.data.frame(L, p[grep("  Debt / EBITDA  ", p) + 1]) }
  
  rownames(L) <- x # tickers as row names
  colnames(L) <- "Debt/EBITDA" # Column name
  
  l <- NULL
  s <- NULL
  f <- NULL
  k <- NULL
  
  for (n in 1:nrow(L)){ if (isTRUE(L[n,] == "n/a")){ f <- c(f, L[n,])
  
  k <- c(k, rownames(L)[n]) } else { l <- c(l, as.numeric(L[n,]))
  
  s <- c(s, rownames(L)[n]) } }
  
  names(l) <- s
  names(f) <- k
  
  v <- sort(l, decreasing = T) # Sort in a descending way
  
  m <- NULL # Write advices about securities according to Debt/EBITDA ratios
  
  if (isFALSE(identical(names(which(v > 4)), character(0)))){
    
    m <- c(m, paste("Dangerously indebted companies:",
                    toString(names(which(v > 4))))) }
  
  j <- list(list(3.5, 4, "Overly indebted companies:"),
            list(3, 3.5, "Highly indebted companies:"), 
            list(2.5, 3, "Indebted Companies:"), list(2, 2.5, "OK Debt Level:"),
            list(1.5, 2, "Comfort Debt Level:"),
            list(1, 1.5, "Perfect Debt Level:"),
            list(0, 1, "Ideal Debt Level:")) 
  
  for (n in 1:length(j)){ # Messages indicating debt / ebitda levels for stocks
    
    if (isFALSE(identical(names(which(v > j[[n]][[1]] & v < j[[n]][[2]])),
                          character(0)))){
      m <- c(m,
             paste(j[[n]][[3]],
                   toString(names(which(v>j[[n]][[1]] & v<j[[n]][[2]]))))) } }
  
  if (isFALSE(identical(names(f), character(0)))){
    
    m <- c(m, paste("Debt Level is unavailable:", toString(names(f)))) }
  
  if (isFALSE(identical(names(which(v < 0)), character(0)))){
    
    m <- c(m, paste("Check EBITDA value of these companies:",
                    toString(names(which(v < 0))))) }
  m # Display
}
p.Debt.EBITDA.advices(df_portfolio) # Test
