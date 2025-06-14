library(rvest) # Library

p.industry <- function(x, agg = F){ # Get sector info for portfolio stocks
  
  x = colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x) %/% 3 + 1)]) 
  
  L <- NULL # Create list
  
  for (n in 1:length(x)){ y <- x[n] # Get data
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                           tolower(y))) %>% html_nodes('body') %>%
      html_nodes('main') %>% html_nodes('div') %>% html_nodes('a') 
    
    L <- rbind.data.frame(L, p[grep("industry", p)] %>% html_text()) } 

  colnames(L) <- "Industry"
  rownames(L) <- x # tickers
  
  if (agg){ # If you want to know companies belong to each industry
    
    df <- data.frame(Ticker=rownames(L),Industry=L$Industry,stringsAsFactors=F)
    
    L <- aggregate(Ticker ~ Industry, data = df,
                   FUN = function(x) paste(x, collapse = ", ")) }
  L # Display
}
p.industry(df_portfolio, F) # Test
