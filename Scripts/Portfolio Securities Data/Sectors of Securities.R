library(rvest) 

p.sector <- function(x, sector = F){ # Get sector info for portfolio stocks
  
  x = colnames(x[,1 + 3 * seq(ncol(x) %/% 3, from = 0)][,-(ncol(x) %/% 3 + 1)]) 
  
  L <- NULL # Create list
  
  for (n in 1:length(x)){ y <- x[n] # Get data
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                           tolower(y))) %>% html_nodes('body') %>%
      html_nodes('main') %>% html_nodes('div') %>% html_nodes('a') 
    
    L <- rbind.data.frame(L, p[grep("sector", p)] %>% html_text()) } 

  colnames(L) <- "Sector"
  rownames(L) <- x # tickers
  
  if (sector){ # If you want to know companies belong to each sector
    
    df <- data.frame(Ticker=rownames(L), Sector=L$Sector, stringsAsFactors=F)
    
    L <- aggregate(Ticker ~ Sector, data = df,
                   FUN = function(x) paste(x, collapse = ", ")) }
  L # Display
}
p.sector(df_portfolio, T) # Test
