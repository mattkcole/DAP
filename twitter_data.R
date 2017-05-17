library(twitteR)
library(lubridate)
# incorporating google trends data
library(readr)
library(gtrendsR)
library(lubridate)

# be sure to have run 'getting_data.R'

# reading in data
hr.115 <- read_csv("data/hr115.csv")
# removing rows with only NA values
hr.115 <- hr.115[apply(hr.115,1,function(x) sum(is.na(x))) != 21, ]

# initializing vectors
interest <- list()
cutoff <- 50
tweets <- list()
for (i in 1:nrow(hr.115)){
  
  # Saving querry
  query <- hr.115[i,]$short_title
  
  if(nchar(query) > 60){
    spaces <- gregexpr(pattern =' ',query)[[1]]
    spaces <- spaces[1:length(spaces)]
    entry <- which(abs(spaces-cutoff)==min(abs(spaces-cutoff)))
    
    query <- substr(query, 1, spaces[entry])
  }
  
  tweets[[i]] <- suppressWarnings(
    searchTwitter(query, 
                          n = 60, 
                          since = as.character(ymd(hr.115[i,]$introhousedate)),
                          until = as.character(ymd(hr.115[i,]$introhousedate) + days(10)))
  )
  print(i)
  remaining <- getCurRateLimitInfo() %>% 
    as_data_frame() %>% 
    dplyr::filter(resource == "/search/tweets") %>% 
    select(remaining)
  if (remaining < 1){
    Sys.sleep(getCurRateLimitInfo()[62,4] - Sys.time())
    Sys.sleep(1)
  }
}
library(dplyr)

getCurRateLimitInfo() %>% 
  as_data_frame() %>% 
  dplyr::filter(resource == "/search/tweets") %>% 
  select(remaining)


