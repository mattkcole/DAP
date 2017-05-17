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
interest.dayof <- vector()
week_before1 <- vector()
week_before2 <- vector()
week_before3 <- vector()
week_before4 <- vector()
cutoff <- 50

for (i in 1:nrow(hr.115)){
  
  # Saving querry
  query <- hr.115[i,]$short_title
  
  if(nchar(query) > 60){
    spaces <- gregexpr(pattern =' ',query)[[1]]
    spaces <- spaces[1:length(spaces)]
    entry <- which(abs(spaces-cutoff)==min(abs(spaces-cutoff)))
  
    query <- substr(query, 1, spaces[entry])
  }
  
  g_dat <- gtrends(query, geo = "US")
  
  if (is.timepoint(g_dat$interest_over_time$date)){
    intro <- hr.115[i,]$introhousedate
    week_before1.i <- interval(intro - weeks(1), intro)
    week_before2.i <- interval(intro - weeks(2), intro - weeks(1))
    week_before3.i <- interval(intro - weeks(3), intro - weeks(2))
    week_before4.i <- interval(intro - weeks(4), intro - weeks(3))
    
    week_before1.l <- g_dat$interest_over_time$date %within% week_before1.i
    week_before2.l <- g_dat$interest_over_time$date %within% week_before2.i
    week_before3.l <- g_dat$interest_over_time$date %within% week_before3.i
    week_before4.l <- g_dat$interest_over_time$date %within% week_before4.i
    
    week_before1[i] <- g_dat$interest_over_time$hits[week_before1.l]
    week_before2[i] <- g_dat$interest_over_time$hits[week_before2.l]
    week_before3[i] <- g_dat$interest_over_time$hits[week_before3.l]
    week_before4[i] <- g_dat$interest_over_time$hits[week_before4.l]
    
  }
  print(i)
}


