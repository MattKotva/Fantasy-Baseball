# statcast.R downloads and saves all statcast data from the last 8 years and saves it
# to sqlite db
# This takes a while

library(baseballr)
library(RSQLite)
library(DBI)

con <- dbConnect(drv =RSQLite::SQLite(), dbname="./draftkings/statcast.db")

# Set opening day and last day of regular season
opening_days <- as.Date(c("2010-04-04", "2011-03-31", "2012-03-28", "2013-03-31",
                          "2014-03-22", "2015-04-03","2016-04-03", "2017-04-02", "2018-03-28"))
last_regs <- as.Date(c("2010-10-03", "2011-09-28", "2012-10-03", "2013-09-30",
                       "2014-09-28", "2015-10-04", "2016-10-02", "2017-10-01", "2018-04-20"))


scrape_days <- cbind.data.frame(opening_days, last_regs)

apply(scrape_days, 1, function(x) {
  all_scrape <- vector(mode = "list")
  i <- 1
  this_day <- as.Date(x[1])
  last_reg <- as.Date(x[2])
  while(this_day <= last_reg) {
    tryCatch({
      day_scrape <- baseballr::scrape_statcast_savant_batter_all(as.character(this_day), as.character(this_day))
      all_scrape[[i]] <- day_scrape
    }, error = function(err) {
      print(paste("No games on ", this_day ))
    })
    this_day <- this_day + 1
    i <- i+1
  }
  
  statcast_df <- do.call(rbind, all_scrape)
  statcast_df$game_date <- as.character(statcast_df$game_date)
  dbWriteTable(conn = con, "statcast", statcast_df, append = T)
  
})

dbDisconnect(con)