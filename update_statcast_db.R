library(baseballr)
library(RSQLite)
library(DBI)

con <- dbConnect(drv =RSQLite::SQLite(), dbname="./draftkings/statcast.db")

statcast <- dbGetQuery(con, "Select * from statcast")

all_scrape <- vector(mode = "list")
i <- 1
most_recent_db <- as.Date(max(statcast$game_date))
today <- Sys.Date()
while(most_recent_db <= today) {
  tryCatch({
    day_scrape <- baseballr::scrape_statcast_savant_batter_all(as.character(most_recent_db), as.character(most_recent_db))
    all_scrape[[i]] <- day_scrape
  }, error = function(err) {
    print(paste("No games on ", most_recent_db ))
  })
  most_recent_db <- most_recent_db + 1
  i <- i+1
}

statcast_df <- do.call(rbind, all_scrape)
statcast_df$game_date <- as.character(statcast_df$game_date)
dbWriteTable(conn = con, "statcast", statcast_df, append = T)
