library(baseballr)

# Set opening day and last day of regular season
opening_day <- as.Date("2016-04-03")
last_reg <- as.Date("2016-10-02")

all_scrape <- vector(mode = "list")
i <- 1
this_day <- opening_day
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


