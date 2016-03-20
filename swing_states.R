library(rvest)
library(data.table)
library(maptools)

URL <- "https://en.wikipedia.org/wiki/" %+% 
  "List_of_U.S._states_and_territories_by_population"

states <- html(URL) %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table() %>% `[[`(1) %>% setDT() %>%
  `[`(grepl("\\d$", `Rank in the fifty states, 2015`))

states[ , population := 
          as.integer(gsub(",","",`Population estimate, July 1, 2015`))]

us_pop <- states[ , sum(population)]

states[ , electoral_votes := 
          as.integer(
            gsub(".*♠",  "", 
                 `Total seats in House of Representatives, 2013–2023`)
          ) + 2L]

states[ , state := gsub("^[^A-Z]*([A-Z])", "\\1", `State or territory`)]

states[ , rank := .I]

n_winners <- 0L
it_count <- 0L
n_desired <- 1e6
winners <- vector("list", n_desired)

setkey(states, rank)

while (n_winners <= n_desired) {
  idx <- sample(50, sample(11:40, 1))
  states[.(idx), 
         {if (sum(electoral_votes) >= 270){
           n_winners <<- n_winners + 1L
           winners[[n_winners]] <<- 
             list(coalition = state,
                  pop_share = sum(population)/us_pop)}}]
  it_count = it_count + 1L
  if (n_winners %% 1000L == 0) cat("Winners:", n_winners, "\n",
                                   "Iterations:", it_count, "\n\n")
}

idx <- which.min(sapply(winners, function(x) x$pop_share))

usa <- readShapePoly("/media/data_drive/gis_data/USA/states.shp")

plot(usa,
     col = c("white","darkgreen")[as.character(usa@data$STATE_NAME) %in% 
                                    winners[idx][[1]]$coalition + 1L])

#As taken from 270towin.com
swing_states <- c("Nevada", "Colorado", "Iowa", "Wisconsin",
                  "Ohio", "Pennsylvania", "New Hampshire",
                  "Virginia", "North Carolina", "Florida")
states[ , sum(population[state %in% swing_states]) / sum(population)]
