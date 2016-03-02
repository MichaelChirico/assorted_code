library(rvest)
library(data.table)

wiki <- "https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population"

states <- html(wiki) %>% html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
  html_table() %>% `[[`(1) %>% setDT()

setnames(states, c("rank", "state", "population",
                   "house_seats", "electoral_votes",
                   "pop_per_house", "pop_per_elec"))

states[ , population := as.numeric(gsub(",", "", population))]
states[ , state := gsub(intToUtf8(160), '', state)]

swing_states <- c("Nevada", "Colorado", "Iowa", "Wisconsin",
                  "Ohio", "Pennsylvania", "New Hampshire",
                  "Virginia", "North Carolina", "Florida")
states[ , sum(population[state %in% swing_states]) / sum(population)]
