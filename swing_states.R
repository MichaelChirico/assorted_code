library(rvest)
library(data.table)

wiki <- "https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population"

states <- html(wiki) %>% html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
  html_table() %>% `[[`(1) %>% setDT()

states[ , Population := as.numeric(gsub(",", "", Population))]
states[ , State := gsub(intToUtf8(160), '', State)]

swing_states <- c("Nevada", "Colorado", "Iowa", "Wisconsin",
                  "Ohio", "Pennsylvania", "New Hampshire",
                  "Virginia", "North Carolina", "Florida")
states[ , sum(Population[State %in% swing_states]) / sum(Population)]
