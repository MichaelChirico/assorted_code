# Tangible Currency Conversion
# Michael Chirico
# July 17, 2016

# packages
library(data.table)
library(rvest)
setwd("~/Documents/assorted_code/")

# Up-to-date Currency Conversion Table

## This list of currencies can easily be expanded.
##   Remember to add denominations and image URLs to
##   currency_denominations.csv as well.

exchange <- 
  data.table(symbol = c("USD", "JPY", "GBP", "CNY", "SGD", "HKD", "EUR"))

## Using USD as reference; scraping from Google Finance

ref <- "USD"
per <- paste0("per_", tolower(ref))
exchange[symbol == ref, (per) := 1]
css <- "#currency_value > div.sfe-break-bottom-4 > span.pr > span.bld"

for (cur in setdiff(unique(exchange$symbol), ref)){
  URL <- paste0("https://www.google.com/finance?q=USD", cur)
  x <- read_html(URL) %>% html_node(css) %>% html_text()
  exchange[symbol == cur, (per) := as.numeric(gsub(cur, "", x))]
}

denom <- fread("currency_denominations.csv")

#More intuitive to simply convert the denominations and then
#  take the log on the fly, but we can't do this until
#  `on` can support on-the-fly column creation,
#  GH#1639: https://github.com/Rdatatable/data.table/issues/1639

inn <- paste0("l_in_", tolower(ref))
denom[exchange, (inn) := 
        log2(x.denomination / get(paste0("i.", per))), on = "symbol"]

denom[ , match := 
         denom[symbol == ref][.SD, x.denomination, 
                              on = "l_in_usd", roll = "nearest"],
       by = symbol]
