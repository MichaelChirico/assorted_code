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
#own exchange rate is 1
exchange[symbol == ref, (per) := 1]
css <- "#currency_value > div.sfe-break-bottom-4 > span.pr > span.bld"

for (cur in setdiff(unique(exchange$symbol), ref)){
  URL <- paste0("https://www.google.com/finance?q=", ref, cur)
  # get value of one unit of ref currency in target currency
  x <- read_html(URL) %>% html_node(css) %>% html_text()
  # output is formatted as "xxx.xxx cur" where cur is the symbol
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

denom[ , c("match", "match_image") := 
         #self-join to only reference currency,
         #  rolling to the nearest match (in log terms)
         denom[symbol == ref][.SD, .(x.denomination, x.image_url),
                              on = "l_in_usd", roll = "nearest"],
       by = symbol]

fwrite(denom, "currency_denominations_matched.csv")
