library(rvest)
#a list of cities in the US
URL = 'https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population'
tbl = read_html(URL) %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>% 
  html_table
#extract 7-letter cities
seven_cty = with(tbl, tolower(City[nchar(City) == 7]))

#a simple dictionary turns out to suffice
all_wds = readLines('/usr/share/dict/american-english') %>% 
  tolower %>% gsub('[[:punct:]0-9 ]', '', .)
#too many matching this 'word'
all_wds = all_wds[all_wds != 'ur']

#flip to letter-wise with strsplit
check = sapply(strsplit(seven_cty, ''), function(x) {
  sapply(1:8, function(ii) {
    out = character(8)
    #insert 'ur' at all possible positions
    out[ii] = 'ur'
    out[-ii] = x
    #convert to one word with ur spliced in
    both = strsplit(paste(out, collapse = ''), '')[[1L]]
    sapply(2:9, function(jj) {
      out = character(10)
      #insert space at all possible positions
      #  (allow possibility of splitting middle of ur)
      out[jj] = ' '
      out[-jj] = both
      #convert to the two words
      wds = strsplit(paste(out, collapse = ''), ' ')[[1L]]
      #to speed up, check one-by-one whether the word
      #  is a real word (since probably 95% of the time
      #  the first check will fail)
      if (wds[1L] %in% all_wds) {
        if (wds[2L] %in% all_wds) {
          #return the two words if we succeed
          paste(wds, collapse = ' ')
        } else ''
      } else ''
    })
  })
}) 

#find all matches, examine for animal-related phrases
check[nzchar(check)]
