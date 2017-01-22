library(rvest)
library(data.table)

trump.url = paste0('http://www.cnn.com/2017/01/20/politics/',
                   'trump-inaugural-address/index.html')
eisen.url = 'http://www.presidency.ucsb.edu/ws/?pid=10856'
list.url = 'http://millercenter.org/president/speeches'

list.page = read_html(list.url)

past.urls = list.page %>% 
  html_nodes(xpath = '//a[contains(text(), "Inaugural")]') %>%
  html_attr("href") %>% paste0("http://millercenter.org", .)

positive_words = 
  grep("^(;|$)", readLines("~/Documents/positive-words.txt"),
       invert = TRUE, value = TRUE)
negative_words = 
  grep("^(;|$)", readLines("~/Documents/negative-words.txt"),
       invert = TRUE, value = TRUE)

sentiments = 
  rbindlist(lapply(past.urls, function(uu) {
    page = read_html(uu)
    speech = page %>% 
      html_nodes(xpath = '//div[@id="transcript"]/p') %>% 
      html_text %>% gsub("[[:punct:]]", " ", .) %>% 
      paste(collapse = " ")
    words = strsplit(speech, split = "\\s+")[[1L]]
    date = page %>% 
      html_node(xpath = '//h1[@id="amprestitle"]') %>%
      html_text %>% gsub(".*\\((.*)\\).*", "\\1", .) %>% 
      as.Date(format = '%B %d, %Y')
    president = page %>% 
      html_node(xpath = '//h1[@id="amprestitle"]/following-sibling::h2') %>%
      html_text
    data.table(president, date, N_words = length(words),
               N_positive = sum(words %in% positive_words),
               N_negative = sum(words %in% negative_words),
               source = uu)
  }))

eisen.speech = read_html(eisen.url) %>%
  html_nodes(xpath = '//td/span/*') %>%
  html_text %>% paste(collapse = " ") %>%
  gsub("Note:.*", "", .) %>%
  gsub("[[:punct:]]", " ", .)

eisen.words = strsplit(eisen.speech, split = "\\s+")[[1L]]

trump.speech = read_html(trump.url) %>% 
  html_nodes(xpath = paste('//div[@class="zn-body__paragraph"',
                           'and not(descendant::a)]')) %>%
  html_text %>% paste(collapse = " ") %>%
  gsub("[[:punct:]]", " ", .)

trump.words = strsplit(trump.speech, split = "\\s+")[[1L]]

sentiments =
  rbind(sentiments,
        data.table(president = "Dwight D. Eisenhower",
                   date = as.Date("1957-01-21"),
                   N_words = length(eisen.words),
                   N_positive = sum(eisen.words %in% positive_words),
                   N_negative = sum(eisen.words %in% negative_words),
                   source = eisen.url),
        data.table(president = "Donald J. Trump",
                   date = as.Date("2017-01-20"),
                   N_words = length(trump.words),
                   N_positive = sum(trump.words %in% positive_words),
                   N_negative = sum(trump.words %in% negative_words),
                   source = trump.url))

setorder(sentiments, date)

sentiments[ , plot(date, N_words, col = rleid(president))]
sentiments[ , plot(date, N_positive - N_negative)]
                                                                 