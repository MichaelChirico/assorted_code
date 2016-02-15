library(rvest)
library(data.table)

wiki <- "https://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States"

#table XPath: //*[@id="mw-content-text"]/table[2]

justices <- html(wiki) %>% html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
  html_table() %>% `[[`(1) %>% setDT()

setnames(justices, c("no","name","state","born_died",
                     "active","chief","retired",
                     "appointer","reason_left"))

#birth year & death year extract
justices[ , c("born", "died") := 
            .(as.integer(substr(born_died, 1, 4)),
              as.integer(gsub(".*–", "", born_died)))]

#approximate (discretized) age at death
justices[ , c("age_at_death", "alive") := 
            .(died - born, is.na(died))]
justices[(alive), age_at_death := 2016L - born]

#active years extract
justices[ , c("term_begin", "term_end") :=
            .(as.integer(substr(active, 1, 4)),
              as.integer(gsub("\\(.*", "", 
                              sub("[^–]*–", "", active))))]

#approximate (discretized) length of term
justices[ , term_length := term_end - term_begin]

#cleanup footnotes; add corresponding color map
justices[ , reason_left := gsub("\\[.*", "", reason_left)]
cols <- c("Currently serving" = "red",
          "Death" = "black", "Rejection" = "yellow",
          "Resignation" = "blue", "Retirement" = "darkgreen")

#regression:

justices[(!alive), summary(reg <<- lm(age_at_death ~ born))]

birth_range <- data.table(born = justices[ , {x<-range(born); x[1]:x[2]}])

#Bootstrapped CIs
BB <- 5000 #replication count
## formatting magic
CIs <- as.data.table(t(apply(replicate(
  BB, justices[(!alive)
               ##resample with replacement
               ][sample(.N, rep = TRUE),
                 ##re-run regression and predict
                 predict(lm(age_at_death ~ born),
                         birth_range)]),
  1, quantile, c(.025, .05, .95, .975))))[ , born := birth_range$born]

#plot:
pdf("~/Desktop/scotus.pdf")
justices[ , plot(born, age_at_death, col = cols[reason_left],
                 main = paste0("SCOTUS Justice Life Expectancy\n",
                               "With Bootstrapped CIs"),
                 xlab = "Year of Birth", pch = 19,
                 ylab = "Approximate Age at Death")]
CIs[ , matplot(born, cbind(`2.5%`, `5%`, `95%`, `97.5%`), add = TRUE,
               lty = 2, lwd = c(1,2,2,1), type = "l", col = "orange")]
justices[ , abline(reg, col = "orange", lwd = 3)]
legend("bottomright", horiz = TRUE, pch = 19, cex = .5,
       col = cols, legend = names(cols),x.intersp=.5)
dev.off()
