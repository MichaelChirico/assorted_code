library(rvest)
library(data.table)

wiki <- "https://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States"

#table XPath: //*[@id="mw-content-text"]/table[2]

justices <- html(wiki) %>% html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
  html_attr("style")
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
justices[ , c("term_length", "active") := 
            .(term_end - term_begin, is.na(term_end))]
justices[(active), term_length := 2016L - term_begin]
justices[ , age_retired := term_end - born]
justices[(active), age_retired := 2016L - born]

#approximate (discretized) age at term onset
justices[ , age_at_conf := term_begin - born]

#cleanup footnotes; add corresponding color map
justices[ , reason_left := gsub("\\[.*", "", reason_left)]
cols <- c("Currently serving" = "red",
          "Death" = "black", "Rejection" = "yellow",
          "Resignation" = "blue", "Retirement" = "darkgreen")

# LIFE EXPECTANCY ####
#regression:
justices[(!alive), summary(reg <<- lm(age_at_death ~ born))]

#Bootstrapped regression coefficient
BB <- 5000 #replication count
quantile(replicate(BB, justices[(!alive)
                         ][sample(.N, rep = TRUE),
                           lm(age_at_death ~ born)$coefficients["born"]]),
         c(.005, .025, .05, .95, .975, .995))

birth_range <- data.table(born = justices[ , {x<-range(born); x[1]:x[2]}])

#Bootstrapped CIs
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
png("~/Desktop/scotus_life_exp.png")
justices[ , plot(born, age_at_death, col = cols[reason_left],
                 main = paste0("SCOTUS Justice Life Expectancy\n",
                               "With Bootstrapped CIs"),
                 xlab = "Year of Birth", pch = 19,
                 ylab = "Approximate Age at Death")]
CIs[ , matplot(born, cbind(`2.5%`, `5%`, `95%`, `97.5%`), add = TRUE,
               lty = 2, lwd = c(1,2,2,1), type = "l", col = "orange")]
justices[ , abline(reg, col = "orange", lwd = 3)]
legend("bottomright", horiz = TRUE, pch = 19, cex = .6,
       col = cols, legend = names(cols),x.intersp=.5)
dev.off()

# TERM LENGTH ####
#regression:
justices[(!active), summary(reg <<- lm(term_length ~ born))]

#Bootstrapped regression coefficient
quantile(replicate(BB, justices[(!active)
                                ][sample(.N, rep = TRUE),
                                  lm(term_length ~ born)$coefficients["born"]]),
         c(.005, .025, .05, .95, .975, .995))

#Bootstrapped CIs
## formatting magic
CIs <- as.data.table(t(apply(replicate(
  BB, justices[(!active)
               ##resample with replacement
               ][sample(.N, rep = TRUE),
                 ##re-run regression and predict
                 predict(lm(term_length ~ born),
                         birth_range)]),
  1, quantile, c(.025, .05, .95, .975))))[ , born := birth_range$born]

#plot:
png("~/Desktop/scotus_term_length.png")
justices[ , plot(born, term_length, col = cols[reason_left],
                 main = paste0("SCOTUS Justice Term Length\n",
                               "With Bootstrapped CIs"),
                 xlab = "Year of Birth", pch = 19,
                 ylab = "Approximate Term Length")]
CIs[ , matplot(born, cbind(`2.5%`, `5%`, `95%`, `97.5%`), add = TRUE,
               lty = 2, lwd = c(1,2,2,1), type = "l", col = "orange")]
justices[ , abline(reg, col = "orange", lwd = 3)]
legend("bottomright", horiz = TRUE, pch = 19, cex = .6,
       col = cols, legend = names(cols),x.intersp=.5)
dev.off()

# AGE AT RETIREMENT ####
#regression:
justices[(!active), summary(reg <<- lm(age_retired ~ born))]

#Bootstrapped regression coefficient
quantile(replicate(BB, justices[(!active)
                                ][sample(.N, rep = TRUE),
                                  lm(age_retired ~ born)$coefficients["born"]]),
         c(.005, .025, .05, .95, .975, .995))

#Bootstrapped CIs
## formatting magic
CIs <- as.data.table(t(apply(replicate(
  BB, justices[(!active)
               ##resample with replacement
               ][sample(.N, rep = TRUE),
                 ##re-run regression and predict
                 predict(lm(age_retired ~ born),
                         birth_range)]),
  1, quantile, c(.025, .05, .95, .975))))[ , born := birth_range$born]

#plot:
png("~/Desktop/scotus_age_retired.png")
justices[ , plot(born, age_retired, col = cols[reason_left],
                 main = paste0("SCOTUS Justice Retirement Age\n",
                               "With Bootstrapped CIs"),
                 xlab = "Year of Birth", pch = 19,
                 ylab = "Approximate Age at Term End")]
CIs[ , matplot(born, cbind(`2.5%`, `5%`, `95%`, `97.5%`), add = TRUE,
               lty = 2, lwd = c(1,2,2,1), type = "l", col = "orange")]
justices[ , abline(reg, col = "orange", lwd = 3)]
legend("bottomright", horiz = TRUE, pch = 19, cex = .6,
       col = cols, legend = names(cols),x.intersp=.5)
dev.off()

# AGE AT TERM START ####
#regression:
justices[(!active), summary(reg <<- lm(age_at_conf ~ born))]

#Bootstrapped regression coefficient
quantile(replicate(BB, justices[(!active)
                                ][sample(.N, rep = TRUE),
                                  lm(age_at_conf ~ born)$coefficients["born"]]),
         c(.005, .025, .05, .95, .975, .995))

#Bootstrapped CIs
## formatting magic
CIs <- as.data.table(t(apply(replicate(
  BB, justices[(!active)
               ##resample with replacement
               ][sample(.N, rep = TRUE),
                 ##re-run regression and predict
                 predict(lm(age_at_conf ~ born),
                         birth_range)]),
  1, quantile, c(.025, .05, .95, .975))))[ , born := birth_range$born]

#plot:
png("~/Desktop/scotus_age_onset.png")
justices[ , plot(born, age_at_conf, col = cols[reason_left],
                 main = paste0("SCOTUS Justice Age at Confirmation\n",
                               "With Bootstrapped CIs"),
                 xlab = "Year of Birth", pch = 19,
                 ylab = "Approximate Age at Confirmation")]
CIs[ , matplot(born, cbind(`2.5%`, `5%`, `95%`, `97.5%`), add = TRUE,
               lty = 2, lwd = c(1,2,2,1), type = "l", col = "orange")]
justices[ , abline(reg, col = "orange", lwd = 3)]
legend("bottomright", horiz = TRUE, pch = 19, cex = .6,
       col = cols, legend = names(cols),x.intersp=.5)
dev.off()

# Grouping by quarters of history
justices[(!active), quarter := {
  x <- range(born); cut(born, breaks = ysq <- round(seq(x[1], x[2], length.out = 5)),
                        right = FALSE, include.lowest = TRUE,
                        labels = paste0(ysq[-5], "-", ysq[-1]))}]

png("~/Desktop/scotus_age_ret_periods.png")
justices[(!active), mean(age_retired), by = quarter
         ][ , barplot(V1, names.arg = quarter,
                      main = "SCOTUS Justice Age at Retirement Over Time",
                      ylab = "Average Approximate Retirement Age",
                      col = c("red", "blue", "orange", "darkgreen"),
                      space = 0)]
dev.off()