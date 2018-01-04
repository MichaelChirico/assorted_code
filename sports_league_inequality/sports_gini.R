#Competitiveness of the major sports
rm(list=ls(all=T))
gc()

library(data.table)
library(rvest)
library(ineq)

URLs <-
  c(NHL="http://www.hockeydb.com/ihdb/stats/leagues/141.html",
    NBA="http://www.basketball-reference.com/leagues/",
    MLB="http://www.shrpsports.com/mlb/stand/",
    NFL="http://www.nfl.com/history/standings/1920")

#Sports' Gini Coefficients ####

##NHL

### Page with list of all season stats
links <- read_html(URLs["NHL"]) %>%
  html_nodes(xpath='//*[@id="thisone"]') %>%
  html_nodes("a") %>% html_attr("href")

links <- 
  paste0("http://www.hockeydb.com",
         grep("nhl1927", links, value = TRUE))

### Peculiarities of NHL
###  - For 1917-18 through 1998-99, 3 game statuses:
###    Win; Loss; Tie
###  - For 1999-2000 through 2003-04, 4 game statuses:
###    Win; Loss; Tie (T/1 pt); OTL
###  - For 2005-06 through present, 4 game statuses:
###    Win (W/2 pts); Loss (L/0 pts); 
###    Overtime Loss (OTL/1 pt); Shootout Loss (SOL/1 pt)

NN <- length(links)
NHL <- data.table(season = integer(NN),
                  gini = numeric(NN))

for (ii in seq_along(links)){
  URL <- links[ii]
  pg <- read_html(URL)
  yr <- as.integer(gsub(".*([0-9]{4})\\.html", "\\1", URL))
  g <- Gini(sapply(
    pg %>% html_node("tbody") %>% html_nodes("tr"),
    function(rr) {
      #Delete rows between conferences - 
      #  characterized by use of th tag
      if (length(html_nodes(rr, "th"))) return(NA)
      x <- suppressWarnings(
        as.integer(strsplit(html_text(rr),
                            split = "\n")[[1]]))
      #6/7: points; 2: games played
      x[6L + (yr > 1999)]/x[2]/2}))
  NHL[ii, c("season", "gini") := .(yr, g)]
}

setorder(NHL, season)

NHL[ , five_yr_ma := NHL[max(1L, .BY$I - 2L):
                           min(.N, .BY$I + 2L),
                         mean(gini)], 
     by = .(I = 1:nrow(NHL))]

##NBA

### Page with list of all season stats
links <- read_html(URLs["NBA"]) %>%
  html_nodes(xpath='//*[@id="div_"]/table') %>%
  html_nodes("a") %>% html_attr("href")

links <- 
  paste0("http://www.basketball-reference.com",
         unique(grep("leagues/NBA", links, value = TRUE)))

### Peculiarities of NBA
###  - For 1946-47 through 1948-49: no NBA, only BAA
###  - For 1967-68 through 1975-76: ABA coexisted with NBA
### *ignoring both BAA and ABA stats for this exercise*

NN <- length(links)
NBA <- data.table(season = integer(NN),
                  gini = numeric(NN))

table_css <- 
  c("#_standings","#E_standings","#W_standings")

for (ii in seq_along(links)){
  URL <- links[ii]
  pg <- read_html(URL)
  yr <- as.integer(gsub(".*([0-9]{4})\\.html", "\\1", URL))
  g <- Gini(unlist(lapply(
    table_css,
    function(div){
      tbl <- pg %>% html_node(css = div)
      if (is.null(tbl)) return(NULL)
      suppressWarnings(
        tbl %>% html_table() %>% `$`(`W/L%`) %>% as.numeric()
      )})))
  NBA[ii, c("season", "gini") := .(yr, g)]
}

setorder(NBA, season)

NBA[ , five_yr_ma := NBA[max(1L, .BY$I - 2L):
                           min(.N, .BY$I + 2L),
                         mean(gini)], 
     by = .(I = 1:nrow(NBA))]

##MLB
links <- read_html(URLs["MLB"]) %>% html_nodes("a") %>% html_attr("href") %>%
  grep(pattern = "[0-9]{4}(finalcnf)*\\.htm", value = TRUE)

links <- links[!duplicated(gsub("([0-9]{4})\\.htm",
                                "\\1finalcnf.htm", links),
                           fromLast = TRUE)]

links <- paste0(URLs["MLB"], links)

### Peculiarities of MLB
###  - Some pages are formatted differently:
###    1913, 1915, 1922
odd_yrs <- c(1913, 1915, 1922)

NN <- length(links)
MLB <- data.table(season = integer(NN),
                  gini = numeric(NN))

for (ii in seq_along(links)){
  URL <- links[ii]
  pg <- read_html(URL)
  yr <- as.integer(gsub(".*([0-9]{4}).*", "\\1", URL))
  rcrds <- pg %>% html_nodes("table") %>% 
    `[`(if (yr %in% odd_yrs) 3:4 else 5:6) %>% 
    lapply(function(x) html_table(x, fill = TRUE)$X2) %>%
    unlist()
  rcrds <- rcrds[!is.na(rcrds)]
  g <- Gini(sapply(strsplit(rcrds, split = "-"),
         function(x){
           WL <- as.integer(x)
           WL[1]/sum(WL)}))
  MLB[ii, c("season", "gini") := .(yr, g)]
}

setorder(MLB, season)

MLB[ , five_yr_ma := MLB[max(1L, .BY$I - 2L):
                           min(.N, .BY$I + 2L),
                         mean(gini)], 
     by = .(I = 1:nrow(MLB))]

##NFL
links <- read_html(URLs["NFL"]) %>% html_nodes("option") %>% 
  html_attr("value") %>% unique() %>% as.integer()

links <- paste0("http://www.nfl.com/history/standings/",
                links)

### Peculiarities of NFL
###  - NFL site defines win percentage in a column
###    as wins/(wins+losses) (even in the presence of ties);
###    I'll calculate like in hockey instead:
###    2 pts/win, 1/tie, win% = % of possible points received
###  - AFL coexisted with NFL from to 1960 - 1969;
###    ignoring these results
###  - Currently (April 4, 2016), the HTML tag on the
###    tables in 1972-4, 1977 were incorrectly set to 
###    history_1971; this has been reported to site support, so
###    hopefully this inconsistency is soon remedied.
###    Recurs in 2006 (as history_2005).
###  - The NFL site only runs through 2006. Using
###    pro-football-reference thereafter

odd_yrs <- c(1972:1974, 1977)

NN <- length(links)
NFL <- data.table(season = integer(NN),
                  gini = numeric(NN))

for (ii in seq_along(links)){
  URL <- links[ii]
  pg <- read_html(URL)
  yr <- as.integer(gsub(".*([0-9]{4}).*", "\\1", URL))
  tbl_css <- paste0("#history_", 
                    if (yr %in% odd_yrs) 1971 else 
                      if (yr == 2006) 2005 else yr)
  tbl <- pg %>% html_nodes(css=tbl_css) %>% 
    html_table(fill = TRUE) %>% `[[`(1L) %>% setDT
  if (length(afl <- grep("[0-9]{4} AFL", tbl[[1]]))){
    tbl <- tbl[1:(afl - 1L)]
  }
  if (!"W" %in% names(tbl)){
    wlt <- c("W","L","T")
    invisible(
      sapply(wlt,
           function(x)
             setnames(tbl, which(unlist(transpose(
               tbl[1:2])) == x) %% ncol(tbl), x))
    )
    suppressWarnings(
      tbl[ , (wlt) := lapply(.SD, as.integer), .SDcols = wlt]
    )
  }
  g <- tbl[ , Gini(.5*(2*W + T)/(W + L + T))]
  NFL[ii, c("season", "gini") := .(yr, g)]
}

#### Supplementing the NFL.com site data

links <- paste0("http://www.pro-football-reference.com/years/",
                2007:2015)

NN <- length(links)
NFL2 <- data.table(season = integer(NN),
                   gini = numeric(NN))

tbl_css <- c("#AFC", "#NFC")

for (ii in seq_along(links)){
  URL <- links[ii]
  pg <- read_html(URL)
  yr <- as.integer(gsub(".*([0-9]{4}).*", "\\1", URL))
  g <- Gini(sapply(
    tbl_css,
    function(div){
      tbl <- pg %>% html_node(css = div) %>% 
        html_table() %>% setDT %>% 
        `[`(i = !is.na(W), j = .5*(2*W + T)/(W+L+T))}))
  NFL2[ii, c("season", "gini") := .(yr, g)]
}

NFL <- rbind(NFL, NFL2)

setorder(NFL, season)

NFL[ , five_yr_ma := NFL[max(1L, .BY$I - 2L):
                           min(.N, .BY$I + 2L),
                         mean(gini)], 
     by = .(I = 1:nrow(NFL))]

## Plotting
sports <- names(URLs)

all_sports <- 
  rbindlist(lapply(setNames(nm=sports), get), 
            idcol="sport")

cols <- c(NHL="black", NBA="red", MLB="blue", NFL="darkgreen")
ltys <- c(NHL=1, NBA=5, MLB=2, NFL=3)

png("~/Desktop/sports_gini.png")
all_sports[ , plot(
  NA, ylim = range(gini), xlim = range(season), 
  xlab = "Season", ylab = "Gini Coefficient", las = 1,
  main = "Competitiveness in Major US Sports Leagues")]

all_sports[ , {
  idx <- .BY$sport
  points(season, gini, col = cols[idx])
  lines(season, five_yr_ma, col = cols[idx], 
        lwd = 3, lty = ltys[idx])},
  by = sport]
legend("topright", legend = names(cols), col = cols,
       lwd = 3, lty = ltys, pch = 1)
dev.off()

#Season-length Sensitivity ####
## Approach: Calculate winning percentages based on 
##   random subsets of the full baseball season, 
##   calculate the Gini on these subseasons as above,
##   repeat, and average to demonstrate any possible
##   bias introduced by season length

download.file("http://www.retrosheet.org/gamelogs/gl2015.zip", 
              "season2015.zip")

season_2015 <- 
  fread("unzip -cq season2015.zip", header = FALSE,
        select = paste0("V", c(1, 4, 7, 10:11)))
setnames(season_2015, 
         c("date", "away_n", "home_n", "away_s", "home_s"))
season_2015[ , game_id := 1:.N]

season_2015 <-
  melt(season_2015, idvar = c("date", "game_id"),
       measure.vars=list(c("away_n","home_n"),
                         c("away_s","home_s")),
       value.name = c("name", "score")
       )[order(game_id, variable)
         ][, variable := c("away", "home")[variable]]

nn <- 240
setkey(season_2015, game_id)
setindex(season_2015, name)
ids <- season_2015[ , unique(game_id)]

set.seed(1029)
ginis <- sapply(s_length <- 10:161 * (15), function(nn)
  mean(replicate(
    1000,
    season_2015[.(idx <- sample(ids, nn)),
                .(winner=name[which.max(score)]),
                .(date,game_id)
                ][ , .(wins=.N), winner
                   ][season_2015[.(idx), .N, name],
                     Gini(wins/i.N), on = c(winner = "name")])))
  
png("~/Desktop/season_length_gini.png")
plot(s_length, ginis, type = "l", lwd = 3,
     main = "How Season Length Affects Gini",
     xlab = "Season Length", ylab = "Gini")
abline(v = ol <- c(16 * 30/2, 82 * 30/2),
       lty = 2, lwd = 2, col = "red")
text(ol, ginis[s_length %in% ol],
     c("NFL", "NBA/NHL"), pos = 4, col = "red")
dev.off()

#Turnover in the Best and Worst ####
##NHL

### getting full list of teams
all_links <- 
  unlist(lapply(links, function(u) read_html(u) %>%
                  html_node("tbody") %>% 
                  html_nodes("a") %>% html_attr("href")))

all_links <- grep("teams/", all_links, value = TRUE)

all_links <- 
  paste0("http://www.hockeydb.com/",
         gsub("^teams", "/ihdb/stats/leagues/seasons/teams", 
              all_links))

name_xpath = "/html/body/div[5]/div/div[1]/div[1]/a[4]"

all_teams <- character(length(all_links))

for (ii in 1:length(all_links)){
  all_teams[ii] <- 
    read_html(all_links[ii]) %>%
    html_node(xpath = name_xpath) %>% 
    html_text()
}