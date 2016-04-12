library(rvest) #devtools::install_github("hadley/rvest")
library(data.table) #devtools::install_github("Rdatatable/data.table")
library(stringi) #from CRAN

index_url <- "https://web.njit.edu/~bukiet/baseball/baseball.html"

all_links <- 
  read_html(index_url) %>% 
  html_nodes(xpath="//a[contains(text(), 'ennant')]") %>%
  html_attr("href")

all_seasons <- rbindlist(lapply(
  all_links, function(u){
    yr <- as.integer(gsub("[^0-9]", "", u))
    if (yr < 2000) yr <- yr + 2000
    if (yr %in% 2015:2016){
      tbl <- read_html(u) %>% html_node("table") %>% 
        html_table() %>% as.matrix()
      ul_coords <- 
        transpose(expand.grid(c(4, 13), c(1, 5, 9)))
      return(setnames(rbindlist(lapply(
        ul_coords, function(ij)
          as.data.table(tbl[ij[1]:(ij[1]+4),
                            ij[2]:(ij[2]+2)])),
        use.names = FALSE), c("team", "w", "l"))[ , year := yr])
    } 
    if (yr %in% 2013:2014){
      tbl <- read_html(u) %>% html_nodes("table") %>% 
        `[[`(2L) %>% html_table() %>% as.matrix()
      return(setnames(as.data.table(tbl[
        -c(1:4, which(tbl[,2] == intToUtf8(160))),
        -c(1, 5, 6)]), c("team","w","l"))[ , year := yr])
    }
    if (yr == 2012){
      tbl <- read_html(u) %>% html_node("table") %>% 
        html_table() %>% as.matrix()
      corners <-
        transpose(data.table(ul = rep(c(3, 12), each = 3),
                             ur = rep(c(1, 6, 11), 2),
                             bl = c(7, 8, 7, 16, 16, 15),
                             lr = rep(c(3, 8, 13), 2)))
      return(setnames(rbindlist(lapply(
        corners, function(ij)
          as.data.table(tbl[ij[1]:ij[3], ij[2]:ij[4]])),
        use.names = FALSE), c("team", "w", "l"))[ , year := yr])
    }
    if (yr %in% 2010:2011){
      tbl <- read_html(u) %>% html_node("table") %>% 
        html_table() %>% as.matrix()
      corners <-
        transpose(data.table(ul = rep(c(5, 14), each = 3),
                             ur = rep(c(1, 7, 13), 2),
                             bl = c(9, 10, 9, 18, 18, 17),
                             lr = rep(c(3, 9, 15), 2)))
      return(setnames(rbindlist(lapply(
        corners, function(ij)
          as.data.table(tbl[ij[1]:ij[3], ij[2]:ij[4]])),
        use.names = FALSE), c("team", "w", "l"))[ , year := yr])
    }
    txt <- read_html(u) %>% html_node("body") %>% html_text()
    txt <- capture.output(cat(txt))
    txt <- gsub("\t", "        ", txt)
    taken <- switch(
      as.character(yr),
      "2009" = c(7:12, 14:18), "2008" = c(7:12, 14:18),
      "2007" = c(7:12, 15:19), "2006" = c(7:12, 15:19),
      "2005" = c(8:13, 16:20), "2004" = c(6:11, 14:18),
      "2003" = c(7:11, 15:20, 24:28), "2002" = c(10:14, 18:23, 27:31),
      "2001" = c(10:14, 18:23, 27:31), "2000" = c(10:14, 18:23, 27:31))
    txt <- txt[taken]
    supp <- max(NN <- nchar(txt)) - NN
    txt <- paste0(txt, sapply(supp, function(n) 
      paste(rep(" ", n), collapse = "")))
    begs <- 
      switch(which(sapply(
        list(2009, 2008, 2006:2007, 2004:2005, 2000:2003),
        function(ys) yr %in% ys)), 
        c(1,13,15,16,18,30,43,45,46,49,59,73,75,76,78),
        c(1,12,18,30,41,43,44,46,57,71,73,74,76),
        c(1,12,19,24,35,37,38,41,46,60,62,63,65),
        c(1,12,19,24,34,37,38,41,46,60,62,63,66),
        c(1,11,14,19,22,37,47,50,55,58,59))
    cols <- list(beg = begs[-length(begs)],
                 end = c(begs[-1L] - 1L))
    tbl <- 
      lapply(seq_len(length(cols$beg)), 
             function(i) stri_sub(txt, cols$beg[i],  cols$end[i]))
    if (yr %in% 2004:2008){
      x <- tstrsplit(tbl[[2]], split = "-")
      tbl <- c(tbl[1], x[1], list(NA), x[2], list(NA), tbl[4:12])
    }
    rbindlist(lapply(list(c(1, 2, 4),
                          c(6, 7, 9), 
                          if (yr %in% 2004:2009) 
                            c(11, 12, 14)),
                     function(jj) setDT(tbl[jj]))
    )[ , year := yr]}))

all_seasons[ , team := gsub("^[^a-z]*|[[:punct:]]|[^a-z]*$", "", tolower(team))]

all_seasons <- all_seasons[team != ""]
  
int <- c("w", "l", "year") 
all_seasons[ , (int) := lapply(.SD, as.integer), .SDcols = int]
all_seasons[fread("mlb_scrape_ids.csv"),
            team_id := team_id, 
            on = c(team="team_name")]

#***TYPO IN TABLE ON WEB PAGE AS OF 2016-04-12***#
#   2011 HOU & PIT predicted to play 152 games
#   2001 PHI predicted to play 163 games
#   2000 MIA predicted to play 161 games
all_seasons[w + l != 162, l := 162 - w]
