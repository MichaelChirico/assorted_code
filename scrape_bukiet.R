library(rvest) #devtools::install_github("hadley/rvest")
library(data.table) #devtools::install_github("Rdatatable/data.table")
library(stringi) #from CRAN

#here we find URLs to all the seasons posted programmatically
index_url <- "https://web.njit.edu/~bukiet/baseball/baseball.html"

all_links <- 
  read_html(index_url) %>% 
  #all are identified by having the word pennant
  #  (may or may not be capitalized)
  html_nodes(xpath="//a[contains(text(), 'ennant')]") %>%
  html_attr("href")

yrs <- as.integer(gsub("[^0-9]", "", all_links))
yrs[yrs<2000] <- yrs[yrs<2000] + 2000

all_seasons <- rbindlist(lapply(
  #name the URLs so we can use idcol in rbindlist
  setNames(all_links, yrs),
  function(u){
    #strip year from URL, adjusting for whether
    #  the full year or abbreviation is present
    yr <- as.integer(gsub("[^0-9]", "", u))
    if (yr < 2000) yr <- yr + 2000
    #Table format varies from year to year;
    #  4 branches here cover the 5 types
    if (yr %in% 2015:2016){
      #grab table, drop in matrix, extract
      #  6 cells for the 6 divisions
      tbl <- read_html(u) %>% html_node("table") %>% 
        html_table() %>% as.matrix()
      #each division has same number of teams, so
      #  each cell has same length/width -> can
      #  get the job done with only upper-left coordinate
      ul_coords <- 
        transpose(expand.grid(c(4, 13), c(1, 5, 9)))
      return(setnames(rbindlist(lapply(
        ul_coords, function(ij)
          as.data.table(tbl[ij[1]:(ij[1]+4),
                            ij[2]:(ij[2]+2)])),
        use.names = FALSE), c("team", "w", "l")))
    } 
    if (yr %in% 2013:2014){
      #an empty table on these pages, so extract
      #  the second (main) table (with `[[`(2L))
      tbl <- read_html(u) %>% html_nodes("table") %>% 
        `[[`(2L) %>% html_table() %>% as.matrix()
      #the six divisions are stacked vertically,
      #  and can be identified by whether or not the
      #  second column is a non-breaking space
      #  (&nbsp; = intToUtf8(160)); also exclud header rows
      return(setnames(as.data.table(tbl[
        -c(1:4, which(tbl[,2] == intToUtf8(160))),
        -c(1, 5, 6)]), c("team","w","l")))
    }
    if (yr %in% 2010:2012){
      tbl <- read_html(u) %>% html_node("table") %>% 
        html_table() %>% as.matrix()
      #in these years, different number of teams in some
      #  divisions -> need upper-left and lower-right
      #  coordinates to extract divisions; exact
      #  coordinates differ by year, using shft to handle this
      shft <- (yr %in% 2010:2011)
      corners <-
        transpose(data.table(
          ul = rep(c(3, 12) + 2*shft, each = 3),
          ur = rep(c(1, 6, 11) + (0:2)*shft, 2),
          bl = c(7, 8, 7, 16, 16, 15) + 2*shft,
          lr = rep(c(3, 8, 13) + (0:2)*shft, 2)))
      return(setnames(rbindlist(lapply(
        corners, function(ij)
          as.data.table(tbl[ij[1]:ij[3], ij[2]:ij[4]])),
        use.names = FALSE), c("team", "w", "l")))
    }
    #the rest of the years don't have the tables with
    #  a <table> tag; rather, they're printed as
    #  (more-or-less) fixed-width tables, so we'll use
    #  the stri_sub approach covered in this SO answer:
    #  http://stackoverflow.com/a/34190156/3576984
    txt <- read_html(u) %>% html_node("body") %>% html_text()
    txt <- capture.output(cat(txt))
    #  some errant \t characters appear to be equal to 8 spaces
    txt <- gsub("\t", "        ", txt)
    taken <- switch(
      as.character(yr),
      #depending on the year, we only need certain lines of
      #  the body of the page, those that contain team
      #  names and wins/losses/records; a bit too 
      #  nondeterministic to do this more concisely
      #  (without seriously undermining readability)
      "2009" = c(7:12, 14:18), "2008" = c(7:12, 14:18),
      "2007" = c(7:12, 15:19), "2006" = c(7:12, 15:19),
      "2005" = c(8:13, 16:20), "2004" = c(6:11, 14:18),
      "2003" = c(7:11, 15:20, 24:28), "2002" = c(10:14, 18:23, 27:31),
      "2001" = c(10:14, 18:23, 27:31), "2000" = c(10:14, 18:23, 27:31))
    txt <- txt[taken]
    supp <- max(NN <- nchar(txt)) - NN
    #supplement each line to force the same number of characters
    #  (some lines end early when there's more teams in one 
    #   division in one of the leagues, which would break code below)
    txt <- paste0(txt, sapply(supp, function(n) 
      paste(rep(" ", n), collapse = "")))
    begs <- 
      switch(which(sapply(
        list(2009, 2008, 2006:2007, 2004:2005, 2000:2003),
        function(ys) yr %in% ys)), 
        #according to the year, the starting column number
        #  of all of the fields we'll extract are given by
        #  this; note that the last number is used to
        #  define the last endpoint below
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
    #in some years, the columns are a bit ragged 
    #  (some teams have >100 wins, longer team names in
    #   the AL than in the NL, etc.), so the hyphens
    #  separating wins from losses are not perfectly
    #  aligned; instead, we get the whole win-loss
    #  combo as a single field, and the strsplit on "-"
    #  to separate into wins and losses, then finagle
    #  the output into a format in common with the other years
    if (yr %in% 2004:2008){
      x <- tstrsplit(tbl[[2]], split = "-")
      tbl <- c(tbl[1], x[1], list(NA), x[2], list(NA), tbl[4:12])
    }
    rbindlist(lapply(list(c(1, 2, 4),
                          c(6, 7, 9), 
                          if (yr %in% 2004:2009) 
                            c(11, 12, 14)),
                     function(jj) setDT(tbl[jj])))}),
  idcol = "year")

#especially in the fixed-width years, we got a lot of
#  extra white space (especially those pesky &nbsp;s);
#  also, easier to deal with team names stripped of
#  punctuation (looking at you, Cardinals and Athletics)
all_seasons[ , team := 
               gsub("^[^a-z]*|[[:punct:]]|[^a-z]*$", "", tolower(team))]

#exclude extraneous rows
all_seasons <- all_seasons[team != ""]
  
#put numeric rows into efficient format
int <- c("w", "l", "year") 
all_seasons[ , (int) := lapply(.SD, as.integer), .SDcols = int]
#use a self-defined lookup table to 
#  get a time-consistent ID for each team
all_seasons[fread("mlb_scrape_ids.csv"),
            team_id := team_id, 
            on = c(team="team_name")]

#clean-up
#***TYPO IN TABLE ON WEB PAGE AS OF 2016-04-12***#
#   2011 HOU & PIT predicted to play 152 games
#   2001 PHI predicted to play 163 games
#   2000 MIA predicted to play 161 games
all_seasons[w + l != 162L, l := 162L - w]
