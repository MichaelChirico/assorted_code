library(rvest)
library(data.table)
library(zoo)
library(pdftools)

stem = 'https://2009-2017.state.gov'
indexURL = paste0(stem, '/j/prm/releases/statistics/index.htm')

#get URLs for all the pages
URLs = read_html(indexURL) %>%
  html_nodes(xpath = '//a[starts-with(text(), "FY")]') %>%
  html_attr('href') %>% paste0(stem, .)

refugees = rbindlist(lapply(URLs, function(uu) {
  #some data stored in PDFs; most in .htm pages
  if (grepl("htm$", uu)) {
    pg = read_html(uu)
    #extract which year this is by finding this text
    #  (URL doesn't contain this information)
    yr = pg %>% html_text %>% gsub(".*Fiscal Year ([0-9]{4}).*", "\\1", .)
    tbl = pg %>% html_node('table') %>% 
      #exclude header rows
      html_table %>% setDT %>% `[`(-(1L:4L))
    #a varying number (0/1/2) of empty rows (sometimes read as NA,
    #  other times as "") are in some of the tables; remove them here
    tbl[ , (names(tbl)) := lapply(.SD, function(x) {
      if (all(is.na(x)) || !any(nchar(x))) NULL else x})]
    #for HTML tables, identify the region by finding this row
    region_row = tbl[ , X1 != ""]
    #now add this region-level data as a new column, and
    #  eliminate the region metadata rows
    tbl[X1 == "", X1 := NA_character_]
    tbl[ , region := na.locf(X1)]
    tbl[ , region_max := X3[1L], by = region]
    tbl = tbl[(!region_row & !grepl("^Total", X2))]
    tbl[ , c('X1', 'X3') := NULL]
    #table basically cleaned; now assign
    #  human-grokable names & convert numbers from strings
    setnames(tbl, grep('X', names(tbl)),
             c('country', 'total_admitted', 
               paste0('admitted_', tolower(month.name[c(10:12, 1:9)]))))
    num = setdiff(names(tbl), c('country', 'region'))
    tbl[ , (num) := lapply(.SD, function(x) as.numeric(gsub(',', '', x))),
         .SDcols = num]
    tbl[ , f_year := yr][]
  } else {
    pg_pdf = pdf_text(uu)
    #pdf_text returns one element per page; unlist here to 
    #  concatenate pages
    pg_lines = unlist(strsplit(pg_pdf, '\n'))
    yr = gsub('.*([0-9]{4}).*', '\\1', pg_lines[grep('Fiscal', pg_lines)])[1L]
    first_row = grep("^Africa", pg_lines)
    #eliminate header rows
    pg_lines = pg_lines[-seq_len(first_row - 1L)]
    #relevant lines all begin with some spaces & contain numbers
    #  (other rows are from multi-row wrapped text, e.g.)
    tbl_lines = pg_lines[grep("^\\s.*[0-9]", pg_lines)]
    #eliminate grand totals line
    tbl_lines = tbl_lines[!grepl("Grand|September", tbl_lines)]
    #messy issue -- want to strsplit the rows into columns
    #  identified by spaces. but of course some country names
    #  have spaces; initially tried splitting by 2+ spaces,
    #  but some rows have data separated by only one.
    #  instead, identify two-word country names with this pattern
    #  (some punctuation sprinkled in by trail-and-error)
    tbl_lines = gsub("(?<=[a-zA-Z.,])(\\s+)(?=[a-zA-Z(])",
                     "_", tbl_lines, perl = TRUE)
    tbl = setDT(tstrsplit(tbl_lines, "\\s+"))
    #regional metadata again
    tbl[grep("Total", V2), region := .I]
    tbl[ , region := na.locf(region, fromLast = TRUE)]
    tbl[ , region_max := V3[.N], by = region]
    tbl = tbl[!grepl("Total", V2)]
    tbl[ , c('V1', 'V16') := NULL]
    #now can copy-paste from above, essentially
    setnames(tbl, grep('^V', names(tbl)), 
             c('country', 'total_admitted', 
               paste0('admitted_', tolower(month.name[c(10:12, 1:9)]))))
    num = setdiff(names(tbl), c('country', 'region'))
    tbl[ , (num) := lapply(.SD, function(x) as.numeric(gsub(',', '', x))),
         .SDcols = num]
    tbl[ , f_year := yr][]
  }}))

refugees[ , f_year := as.integer(f_year)]

fwrite(refugees, 'us_refugee_admittances_fy07_15.csv')

iraq = 
  melt(refugees[grepl("Iraq", country)], value.name = "admitted",
       id.vars = "f_year", measure.vars = patterns("^admitted"))
iraq[ , month := match(gsub(".*_", "", variable), tolower(month.name))]
#Fiscal Year is October - September, so e.g. October of
#  FY'15 is actually October, 2014
iraq[month %in% 10:12, year := f_year - 1L]
iraq[is.na(year), year := f_year]
iraq[ , date := as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))]
png('refugee_time_series.png')
iraq[order(year, month), {
  #initialize empty plot so the background shading
  #  stays in the background
  plot(NULL, xlim = range(date), ylim = range(admitted),
       xaxt= "n", ylab = "# Admitted", las = 1L, xlab = "Date",
       main = "Refugees Admitted to US Each Month from Iraq")
  arrest = as.Date('2011-05-25')
  rect(arrest, 0, arrest+6*30, par('usr')[4L],
       col = 'red', density = 45)
  points(date, admitted, pch = 19L, col = 'slateblue')
  #function to calculate moving average 2 months
  #  in both directions
  avg = function(x) rowMeans(sapply(-2:2, function(nn) { 
    if (nn < 0) {type = 'lag'; nn = abs(nn)} 
    else type = 'lead'
    shift(x, nn, type = type)}), na.rm = TRUE)
  lines(date, avg(admitted), lwd = 3L)
  yrs = as.Date(paste0(2008:2016, "-01-01"))
  axis(side = 1L, at = yrs, labels = format(yrs, "%Y"))
  abline(h = 0)
}]
dev.off()

png('refugee_monthly.png')
dcast(iraq, month ~ year, value.var = 'admitted')[ , {
  vals = .SD[ , !'month']
  matplot(month, vals, type = 'l', lty = 1L,
          xlab = 'Month', ylab = "# Admitted",
          main = 'Refugees Admitted to US from Iraq, by Month')
  text(1, unlist(vals[1L]), names(vals), cex = .6)
}]
dev.off()
