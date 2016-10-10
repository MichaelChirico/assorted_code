library(quantmod)
library(pdftools)
library(data.table)

#Records start January, 2000
mos = apply(expand.grid(2000:format(Sys.Date(), "%Y"),
                        sprintf("%02d", 1:12)), 1L, paste0, collapse = "-")

#Blessedly, all pdfs in a pattern
dt.urls = paste0("http://www.mmb.pa.gov/Pricing%20Information/",
                 "Wholesale%20or%20Retail/Documents/",
                 mos, "-Wholesale_Retail_Pricing.pdf")

names(dt.urls) = paste0(mos, "-01")

#Include only through current month
idx = grep(format(Sys.Date(), "%Y-%m"), dt.urls)
dt.urls = dt.urls[1L:idx]

#Record URLs with issues into this
problems = character(0L)

milk_data = 
  rbindlist(lapply(dt.urls, function(uu) {
    #Get pdf
    pdf1 = pdf_text(uu)
    #Get first page with data
    #  (some pdfs have an announcement page
    #   prior to the page with table;
    #   table page identified by WHOLESALE)
    pdf1 = pdf1[grep("WHOLESALE", pdf1)[1L]]
    
    #Delete extraneous initial info, split into lines,
    #  and cast to lowercase (since case is not constant
    #  across all pdfs)
    tbl = tolower(strsplit(gsub(".*WHOLESALE", "", pdf1), split = "\n")[[1L]])
    #Remove space so we can treat all spaces as tabs
    tbl = gsub("half gallon", "half_gallon", tbl)
    #Exclude more extraneous info
    tbl = tbl[grep("gallon", tbl)[1L]:(grep("issued on", tbl) - 1L)]
    tbl = gsub("\\s+", "\t", tbl)
    #2/3 of rows missed data in the first column (blank)
    tbl = gsub("\t(gallon|quart)", "\t\t\\1", tbl)
    #so fread can tell it's numeric
    tbl = gsub("$", "", tbl, fixed = TRUE)
    
    if (!all((nch <- nchar(gsub("[^\t]", "", tbl))) == 10L)) {
      #some pdfs mal-formed (seems to be a pattern -- 
      #  may be some way of fixing more programatticaly;
      #  for now, just skip such data pulls)
      message("Parsing Problem -- table trace:")
      cat(sprintf("%02d", nch), "\n", sep = " ")
      problems <<- c(problems, uu)
      return()
    }
    
    #Exclude first column since blank
    DT = fread(paste(tbl, collapse = "\n"), header = FALSE, drop = "V1")
    setnames(DT, c("area", "size",
                   paste0(rep(c("homo", "red_fat", "lo_fat", "no_fat"), 2L),
                          rep(c("_whole", "_retail"), each = 4L))))
    #Since missed area column on 2/3 of rows, add it back via merge
    DT[ , I := seq_len(.N), by = size]
    DT[DT[size == "half_gallon"], area := i.area, on = "I"][] }),
    idcol = "date")

getSymbols('CPIAUCNS', src = 'FRED')

cpi = as.data.table(CPIAUCNS)

milk_data[cpi, rel := homo_retail / i.CPIAUCNS,
          on = c(date = "index")]

milk_data[abs(rel) < .Machine$double.eps | 
            abs(rel) > .Machine$integer.max, rel := NA]

milk_data[ , rel := rel * cpi[index == as.Date("2016-08-01"), CPIAUCNS]]
