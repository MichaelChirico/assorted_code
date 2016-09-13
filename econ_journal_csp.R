library(rvest)
library(data.table)

#Top 10 journals by Impact Factor
URLs <- 
  #This could be more automated -- we could scrape this page:
  #  http://citec.repec.org/search.html#journals
  #  and extract URLs from the table with xpath:
  #  //*[@id="sortable_example"]
  #  under the Series column. Simple here for starters.
  c(Econometrica = "http://citec.repec.org/s/2015/wlyemetrp.html",
    QJE = "http://citec.repec.org/s/2015/oupqjecon.html",
    JEL = "http://citec.repec.org/s/2015/aeajeclit.html",
    REStud = "http://citec.repec.org/s/2015/ouprestud.html",
    AEJM = "http://citec.repec.org/s/2015/aeaaejmac.html",
    JEP = "http://citec.repec.org/s/2015/aeajecper.html",
    JoF = "http://citec.repec.org/s/2015/blajfinan.html",
    JPE = "http://citec.repec.org/s/2015/ucpjpolec.html",
    AEJAE = "http://citec.repec.org/s/2015/aeaaejapp.html",
    JEEA = "http://citec.repec.org/s/2015/blajeurec.html")

journals = rbindlist(lapply(URLs, function(uu)
  #Extract Table
  read_html(uu) %>% html_node(xpath = '//*[@id="sortable_example"]') %>%
  #Convert to Table, concatenate across pages into single data set
  html_table() %>% setDT()), idcol = "journal")

#inconveniently named column
setnames(journals, "#", "cite_rank")

#For consistently-identified colors
cols = c("red", "blue", "black", "darkgreen",
         "orange", "skyblue", "orchid", "yellow",
         "green", "firebrick")
names(cols) <- names(URLs)

png("~/Desktop/journal_csp.png")
#Initialize Plot
plot(NULL, xlim = c(1, 50), ylim = c(0, journals[ , max(Cited)]),
     main = "Citation Signature Plot of Top Journals",
     xlab = "Order Statistic of Paper Citations",
     ylab = "# Citations")

#Note: there's a bug in RStudio, so this plot will not
#  render correctly on the RStudio Graphics Device.
#  See https://github.com/Rdatatable/data.table/issues/1524 and 
#  https://support.rstudio.com/hc/en-us/community/posts/208778048
journals[order(cite_rank), 
         lines(cite_rank, Cited, lwd = 3, 
               col = cols[.BY$journal]),
         by = journal]
journals[ , legend("topright", legend = unique(journal),
                   col = cols[unique(journal)], lwd = 3)]
dev.off()
