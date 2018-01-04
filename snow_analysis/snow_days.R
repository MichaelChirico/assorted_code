library(readxl)
library(data.table)
rm(list = ls(all = TRUE))
setwd("~/Desktop/")

#via Franklin Institute
#  https://www.fi.edu/historical-weather-data-philadelphia
sh.n <- excel_sheets("philly_weather.xls")[1:3]

all_data <- setnames(rbindlist(lapply(lapply(
  sh.n, read_excel, path = "philly_weather.xls", na = c("NA", "N/A")), setDT)),
  c("month","day","year","temp_high","temp_low","precip","snow"))

#1898 seems to have been duplicated for some reason; remove
all_data <- all_data[ , .SD[1L], by = .(year, month, day)]

#First 12 years or so are almost or entirely missing;
#  keep the 13th year (1884) because, though its first
#  months are missing, its winter is not.
all_data <- all_data[ , if(sum(is.na(snow)) < 350) .SD, by = year]

#Reset the few remaining NA values to 0 for simplicity
all_data[is.na(snow) , snow := 0]
#Not sure what negative numbers mean for snow... just set them to 0.
all_data[snow < 0, snow := 0]

#Re-define the year since winter straddles years
#  take anything after September and add it to the following year
all_data[ , year_w := year + (month >= 9)]

#Delete first and last years since they're incomplete
##holdover function until data.table#1489 is implemented:
##  https://github.com/Rdatatable/data.table/issues/1489
"%bwx%" <- function(x, y) between(x, y[1L], y[2L], incbounds = FALSE)
all_data <- all_data[year_w %bwx% range(year_w)]

#Also reset the ordering of month so it starts in september
all_data[ , month_w := factor(month, c(9:12, 1:8))]

almanac <- 
  all_data[order(month_w) , 
           {snowed <- snow > 0
           .(total = sum(snow), 
             first = day[which.max(snowed)],
             days = sum(snowed),
             big = max(snow))}, by = year_w]

#Farmer's Tale:
#  The day of the month of the first snow is 
#  equal to the number of snows to expect that winter
reg <- almanac[ , lm(days ~ first)]
png("freq.png")
par(bg = "cadetblue1")
almanac[ , {FF <- max(first)
  plot(first, days, pch = 8, col = "white", 
       main = paste0("Relationship between Day of First Snow\n",
                     "and Total Snow Days Each Winter\n(Philadelphia)"),
       xlab = "Day of First Snow", ylab = "Number of Snow Days")
  lines(1:FF, 1:FF, lty = 2)
  abline(reg, col = "darkgreen", lwd = 2)
  legend("topleft", legend = c("45˚ line", "OLS Fit"), 
         col = c("black", "darkgreen"), lty = c(2, 1), bg = "white")
  text(FF - 3, par("usr")[3L] + 5, 
       paste0("R^2 = ", round(summary(reg)$r.squared, 2)))}]
dev.off()
summary(reg)

#What about day of the month predicting 
#  how _much_ it will snow? (quantity, not frequency)
reg <- almanac[ , lm(I(total / 10) ~ first)]
png("quant.png")
par(bg = "cadetblue1")
almanac[ , {FF <- max(first)
  plot(first, total / 10, pch = 8, col = "white", 
       main = paste0("Relationship between Day of First Snow\n",
                     "and Total Snow Each Winter\n(Philadelphia)"),
       xlab = "Day of First Snow", ylab = "Total Snow (in.)")
  abline(reg, col = "darkgreen", lwd = 2)
  legend("topleft", legend = "OLS Fit", col = "darkgreen", lty = 1, bg = "white")
  text(FF - 3, par("usr")[4L] - 5, 
       paste0("R^2 = ", round(summary(reg)$r.squared, 2)))}]
dev.off()
almanac[ , summary(reg)]

#What about day of the month predicting 
#  how much it will snow _on the snowiest day_?
reg <- almanac[ , lm(big ~ first)]
png("max.png")
par(bg = "cadetblue1")
almanac[ , {FF <- max(first)
plot(first, big, pch = 8, col = "white", 
     main = paste0("Relationship between Day of First Snow\n",
                   "and Biggest Snow Each Winter\n(Philadelphia)"),
     xlab = "Day of First Snow", ylab = "Largest Daily Fall (1/10 in.)")
abline(reg, col = "darkgreen", lwd = 2)
legend("topleft", legend = "OLS Fit", col = "darkgreen", lty = 1, bg = "white")
text(FF - 3, par("usr")[4L] - 50, 
     paste0("R^2 = ", round(summary(reg)$r.squared, 2)))}]
dev.off()
almanac[ , summary(reg)]

reg <- almanac[ , lm(I(total/days) ~ first)]
png("avg.png")
par(bg = "cadetblue1")
almanac[ , {FF <- max(first)
plot(first, total/days, pch = 8, col = "white", 
     main = paste0("Relationship between Day of First Snow\n",
                   "and Average Snowfall Each Winter\n(Philadelphia)"),
     xlab = "Day of First Snow", ylab = "Snow Per Snow Day (1/10 in.)")
abline(reg, col = "darkgreen", lwd = 2)
legend("topleft", legend = "OLS Fit", col = "darkgreen", lty = 1, bg = "white")
text(FF - 3, par("usr")[4L] - 5, 
     paste0("R^2 = ", round(summary(reg)$r.squared, 2)))}]
dev.off()
almanac[ , summary(reg)]