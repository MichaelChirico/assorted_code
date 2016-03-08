#Reproducing http://i.imgur.com/p9H3Po7.gifv

library(Lahman)
library(data.table)
library(animation)

setwd("~/Desktop/")
data(Batting)
data(Master)
setDT(Batting, key = c("playerID", "yearID"))
setDT(Master, key = "playerID")

#add all stats within-year
Homers <- Batting[ , .(HR = sum(HR)), by = .(playerID, yearID)]

#cumulative homeruns
Homers[ , cum_HR := cumsum(HR), by = playerID]

#within-year rankings
Homers[, HR_rank := frank(-cum_HR, ties.method = "min"), by = yearID]

#assign fixed colors to players
## for reproducibility
set.seed(102938)
color_map <- 
  data.table(playerID = Homers[HR_rank %in% 1:10, unique(playerID)],
             key = "playerID")[ , col := sample(colors(), .N)]

#merge in player names
color_map[Master, player_name := 
            paste(i.nameFirst, i.nameLast), on = "playerID"]


Homers[color_map, player_color := i.col, on = "playerID"]

#keying for easy extraction
setkey(Homers, yearID, HR_rank)
#example year: 1920
yr0 <- 5 * (Homers[ , min(yearID)] %/% 5)
saveGIF(sapply(
  Homers[ , unique(yearID)],
  function(yr) 
    Homers[playerID %in% Homers[.(yr, 1:10), playerID] & yearID <= yr,
           {plot_block <- 
             dcast(.SD, yearID ~ playerID, value.var = "cum_HR")
           who <- names(plot_block)[-1]
           cols <- color_map[.(who), col]
           plot_block[, matplot(yearID, .SD[,-1,with=FALSE], ylab = "Home Runs",
                                xlab = "Year", las = 1, xaxt = "n",
                                main = paste0("Top Slugger Trajectories for ", yr),
                                type = "l", lty = 1, lwd = 3, col = cols)]
           axis(1, at = seq(yr0, yr, by = 5))
           legend("topleft", legend = color_map[.(who), player_name],
                  col = cols, lty = 1, lwd = 3)}]),
  "home_runs_over_time.gif")
