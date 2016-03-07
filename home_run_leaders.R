#Reproducing http://i.imgur.com/p9H3Po7.gifv

library(Lahman)
library(data.table)

data(Batting)
data(Master)
setDT(Batting, key = c("playerID", "yearID"))
setDT(Master, key = "playerID")

#add all stats within-year
Homers <- Batting[ , .(HR = sum(HR)), by = .(playerID, yearID)]

#merge in player names
Homers[Master, player_name := paste(i.nameFirst, i.nameLast), on = "playerID"]

#cumulative homeruns
Homers[ , cum_HR := cumsum(HR), by = playerID]

#within-year rankings
Homers[, HR_rank := frank(-cum_HR, ties.method = "min"), by = yearID]

#assign fixed colors to players
## for reproducibility
set.seed(102938)
avail_col <- 
  sample(colors(), 
         length(top_ten <- Homers[HR_rank %in% 1:10, unique(playerID)]))
Homers[.(top_ten),
       player_color := avail_col[.GRP], by = playerID]

#keying for easy extraction
setkey(Homers, yearID, HR_rank)
#example year: 1920
yr <- 1920
Homers[playerID %in% Homers[.(yr, 1:10), playerID] & yearID <= yr]
      
        