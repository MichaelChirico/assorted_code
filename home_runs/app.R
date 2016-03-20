library(shiny)
library(Lahman)
#note: in order to get this to deploy to
#  shinyapps, I had to use the CRAN
#  version of data.table (don't think
#  it makes a difference...)

data(Batting)
data(Master)
setDT(Batting, key = c("playerID", "yearID"))
setDT(Master, key = "playerID")

#add all stats within-year
Homers <- Batting[ , .(HR = sum(HR)), by = .(playerID, yearID)]

#cumulative homeruns
Homers[ , cum_HR := cumsum(HR), by = playerID]

#up-to-date all-time rankings --
#  in year T, where was each player ranked
#  on the all-time list?
yr0 <- 5 * (Homers[ , min(yearID)] %/% 5)
setkey(Homers, yearID)
top_10 <- rbindlist(lapply(
  all_yr <- Homers[ , unique(yearID)],
  function(yr)
    unique(Homers[.(yr0:yr)], by = "playerID", fromLast = TRUE
    )[, {all_time <- frank(-cum_HR, ties.method = "min")
    .(yearID = yr,
      playerID = playerID[idx <- all_time %in% 1:10],
      HR_rank = all_time[all_time %in% 1:10])}]))

#assign fixed colors to players
## for reproducibility
set.seed(102938)
avail_col <- colors(distinct = TRUE)
#eliminate grayscale colors as unreadable
avail_col <- avail_col[!grepl("gray|white|alice|snow|corn", avail_col)]
avail_col <- avail_col[!grepl("lightsteel|lightcyan|lightgold", avail_col)]
avail_col <- avail_col[!grepl("lavender|mint|lace|beige", avail_col)]
color_map <- 
  data.table(playerID = top_10[, unique(playerID)],
             key = "playerID")[ , col := sample(avail_col, .N)]

#merge in player names
color_map[Master, player_name := 
            paste(i.nameFirst, i.nameLast), on = "playerID"]


top_10[color_map, player_color := i.col, on = "playerID"]

#keying for easy extraction
setkey(top_10, yearID, HR_rank)
setkey(Homers, yearID)

# Shiny Application Part

## Plotting function
traj_yr <- function(yr) 
  Homers[playerID %in% top_10[.(yr), playerID] & yearID <= yr,
         {plot_block <- 
           dcast(.SD, yearID ~ playerID, value.var = "cum_HR"
           )[ , -1, with = FALSE]
         setcolorder(plot_block, names(sort(sapply(plot_block, max, 
                                                   na.rm = TRUE),
                                            decreasing = TRUE)))
         who <- names(plot_block)
         cols <- color_map[.(who), col]
         matplot(unique(yearID), plot_block, ylab = "Home Runs",
                 xlab = "Year", las = 1, xaxt = "n", 
                 xlim = c(min(yearID), yr),
                 main = paste0("Top Slugger Trajectories for ", yr),
                 type = "l", lty = 1, lwd = 3, col = cols)
         axis(1, at = seq(5 * (min(yearID) %/% 5), yr, by = 5))
         legend("topleft", legend = color_map[.(who), player_name],
                col = cols, lty = 1, lwd = 3)}]

ui <- shinyUI(fluidPage(
  titlePanel("All-Time Top 10 HR Leaders by Year"),
  fluidRow(column(12, plotOutput("trajectory", height = "600px"))),
  fluidRow(column(12, sliderInput("yr", "Year:", width = "100%",
                                  min = Homers[ , min(yearID)],
                                  max = Homers[ , max(yearID)], step = 1,
                                  value = Homers[sample(.N,1), yearID])))
))

# server simply runs traj_yr
server <- shinyServer(function(input, output) {
  output$trajectory <- renderPlot(traj_yr(input$yr))
})

# Run the application 
shinyApp(ui = ui, server = server)
