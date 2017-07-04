library(data.table)
#note: in order to get this to deploy to
#  shinyapps, I had to use the CRAN
#  version of data.table (don't think
#  it makes a difference for code)

#as created in social_security_initials.R
full_data = fread('social_security_tabulated_by_year.csv',
                  key = 'birth,first,last',
                  #to facilitate plotting
                  stringsAsFactors = TRUE)[birth>=1900]

#split by birth year
DTsplit = split(full_data, by = 'birth', keep.by = TRUE)

plotdata = rbindlist(lapply(DTsplit, function(DT) {
  #allow for ties by using frank
  DT[ , rnk := frank(-N, ties.method = 'min')]
  DT[rnk <= 10]}))

#reverse y-axis ordering to allow cascading down, not up
plotdata[ , last := factor(last, levels = rev(LETTERS))]

nms = c('first', 'last')
plotdata[ , (nms) := lapply(.SD, as.integer), .SDcols = nms]

#sort & key for faster plotting
setkey(plotdata, birth)
#pre-define colors for faster plotting
plotdata[ , col := ifelse(rnk == 1L, 'red', 'black')]

# Shiny Application Part
xgrd = ygrd = seq(.5, 26.5, length.out = 27)
plot_top = function(yr)
  plotdata[.(yr), {
    plot(NULL, xlim = c(.5, 26.5), ylim = c(.5, 26.5),
         xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', asp = 1L)
    #forcing asp = 1 made the axes appear too far from the plot
    text(1:26, 27, LETTERS)
    text(0, 26:1, LETTERS)
    mtext(side = 3L, 'First Initial')
    text(-1.5, 13, 'Last\nInitial')
    #could use grid, but segments allows more control
    segments(xgrd, .5, xgrd, 26.5)
    segments(0.5, ygrd, 26.5, ygrd)
    #can't use points because pch = 10 turns into 1
    text(first, last, paste0(rnk), col = col, offset = 0)
  }]

ui <- shinyUI(fluidPage(
  titlePanel("Top 10 Most Common Initials by Birth Year"),
  fluidRow(column(12, plotOutput("chart", height = "600px"))),
  fluidRow(column(12, sliderInput("yr", "Year:", width = "100%",
                                  min = plotdata[ , min(birth)],
                                  max = plotdata[ , max(birth)], step = 1,
                                  value = plotdata[sample(.N, 1), birth],
                                  #otherwise thousands separator looks
                                  #  weird for years
                                  sep = '')))
))

# server simply runs traj_yr
server <- shinyServer(function(input, output) {
  output$chart <- renderPlot(plot_top(input$yr))
})

# Run the application 
shinyApp(ui = ui, server = server)
