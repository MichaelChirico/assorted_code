library(data.table)
#note: in order to get this to deploy to
#  shinyapps, I had to use the CRAN
#  version of data.table (don't think
#  it makes a difference...)

full_data = fread('social_security_tabulated_by_year.csv',
                  key = 'birth,first,last',
                  stringsAsFactors = TRUE)
nms = c('first', 'last')
full_data[ , (nms) := lapply(.SD, as.integer), .SDcols = nms]

DTsplit = split(full_data, by = 'birth', keep.by = TRUE)

plotdata = rbindlist(lapply(DTsplit, function(DT) {
  DT[ , rnk := frank(-N, ties.method = 'min')]
  DT[rnk <= 10]}))
setkey(plotdata, birth)
plotdata[ , col := ifelse(rnk == 1L, 'red', 'black')]

# Shiny Application Part

xgrd = ygrd = seq(.5, 26.5, length.out = 27)
plot_top = function(yr)
  plotdata[.(yr), {
    plot(NULL, xlim = c(.5, 26.5), ylim = c(.5, 26.5),
         xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', asp = 1L)
    text(1:26, 0, LETTERS)
    text(0, 1:26, LETTERS)
    segments(xgrd, .5, xgrd, 26.5)
    segments(0.5, ygrd, 26.5, ygrd)
    text(first, last, paste0(rnk), col = col, offset = 0)
  }]

ui <- shinyUI(fluidPage(
  titlePanel("Most Common Initials by Birth Year"),
  fluidRow(column(12, plotOutput("chart", height = "600px"))),
  fluidRow(column(12, sliderInput("yr", "Year:", width = "100%",
                                  min = plotdata[ , min(birth)],
                                  max = plotdata[ , max(birth)], step = 1,
                                  value = plotdata[sample(.N, 1), birth],
                                  sep = '')))
))

# server simply runs traj_yr
server <- shinyServer(function(input, output) {
  output$chart <- renderPlot(plot_top(input$yr))
})

# Run the application 
shinyApp(ui = ui, server = server)
