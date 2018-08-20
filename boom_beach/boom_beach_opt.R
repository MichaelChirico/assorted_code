library(shiny)
library(viridis)

# variable explanations:
#  - energy: total energy budget willing to expend between artillery, barrage
#  - damage_{artillery, barrage}: (average, for barrage) damage per usage
#  - initial_{a, b}: initial energy cost
#  - marginal_{a, b}: marginal cost (how much does cost increase)

# change-of-variables so that quadratic energy surface is at the origin
new_zero = function(initial, marginal) .5*(2*initial - marginal)/marginal

# under change-of-variables, energy budget becomes this "shadow" energy
shadow_energy = function(energy, marginal_barrage, marginal_artillery,
                         new_zero_artillery, new_zero_barrage) {
  (2*energy + marginal_artillery*new_zero_artillery^2 +
       marginal_barrage*new_zero_barrage^2)/marginal_barrage
}

continuous_optimum = function(energy, damage_artillery, damage_barrage,
                              new_zero_artillery, new_zero_barrage,
                              marginal_artillery, marginal_barrage) {
  C = shadow_energy(energy, marginal_barrage, marginal_artillery,
                    new_zero_artillery, new_zero_barrage)

  damage_ratio = damage_artillery/damage_barrage
  marginal_ratio = marginal_artillery/marginal_barrage
  p2r = damage_ratio^2/marginal_ratio
  list(
    artillery_star = sqrt(C/marginal_ratio/(1 + 1/p2r)) - new_zero_artillery,
    barrage_star = sqrt(C/(1 + p2r)) - new_zero_barrage
  )
}

# how much energy does it cost to fire n times?
energy_cost = function(n, initial, marginal) {
  n*(initial + marginal*(n-1L) / 2L)
}

# using this ability only, how many times can it be fired?
#   (basically discretizing a quadratic solution)
max_alone = function(energy, initial, marginal, round = TRUE) {
  b = initial/marginal - .5
  true = sqrt(b^2 + 2*energy/marginal) - b
  if (round) as.integer(floor(true)) else true
}

damage = function(artillery, damage_artillery,
                  barrage, damage_barrage) {
  artillery * damage_artillery + barrage * damage_barrage
}

ui <- shinyUI(pageWithSidebar(
  headerPanel('Maximizing Damage between Artillery & Barrage Salvos'),
  sidebarPanel(
    numericInput('energy', 'Energy Budget', 83, min = 1),
    h1('Artillery'),
    numericInput('damage_artillery', 'Damage', 9396, min = 0),
    numericInput('initial_artillery', 'First Artillery Cost', 3, min = 0),
    numericInput('marginal_artillery', 'Artillery Cost Increment', 2, min = 0),
    h1('Barrage'),
    numericInput('damage_barrage', 'Damage', 12000, min = 0),
    numericInput('initial_barrage', 'First Barrage Cost', 10, min = 0),
    numericInput('marginal_barrage', 'Barrage Cost Increment', 6, min = 0)
  ),
  mainPanel(
    plotOutput('plot')
  )
))

server <- shinyServer(function(input, output) {
  output$plot = renderPlot({
    for (v in names(input))
      assign(v, input[[v]])
    max_barrage = max_alone(energy, initial_barrage, marginal_barrage)
    max_artillery = max_alone(energy, initial_artillery, marginal_artillery)

    barrages = 0:max_barrage
    artilleries =
      max_alone(energy -
                  energy_cost(barrages, initial_barrage, marginal_barrage),
                initial_artillery, marginal_artillery)
    artilleries_new = setdiff(0:max_artillery, artilleries)
    barrages_new =
      max_alone(energy -
                  energy_cost(artilleries_new,
                              initial_artillery, marginal_artillery),
                initial_barrage, marginal_barrage)
    barrages = sort(c(barrages, barrages_new))
    artilleries = sort(c(artilleries, artilleries_new), decreasing = TRUE)
    n = length(artilleries)

    new_zero_artillery = new_zero(initial_artillery, marginal_artillery)
    new_zero_barrage = new_zero(initial_barrage, marginal_barrage)

    cont_star = continuous_optimum(energy, damage_artillery, damage_barrage,
                                   new_zero_artillery, new_zero_barrage,
                                   marginal_artillery, marginal_barrage)

    max_idx = which.max(damage(artilleries, damage_artillery,
                               barrages, damage_barrage))
    pt_col = rep('black', n)
    pt_col[max_idx] = 'red'

    plot(artilleries, barrages, pch = 19L, las = 1L, col = pt_col,
         xlim = c(-new_zero_artillery, artilleries[1L] + 1),
         ylim = c(-new_zero_barrage, max_barrage + 1),
         xlab = '# Artillery', ylab = '# Barrage')
    lims = par('usr')
    abline(v = floor(lims[1L]):ceiling(lims[2L]),
           h = floor(lims[3L]):ceiling(lims[4L]),
           lwd = .5, col = 'gray')
    abline(v = 0, h = 0)
    abline(v = -new_zero_artillery,
           h = -new_zero_barrage,
           lty = 2L, lwd = .75)
    points(cont_star$artillery_star, cont_star$barrage_star,
           pch = 0L, col = 'darkblue')

    aa = seq(0, max_alone(energy, initial_artillery,
                          marginal_artillery, round = FALSE),
             length.out = 100L)
    bb = max_alone(energy - energy_cost(aa, initial_artillery,
                                        marginal_artillery),
                   initial_barrage, marginal_barrage, round = FALSE)
    lines(aa, bb, lwd = 3L)

    cols = viridis(n)
    damage_ratio = damage_artillery/damage_barrage
    intercepts = barrages + damage_ratio*artilleries
    ranks = rank(intercepts)
    for (ii in seq_len(n))
      abline(intercepts[ii], -damage_ratio,
             col = cols[ranks[ii]])
    text(artilleries, barrages, pos = rep(c(1L, 3L), length.out = n),
         col = pt_col, prettyNum(intercepts * damage_barrage, big.mark = ','))

    legend('bottomleft',
           legend = c('Feasible Pair', 'Optimal Pair', 'Continuous Optimum',
                      'Least Damage', 'Most Damage', 'Energy Budget'),
           col = c('black', 'red', 'darkblue',
                   cols[1L], cols[n], 'black'),
           lwd = c(NA, NA, NA, 1L, 1L, 3L),
           pch = c(19L, 19L, 0L, NA, NA, NA))
  })
})

# Run the application
shinyApp(ui = ui, server = server)
