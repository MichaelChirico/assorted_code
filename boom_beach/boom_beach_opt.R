library(shiny)
library(data.table)
library(viridis)

# variable explanations:
#  - energy: total energy budget willing to expend between artillery, barrage
#  - damage_{artillery, barrage}: (average, for barrage) damage per usage
#  - initial_{a, b}: initial energy cost
#  - marginal_{a, b}: marginal cost (how much does cost increase)

# change-of-variables so that quadratic energy surface is
#   at the origin of the transformed space
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
    numericInput('energy_delta', 'Energy Increment', 3, min = 1),
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
    for (v in names(input)) {
      validate(need(is.integer(input[[v]]), 'Please enter whole numbers only.'))
      assign(v, input[[v]])
    }
    max_artillery = max_alone(energy, initial_artillery, marginal_artillery) + 1
    max_barrage = max_alone(energy, initial_barrage, marginal_barrage) + 1

    DT = CJ(artillery = 0:max_artillery,
            barrage = 0:max_barrage)
    DT[ , damage := damage(artillery, damage_artillery,
                           barrage, damage_barrage)]
    DT[ , cost :=
          energy_cost(artillery, initial_artillery, marginal_artillery) +
          energy_cost(barrage, initial_barrage, marginal_barrage)]
    DT[ , frontier := FALSE]
    DT[cost <= energy, frontier := barrage == max(barrage), by = artillery]
    DT[cost <= energy, frontier :=
         frontier | artillery == max(artillery), by = barrage]

    new_zero_artillery = new_zero(initial_artillery, marginal_artillery)
    new_zero_barrage = new_zero(initial_barrage, marginal_barrage)

    cont_star = continuous_optimum(energy, damage_artillery, damage_barrage,
                                   new_zero_artillery, new_zero_barrage,
                                   marginal_artillery, marginal_barrage)


    DT[(frontier), {
      max_idx = which.max(damage(artillery, damage_artillery,
                                 barrage, damage_barrage))

      # "current" maximal states, for efficiency when updating
      #   maxima w.r.t. additional turret bounty
      AA <<- artillery[max_idx]
      BB <<- barrage[max_idx]

      pt_col = rep('black', .N)
      pt_col[max_idx] = 'red'

      plot(artillery, barrage, pch = 19L, las = 1L, col = pt_col,
           xlim = c(-new_zero_artillery, max_artillery),
           ylim = c(-new_zero_barrage, max_barrage),
           xlab = '# Artillery', ylab = '# Barrage')

      cols = viridis(.N)
      damage_ratio = damage_artillery/damage_barrage
      intercepts = barrage + damage_ratio*artillery
      ranks = frank(intercepts)
      for (ii in seq_len(.N))
        abline(intercepts[ii], -damage_ratio, col = cols[ranks[ii]])
      text(artillery, barrage, pos = rep(c(1L, 3L), length.out = .N),
           col = pt_col, prettyNum(intercepts * damage_barrage, big.mark = ','))

      abline(v = 0:max_artillery, h = 0:max_barrage,
           lwd = .5, col = 'gray')
      abline(v = 0, h = 0)

      legend('bottomleft',
             legend = c('Feasible Pair', 'Optimal Pair',
                        'Optimal Pair (+n Downed Turrets)',
                        'Continuous Optimum', 'Least Damage', 'Most Damage',
                        'Energy Budget', 'Energy Budget (+n Downed Turrets)'),
             col = c('black', 'red', '#FF000077', 'darkblue',
                     cols[1L], cols[.N], 'black', '#00000020'),
             lwd = c(NA, NA, NA, NA, 1L, 1L, 3L, 3L),
             pch = c(19L, 19L, 19L, 0L, NA, NA, NA, NA))
    }]

    abline(v = -new_zero_artillery, h = -new_zero_barrage, lty = 2L, lwd = .75)
    points(cont_star$artillery_star, cont_star$barrage_star,
           pch = 0L, col = 'darkblue')

    # total energy required for the maximal depicted combination
    max_e =
      energy_cost(max_artillery, initial_artillery, marginal_artillery) +
      energy_cost(max_barrage, initial_barrage, marginal_barrage)
    # number of turrets to destroy to reach this energy
    max_n = (max_e - energy)/energy_delta

    for (n_turrets in 0:max_n) {
      ee = energy + energy_delta * n_turrets
      spare_energy = ee -
        energy_cost(AA, initial_artillery, marginal_artillery) -
        energy_cost(BB, initial_barrage, marginal_barrage)
      aa = seq(0, max_alone(ee, initial_artillery,
                            marginal_artillery, round = FALSE),
               length.out = 100L)
      bb = max_alone(ee - energy_cost(aa, initial_artillery,
                                      marginal_artillery),
                     initial_barrage, marginal_barrage, round = FALSE)
      col = if (n_turrets == 0L) 'black' else '#00000020'
      lines(aa, bb, lwd = 3L, col = col)

      if (n_turrets > 0L) {
        DT[cost <= ee, {
          idx = which.max(damage)
          if (!identical(c(artillery[idx], barrage[idx]), c(AA, BB))) {
            AA <<- artillery[idx]
            BB <<- barrage[idx]
            points(AA, BB, pch = 19L, col = '#FF000077')
            text(AA, BB, paste0('+', n_turrets),
                 pos = 3L, col = '#FF000077', xpd = TRUE)
          }
        }]
      }
    }

  })
})

# Run the application
shinyApp(ui = ui, server = server)
