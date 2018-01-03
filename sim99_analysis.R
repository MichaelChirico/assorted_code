library(data.table)

python = '/Users/michael.chirico/anaconda2/bin/python'
game_end = 99L
n_players = 5L
cards_in_hand = 3L
n_simulations = 50000L
system(sprintf('%s sim99.py %d %d %d %d', python, game_end,
               n_players, cards_in_hand, n_simulations))
simf = sprintf('sim_%d_max_%d_players_%d_cards.csv',
               game_end, n_players, cards_in_hand)
sims = fread(simf)
file.remove(simf)

# game value distribution by turn
sims[ , {
  png('~/Desktop/points_by_turn.png')
  boxplot(game_value ~ turn_id, col = 'blue',
          xlab = 'Turn Number', ylab = 'Points on the Board',
          main = 'Game Value Distribution by Turn')
  dev.off()
  NULL
}]

# final turn distribution
sims[is.na(game_value), {
  png('~/Desktop/final_turn_number.png')
  plot(table(turn_id)/.N, xlab = 'Turn Number',
       ylab = 'Relative Frequency in Simulations',
       main = 'Distribution of Final Turn')
  dev.off()
}]

# distribution of number of turns between
#   first redlining and game's end
sims[ , {
  party_time = which.max(game_value == max(game_value, na.rm = TRUE))
  .(panic_rounds = turn_id[.N] - turn_id[party_time])
}, keyby = game_id][ , {
  png('~/Desktop/party_time.png')
  plot(table(panic_rounds)/.N,
       xlab = 'Number of Hot-Button Rounds',
       ylab = 'Relative Frequency in Simulations',
       main = 'Distribution of Panicked Play')
  dev.off()
}]

# given the current score, how many more turns
#   should you expect before the going gets tough?
sims[ , {
  party_time = which.max(game_value == max(game_value, na.rm = TRUE))
  rounds_to_party_time = turn_id[party_time] - turn_id
  cool_idx = rounds_to_party_time > 0
  .(rtpt = rounds_to_party_time[cool_idx],
    gv = game_value[cool_idx])
}, keyby = game_id][ , {
  png('~/Desktop/turns_left.png')
  boxplot(rtpt ~ gv, notch = TRUE, col = 'darkgreen',
          xlab = 'Current Score', ylab = 'Turns Until Red Zone',
          main = 'Expected Number of Turns Remaining')
  dev.off()
}]

# how many special cards per player in a game?
sims[ , .(n_special = sum(card_value > 0, na.rm = TRUE)),
      keyby = .(game_id, player_id)
      ][ , .N, keyby = n_special
         ][ , {
           png('~/Desktop/specials_played.png')
           barplot(N/sum(N), col = 'darkred',
                   names.arg = n_special,
                xlab = 'Number of Trick Cards Played',
                ylab = 'Relative Frequency in Simulations',
                main = 'How Many Specials Will I Get to Play?')
           dev.off()
         }]

# how many games end without a single special card being played?
#   (drop games that end before any cards are "actively" played
sims[ , if (.N > 2) all(card_value < 0, na.rm = TRUE),
      keyby = game_id][ , mean(V1)]

# as the first player to drop a trick,
#   how likely are you to lose?
sims[ , {
  trick_idx = card_value > 0
  if (.N > 2L & any(trick_idx, na.rm = TRUE)) {
    first_trick = which.max(trick_idx)
    player_id[.N] == player_id[first_trick]
  }
}, keyby = game_id][ , mean(V1)]
