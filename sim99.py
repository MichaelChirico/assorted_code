from random import shuffle
# use heapq to more efficiently
#   replace played cards and re-sort hands
import heapq as hq
# for data storage, output
import pandas as pd
# for forcing data types in pandas
import numpy as np

def create_and_shuffle(n_decks = 2):
    one_suit = ['2', '3', '4', '5', '6', '7',
                '8', '9', '-', 'J', 'Q', 'K', 'A']
    # Use * for Joker and - for 10
    #   to maintain single-width avatars for cards
    one_deck = one_suit * 4 + ['*'] * 2
    all_decks = one_deck * n_decks
    shuffle(all_decks)
    return all_decks

def play_card(card, V, M):
    # return (delta, changeDirection) pair
    #   dictating by how much to change
    #   game_value, and whether direction
    #   of play has changed; return
    #   'NEXT_CARD' if current card can't
    #   be played
    if card < 0:
        if V - card <= M:
            # play the high card;
            #   subtract the negative value to increment
            return (-card, False)
        else:
            # can't play high card; abandon
            return ('NEXT_CARD', None)
    elif card == 0:
        # Ace -- if 11 can be played, do so;
        #        if not and 1 can be played, do so;
        #        otherwise, play a different card
        if V + 11 <= M:
            return (11, False)
        elif V + 1 <= M:
            return (1, False)
        else:
            return ('NEXT_CARD', None)
    elif card == 1:
        # Joker: game_value -> game_end
        return (M - V, False)
    elif card == 2:
        # 9: game_value unchanged
        return (0, False)
    elif card == 3:
        # Jack: reverse direction of turns
        return (0, True)
    elif card == 4:
        # 10: decrement game_value by 10
        return (-10, True)
    else:
        raise Exception('HOW DID YOU GET HERE?')

game_end = 99
n_players = 5
cards_in_hand = 3
# for initial decision -- trying
#   to dump the largest possible non-special card;
#   use negative values as a pat on the back to heapq
#   (which is a min heap not a max heap)
#   code all specials as >= 0 for separate treatment
#   (while maintaining precedence ordering)
card_values = {
  '2': -2, '3': -3, '4': -4, '5': -5,
  '6': -6, '7': -7, '8': -8, '9': 2,
  '-': 4, 'J': 3, 'Q': -10,
  'K': -10, 'A': 0, '*': 1
}

# for reverse-engineering the card value
#   from the precedence value (only used
#   when verbose = True)
card_from_val = ['Q/K', '', '8', '7', '6', '5', '4', '3',
                 '2', '', 'A', 'Joker', '9', 'J', '10']

# long to have typed deep in nested statements
update_str = 'Player %d played %s; Game Value = %d; play moves to %d'

columns = ['player_id', 'card_value', 'game_value']
def simulate_round(id = 0, verbose = False):
    # shuffle, reset score,
    deck = create_and_shuffle()
    if verbose:
        print 'Full Shuffle:\n' + ''.join(deck)
    game_value = 0
    value_history = []
    # use list comprehension to avoid mis-step:
    # https://stackoverflow.com/q/240178/
    hands = [[] for _ in range(n_players)]
    # deal cards sequentially
    #   (better: simply extract the elements and reshape,
    #    but this is more intuitive -- popping imitates
    #    the actual deal quite accurately, especially
    #    with the use of the heap queue)
    # only track the values of the cards since this is
    #   less unwieldy to keep track of, and there's 1-1
    #   mapping from these values to specials (also, from
    #   the game's perspective, Q & K are identical)
    for deal_round_j in range(cards_in_hand):
        for player_i in range(n_players):
            hq.heappush(hands[player_i], card_values[deck.pop()])
    if verbose:
        print 'Hands as Dealt\n' + str(hands)
    card_history = []

    current_player = 0
    player_history = []
    # toggle by -1 whenever a Jack is played
    increment = 1
    game_continues = True
    while game_continues:
        player_history += [current_player]
        # rely on sorting of hands
        current_card = hq.heappop(hands[current_player])
        # keep track of unusable cards, as well
        #   as the newly drawn card
        try:
            return_to_hand = [card_values[deck.pop()]]
        # shouldn't be possible except for edge
        #   cases of incompatibility of n_players,
        #   n_decks, and cards_in_hand
        except IndexError:
            raise Exception('RAN OUT OF CARDS?')
        # try to play each of this player's
        #   cards in turn, executing the first
        #   possibility & "booting" the player otherwise
        while True:
            delta, reverse = \
              play_card(current_card, game_value, game_end)
            # couldn't play this card
            if delta == 'NEXT_CARD':
                # need to restore this card to hand below
                return_to_hand += [current_card]
                # retrieve next-precedence card
                try:
                    current_card = hq.heappop(hands[current_player])
                # no cards to play -> game over
                except IndexError:
                    if verbose:
                        print ('Player %d Loses!' %
                               current_player)
                    game_continues = False
                    value_history += [None]
                    card_history += [None]
                    break
            #card successfully played
            else:
                game_value += delta
                if reverse:
                    increment *= -1
                # now restore current player's hand
                [hq.heappush(hands[current_player], card)
                   for card in return_to_hand]
                value_history += [game_value]
                card_history += [current_card]

                if verbose:
                    just_played = current_player
                current_player = (current_player + increment) \
                                   % n_players
                if verbose:
                    print (update_str %
                             (just_played,
                              card_from_val[current_card + 10],
                              game_value, current_player))
                    print 'Current Hands:\n%s' % str(hands)
                break
    round_columns = [player_history, card_history, value_history]
    # transposing using trick here:
    #   https://stackoverflow.com/a/6473724/3576984
    round_rows = map(list, zip(*round_columns))
    DF = pd.DataFrame(round_rows, columns = columns)
    DF['game_id'] = id
    DF.index.names = ['turn_id']
    return DF

n_simulations = 10000
simulations = pd.concat(
  [simulate_round(i) for i in range(n_simulations)],
  axis = 0
)
simulations.to_csv('out.csv')
