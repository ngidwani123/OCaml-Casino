open Blackjack
open State
open Roulette
open RouletteState

type t = {
  mutable init_chips : int;
  mutable bets : int;
  mutable money_lost : int;
  mutable money_won : int;
  mutable bj_games : int;
}

let init_state (chips : int) : t =
  { init_chips = chips; bets = 0; money_lost = 0; money_won = 0; bj_games = 0 }

let update_chips (state : t) (chips : int) =
  state.init_chips <- chips;
  state

let update_bj_games (state : t) (games : int) =
  state.bj_games <- games;
  state

let update_bets (state : t) (bets : int) =
  state.bets <- bets;
  state

let update_money_lost (state : t) (lost : int) =
  state.money_lost <- lost;
  state

let update_money_won (state : t) (won : int) =
  state.money_won <- won;
  state

let get_bet (state : t) = state.bets
let get_chips (state : t) = state.init_chips
let get_won (state : t) = state.money_won
let get_lost (state : t) = state.money_lost
let get_bj_games (state : t) = state.bj_games
