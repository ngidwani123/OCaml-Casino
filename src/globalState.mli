(** Representation of the global casino state.

    This module represents the the states of the overall Casino. It initializes
    states and handles tracking money lost, money earned net chips, and total
    games of Blackjack played. *)

open State
open Roulette
open RouletteState
open Blackjack

type t
(** The abstract type of values representing states in the Casino. *)

val init_state : int -> t
(** [init_state chips] initializes a global state with [chips] as the chip
    amount *)

val update_bets : t -> int -> t
(** [update_bets state bet] updates [state].bets with [bet] *)

val update_chips : t -> int -> t
(** [update_chips state chips] updates [state].chips with [chips] *)

val update_money_lost : t -> int -> t
(** [update_money_lost state lost] updates [state].money_lost with [lost] *)

val update_money_won : t -> int -> t
(** [update_money_won state won] updates [state].money_won with [won] *)

val update_bj_games : t -> int -> t
(** [update_money_lost state games] updates [state].bj_games with [games] *)

val get_bet : t -> int
(** [get_bet state] returns [state].bets *)

val get_chips : t -> int
(** [get_chips state] returns [state].chips *)

val get_won : t -> int
(** [get_won state] returns [state].money_won *)

val get_lost : t -> int
(** [get_lost state] returns [state].money_lost *)

val get_bj_games : t -> int
(** [get_bj_games state] returns [state].bj_games *)
