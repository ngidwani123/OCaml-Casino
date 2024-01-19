open Blackjack
(** Representation of states in Blackjack.

    This module represents the the states of Blackjack. It initializes states
    and handles hitting, splitting, doubling, surrendering, and finding winners. *)

type t
(** The abstract type of values representing states in Blackjack. *)

type hand = card list * int * bool
(** The abstract type of values representing hands with card lists, bets, and
    busted booleans. *)

val init_state : int -> t
(** [init_state chips] initializes a state with [chips] as the chip amount *)

val get_dealer_cards : t -> Blackjack.card list
(** [get_dealer_cards state] returns the dealer's cards*)

val player_hit : t -> hand -> t
(** [player_hit state start_hand] adds cards to [start_hand] if the sum is < 21
    and updates the hand's associated boolean with it's busted state. It also
    moves each finalized and hit hand to [state].final_list *)

val update_chips : t -> int -> t
(** [update_chips state chips] updates [state].chip_amount with [chips] *)

val dealer_hit : t -> t
(** [dealer_hit state] adds cards to [state].dealer_cards until the sum is >= 17
    and updates the hand's associated boolean with it's busted state *)

val update_bet : t -> int -> hand -> t
(** [update_bet state bet] updates [hand] with the [bet] and replaces [hand] in
    [state].no_split_list with updated hand with bet *)

val get_chips : t -> int
(** [get_chips state] returns [state].chip_amount *)

val find_winner : t -> hand -> string
(** [find_winner state hand] determines the winner between [state].dealer_cards
    and [hand]*)

val double : t -> hand -> t
(** [double state hand] doubles the bet associated with [hand] and hits the
    [hand]*)

val check_double : t -> hand -> bool
(** [check_double state hand] checks if there are enough chips in [state].
    chip_amount to play double the bet associated with [hand] *)

val surrender : t -> hand -> t
(** [surrender state hand] moves [hand] to [state].final_list immediately *)

val is_splittable : hand -> bool
(** [is_splittable hand] checks if the 2 cards in [hand] are the same value/name *)

val get_no_split_hands : t -> hand list
(** [get_no_split_hands state] returns [state].no_split_hands *)

val get_split_hands : t -> hand list
(** [get_split_hands state] returns [state].split_hands *)

val split : t -> hand -> t
(** [split state hand] splits a [hand] of equal cards, removes [hand] from
    [state].split_hands, and moves the new hands to [state].no_split_hands *)

val move_to_split : t -> t
(** [move_to_split state] moves hands from [state].no_split_hands to
    [state].split_hands *)

val move_to_no_split : t -> hand -> t
(** [move_to_no_split state] moves hands from [state].split_hands to
    [state].no_split_hands *)

val is_split_empty : t -> bool
(** [is_split_empty state] checks if [state].split_hands is empty *)

val get_first_split_hand : t -> hand
(** [get_first_split_hand state] returns the first hand from [state].split_hands *)

val no_split_length : t -> int
(** [no_split_length state] returns the length of [state].no_split_hands *)

val get_nth_hand : t -> int -> hand
(** [get_nth_hand state index] returns the hand at [index] in
    [state].no_split_hands *)

val check_busted : hand -> bool
(** [check_busted hand] checks if [hand] sum > 21 *)

val get_hand : hand -> card list
(** [get_hand hand] gets a card list from a [hand] tuple*)

val get_bet : hand -> int
(** [get_bet hand] returns the bet associated with [hand] *)

val get_first_no_split_hand : t -> hand
(** [get_first_no_split_hand state] returns the first hand in
    [state].no_split_hands *)

val get_first_final_hand : t -> hand
(** [get_first_final_hand state] returns the first hand in [state].final_list *)

val final_list_length : t -> int
(** [final_list_length state] returns the length of [state].final_list *)

val get_nth_hand_final : t -> int -> hand
(** [get_nth_hand_final state index] returns the hand at [index] in
    [state].final_list *)

val get_cards_from_list : hand list -> card list
(** [get_cards_from_list hand_list] returns the list of cards from a [hand_list] *)

val check_blackjack : hand -> bool
(** [check_blackjack hand] checks if the sum of cards in [hand] = 21 *)

val double_helper : hand -> hand
(** [double_helper hand] returns a hand with a doubled bet and hit cards *)

val get_first_hand : t -> hand
(** [get_first_hand state] returns the first non-split hand in [state] *)

val hit_once_step : t -> card list
(** [hit_once_step state] hits the first element of [state].no_split_list and
    returns the hit card list*)
