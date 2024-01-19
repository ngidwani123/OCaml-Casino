(* * Representation of static game data. This module represents the data stored
   in game files. *)

type card
(** The abstract type of values representing cards. *)

val val_from_num : int -> int
(** [val_from_num i] the value of a card that is associated with i. Requires: i
    is an int between 0-12 inclusive *)

val name_from_num : int -> string
(** [name_from_num i] the name of a card that is associated with i. Requires: i
    is an int between 0-12 inclusive *)

val suit_from_num : int -> string

(** [suit_from_num i] the suit of a card. Requires: i is an into between 0-3
    inclusive *)

val create_card : unit -> card
(** [create_card] is a card that is assigned a name and value based on a random
    integer between 0 and 13 (exclusive upper bound) and a suit based on a
    random integer 0-4 (exclsuive upper bound)*)

val deal_cards : unit -> card list
(** [deal_cards] is the result of dealing created cards. If the two new created
    cards are created and dealt until the cards are not equal to each other*)

val sum_of_cards : int -> card list -> int
(** [sum_of_cards cards] is the sum of all the cards in a [cards] which is a
    list of the cards dealt*)

val print_cards : card list -> unit
(** [print_cards cards] is the printing of the cards in [cards] which is a list
    of the cards dealt *)

val gui_card : card -> string
(** [gui_card card] is the printing of a singular [card] with its symbol and
    number *)

val create_specific_card : string -> int -> string -> card
(** [create_specific_card n v s] creates a card with name [n], value [v], and
    suite [s] for testing purposes *)

val name_compare : card -> card -> int
(** [name_compare x y] compares the names of cards, returning 0 if equal and -1
    otherwise *)

val card_compare : card -> card -> int
(** [name_compare x y] compares the value of cards, returning 0 if equal, -1 if
    x < y, and 1 if x > y *)
