open Roulette
(** RouletteState is a representation of the bets, and the chips bet, made by
    the player of the game *)

type bet
(** [bet] is the type of bet a user makes which could be a Color, Range3, Num,
    Parity, Row, or Col *)

type t
(** [t] is the type of current state in the Roulette game which keeps track of
    the user's current chips, the list of bets that a user has made, the winning
    square in the Roulette game, and which of the user bets satsify the winning
    square's characteristics *)

val init_state : t
(** [init_state] is the initial state which sets chip_amount to 0, the bet_list
    empty, the winning square to Roulette.win_sqr, and the bets_won empty *)

val bet_color : string -> bet
(** [bet_color] returns the Bet Color based on user input. Throws exception
    Unknown_color if user input is not "black" or "red". *)

val bet_num : int -> bet
(** [bet_num] returns the Bet Number based on user input. Throws exception
    Unknown_num if user input is not within [0,36] inclusive. *)

val bet_parity : int -> bet
(** [bet_num] returns the Bet Parity based on user input. Throws exception
    Unknown_parity if user input is not an integer*)

val bet_range2 : int -> bet
(** [bet_range2] returns the Bet Range2 based on user input. Throws exception
    Unknown_range2 if user input is not within [1,3] inclusive. *)

val bet_range3 : int -> bet
(** [bet_range3] returns the Bet Range3 based on user input. Throws exception
    Unknown_range3 if user input is not within [1,2] inclusive. *)

val bet_row : int -> bet
(** [bet_row] returns the Bet Row based on user input. Throws exception
    Unknown_row if user input is not within [1,3] inclusive. *)

val bet_col : int -> bet
(** [bet_col] returns the Bet Column based on user input. Throws exception
    Unknown_col if user input is not within [1,12] inclusive. *)

val init_bet_list : unit -> bet list
(**[init_bet_list] is the empty list used to initialize a list of type bet *)

val bet_list : bet -> t
(**[bet_list] is the list of all bets made by the user appended together *)

val bets_won_list : bet -> unit
(**[bets_won_list] is the list of all the winning bets made my the user appended *)

val to_string : string -> bet -> string
(** [ to_string] is the string representation of a list of bets *)

val get_bets : t -> bet list
(**[get_bets] is the field representing the list of bets made by the user for a
   state t passed into the function *)

val get_bets_won : t -> bet list
(**[get_bets_won] is the field representing the list of winning bets made by the
   user for a state t passed into the function *)

val get_chip_amt : t -> int
(**[get_chip_amt] is the field representing the integer representation of the
   current amount of chips in the game of roulette in any state t passed into
   fucntion*)

val win_color_bet : bet
(**[win_color_bet] is the color of type bet that the winning square made in
   Roulette.ml *)

val win_parity_bet : bet
(**[win_parity_bet] is the parity of type bet that the value of the winning
   square made in Roulette.ml *)

val win_range2_bet : bet
(**[win_range2_bet] is the range of type bet that the value of the winning
   square made in Roulette.ml is in when the board is split into 2 ranges *)

val win_range3_bet : bet
(**[win_range3_bet] is the range of type bet that the value of the winning
   square made in Roulette.ml is in when the board is split into 3 ranges *)

val win_row_bet : bet
(**[win_row_bet] is the winning row of type bet for the winning square made in
   Roulette.ml *)

val win_col_bet : bet
(** [win_col_bet] is the winning column of type bet for the winning square made
    in Roulette.ml *)

val bets_won_after_bets : bet list -> unit
(** [bets_won_after_bets] is the list representation of the bets that the user
    has won after they make their bets. The bets that the user has won are the
    bets that match the properties of the winning square on the board. Throws
    exception based on the bet type being pattern matched if the winning bet
    does not correlate to the type being checked *)

val chips_after_bets : t -> int list -> bet list -> unit
(** [chips_after_bets] is a representatin of a list of the chips that the user
    has after they make their bets. Throws exception based on the bet type being
    pattern matched if the winning bet does not correlate to the type being
    checked *)

val win_num_bet : bet
(** [win_num_bet] is the number of the value of the winning square made in
    Roulette.mli*)

val create_specific_state : int -> bet list -> square -> t
(** [create_specific_state] creates a specific state for OUnit testing *)

val create_specific_bet : int -> string -> int -> int -> bet
(** [create_specific_bet] creates a specific bet for OUnit testing *)
