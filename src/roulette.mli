(** Initializes a game of roulette. *)

type square
(** [square] is the type of Roulette number which contains an int value and a
    string color *)

val init_list : square array
(** [init_list] is the initial square array with 37 entries of squares defaulted
    with the black color and 1-37 value *)

val duplicate_list : int array
(** [duplicate_list] is an int array with 37 values of [-37, 0] to keep track of
    number duplication when randomizing the roulette table *)

val dlist_iter : int ref
(** [dlist_iter] is an int ref that is used as a counter for add_square *)

val add_square : unit -> int
(** [add_square] prevents square duplication by checking if an int value of a
    square is already in init_list *)

val create_list : square array -> unit
(** [create_list] creates a randomized arrangement of square values [0, 37] with
    alternating black and red color *)

val win_sqr : unit -> square
(** [win_sqr] is a random square in the updated init_list after create_list is
    called. This is the winning roulette square. *)

val print_row : square array -> int -> unit
(** [print_row] prints a row of our custom roulette board UI *)

val print_square_list : unit -> unit
(** [print_square_list] prints the entire roulette board UI *)

val create_specific_square : int -> string -> square
(** [create_specific_square] creates a specific square for OUnit testing *)

val get_value : square -> int
(** [get_value] returns the int value of a given square *)

val get_color : square -> string
(** [get_color] returns the string color of a given square *)