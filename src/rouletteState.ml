open Roulette

exception Unknown_color of string
exception Unknown_num of int
exception Unknown_parity of string
exception Unknown_range2 of int
exception Unknown_range3 of int
exception Unknown_row of int
exception Unknown_col of int
exception Unknown_bet of string

type bet =
  | Color of string
  | Range3 of (int * int)
  | Range2 of (int * int)
  | Num of int
  | Parity of string
  | Row of int
  | Col of int

type t = {
  mutable chip_amount : int;
  mutable bet : bet list;
  mutable win_square : square;
  mutable bets_won : bet list;
}

let init_bet_list () = []

let init_state =
  {
    chip_amount = 0;
    bet = init_bet_list ();
    win_square = Roulette.win_sqr ();
    bets_won = init_bet_list ();
  }

let get_bets state = state.bet
let get_bets_won state = state.bets_won
let get_chip_amt state = state.chip_amount

let bet_color col =
  match col with
  | "Red" | "red" -> Color "Red"
  | "Black" | "black" -> Color "Black"
  | _ -> raise (Unknown_color col)

let bet_num number =
  let num_bet_made = Num number in
  let num_exception = Unknown_num number in
  match number < 36 with
  | true -> if number < 0 then raise num_exception else num_bet_made
  | false -> raise num_exception

let bet_parity number =
  try
    match number mod 2 with
    | 0 -> Parity "even"
    | _ -> Parity "odd"
  with Not_found -> raise (Unknown_parity "error: invalid number")

let bet_range2 number =
  match number with
  | 1 -> Range2 (0, 18)
  | 2 -> Range2 (19, 36)
  | _ -> raise (Unknown_range2 number)

let bet_range2_winner number =
  match number > -1 with
  | true -> begin
      match number < 19 with
      | true -> Range2 (0, 18)
      | false -> begin
          match number < 37 with
          | true -> Range2 (19, 36)
          | false -> raise (Unknown_range2 number)
        end
    end
  | false -> raise (Unknown_range2 number)

let bet_range3 number =
  match number with
  | 1 -> Range3 (0, 12)
  | 2 -> Range3 (13, 24)
  | 3 -> Range3 (25, 36)
  | _ -> raise (Unknown_range3 number)

let bet_range3_winner number =
  match number > -1 with
  | true -> begin
      match number < 12 with
      | true -> Range3 (0, 12)
      | false -> begin
          match number < 24 with
          | true -> Range3 (13, 24)
          | false -> begin
              match number < 37 with
              | true -> Range3 (25, 36)
              | false -> raise (Unknown_range3 number)
            end
        end
    end
  | false -> raise (Unknown_range3 number)

let bet_row number =
  match number with
  | 1 -> Row 1
  | 2 -> Row 2
  | 3 -> Row 3
  | _ -> raise (Unknown_row number)

let bet_row_winner number =
  match number > 0 with
  | true -> begin
      match number < 12 with
      | true -> Row 1
      | false -> begin
          match number < 24 with
          | true -> Row 2
          | false -> begin
              match number < 36 with
              | true -> Row 3
              | false -> raise (Unknown_row number)
            end
        end
    end
  | false -> raise (Unknown_row number)

let bet_col number =
  match number with
  | 1 -> Col 1
  | 2 -> Col 2
  | 3 -> Col 3
  | 4 -> Col 4
  | 5 -> Col 5
  | 6 -> Col 6
  | 7 -> Col 7
  | 8 -> Col 8
  | 9 -> Col 9
  | 10 -> Col 10
  | 11 -> Col 11
  | 12 -> Col 12
  | _ -> raise (Unknown_col number)

let bet_col_winner number =
  match number mod 12 with
  | 0 -> Col 1
  | 1 -> Col 2
  | 2 -> Col 3
  | 3 -> Col 4
  | 4 -> Col 5
  | 5 -> Col 6
  | 6 -> Col 7
  | 7 -> Col 8
  | 8 -> Col 9
  | 9 -> Col 10
  | 10 -> Col 11
  | 11 -> Col 12
  | _ -> raise (Unknown_col number)

let bet_list bet_made =
  let new_bet = bet_made :: init_state.bet in
  init_state.bet <- new_bet;
  init_state

let bets_won_list bet_made =
  let new_bet = bet_made :: init_state.bets_won in
  init_state.bets_won <- new_bet

let find_index (arr : Roulette.square array) (win_val : Roulette.square)
    (index : int) =
  let rec find arr win_val index =
    if arr.(index) = win_val then index else find arr win_val (index + 1)
  in
  try find arr win_val 0 with _ -> raise (Unknown_num 0)

let winner = init_state.win_square
let winner_val = Roulette.get_value winner
let winner_color = Roulette.get_color winner
let winner_num_props (bet_fun : int -> bet) = bet_fun winner_val
let winner_index_props (bet_fun : int -> bet) (index : int) = bet_fun index
let win_color_bet = bet_color winner_color
let win_num_bet = winner_num_props bet_num
let win_parity_bet = winner_num_props bet_parity
let win_range2_bet = winner_num_props bet_range2_winner
let win_range3_bet = winner_num_props bet_range3_winner

let win_row_bet =
  winner_index_props bet_row_winner (find_index Roulette.init_list winner 0)

let win_col_bet =
  winner_index_props bet_col_winner (find_index Roulette.init_list winner 0)

let rec bets_won_after_bets bets =
  match bets with
  | [] -> ()
  | h :: t -> begin
      match h with
      | Color c ->
          if c = winner_color then bets_won_list h
          else init_state.bets_won <- init_state.bets_won;
          bets_won_after_bets t
      | Range2 (x, y) -> begin
          match win_range2_bet with
          | Range2 (a, b) ->
              if x = a && b = y then bets_won_list h
              else init_state.bets_won <- init_state.bets_won;
              bets_won_after_bets t
          | _ -> raise (Unknown_range2 3)
        end
      | Range3 (x, y) -> begin
          match win_range3_bet with
          | Range3 (a, b) ->
              if x = a && b = y then bets_won_list h
              else init_state.bets_won <- init_state.bets_won;
              bets_won_after_bets t
          | _ -> raise (Unknown_range3 4)
        end
      | Parity p -> begin
          match win_parity_bet with
          | Parity wp ->
              if wp = p then bets_won_list h
              else init_state.bets_won <- init_state.bets_won;
              bets_won_after_bets t
          | _ -> raise (Unknown_parity "Error")
        end
      | Num n -> begin
          match win_num_bet with
          | Num nw ->
              if nw = n then bets_won_list h
              else init_state.bets_won <- init_state.bets_won;
              bets_won_after_bets t
          | _ -> raise (Unknown_num 37)
        end
      | Row r -> begin
          match win_row_bet with
          | Row rw ->
              if rw = r then bets_won_list h
              else init_state.bets_won <- init_state.bets_won;
              bets_won_after_bets t
          | _ -> raise (Unknown_row 4)
        end
      | Col c -> begin
          match win_col_bet with
          | Col cw ->
              if cw = c then bets_won_list h
              else init_state.bets_won <- init_state.bets_won;
              bets_won_after_bets t
          | _ -> raise (Unknown_row 4)
        end
      | _ ->
          init_state.bets_won <- init_state.bets_won;
          bets_won_after_bets t
    end

let return_tail (chip_list : int list) =
  match chip_list with
  | [] -> []
  | h :: t -> t

let rec chips_after_bets (state : t) chip_list bet_list =
  match bet_list with
  | [] -> ()
  | h :: t -> begin
      match h with
      | Color c -> (
          match chip_list with
          | [] -> ()
          | x :: y ->
              if c = winner_color then (
                state.chip_amount <- state.chip_amount + (2 * x);
                chips_after_bets state (return_tail chip_list) t)
              else state.chip_amount <- state.chip_amount - 0;
              chips_after_bets state (return_tail chip_list) t)
      | Num n -> (
          match chip_list with
          | [] -> ()
          | x :: y -> (
              match win_num_bet with
              | Num nw ->
                  if nw = n then (
                    state.chip_amount <- state.chip_amount + (36 * x);
                    chips_after_bets state (return_tail chip_list) t)
                  else state.chip_amount <- state.chip_amount - 0;
                  chips_after_bets state (return_tail chip_list) t
              | _ -> raise (Unknown_num 0)))
      | Parity p -> (
          match chip_list with
          | [] -> ()
          | x :: y -> (
              match win_parity_bet with
              | Parity pw ->
                  if pw = p then (
                    state.chip_amount <- state.chip_amount + (2 * x);
                    chips_after_bets state (return_tail chip_list) t)
                  else state.chip_amount <- state.chip_amount - 0;
                  chips_after_bets state (return_tail chip_list) t
              | _ -> raise (Unknown_parity "Not odd or even")))
      | Row r -> (
          match chip_list with
          | [] -> ()
          | x :: y -> (
              match win_row_bet with
              | Row rw ->
                  if rw = r then (
                    state.chip_amount <- state.chip_amount + (3 * x);
                    chips_after_bets state (return_tail chip_list) t)
                  else state.chip_amount <- state.chip_amount - 0;
                  chips_after_bets state (return_tail chip_list) t
              | _ -> raise (Unknown_row 0)))
      | Col c -> (
          match chip_list with
          | [] -> ()
          | x :: y -> (
              match win_col_bet with
              | Col cw ->
                  if cw = c then (
                    state.chip_amount <- state.chip_amount + (12 * x);
                    chips_after_bets state (return_tail chip_list) t)
                  else state.chip_amount <- state.chip_amount - 0;
                  chips_after_bets state (return_tail chip_list) t
              | _ -> raise (Unknown_col 0)))
      | Range2 (a, b) -> (
          match chip_list with
          | [] -> ()
          | x :: y -> (
              match win_range2_bet with
              | Range2 (c, d) ->
                  if c = a && d = b then (
                    state.chip_amount <- state.chip_amount + (2 * x);
                    chips_after_bets state (return_tail chip_list) t)
                  else state.chip_amount <- state.chip_amount - 0;
                  chips_after_bets state (return_tail chip_list) t
              | _ -> raise (Unknown_range2 3)))
      | Range3 (a, b) -> (
          match chip_list with
          | [] -> ()
          | x :: y -> (
              match win_range3_bet with
              | Range3 (c, d) ->
                  if c = a && d = b then (
                    state.chip_amount <- state.chip_amount + (3 * x);
                    chips_after_bets state (return_tail chip_list) t)
                  else state.chip_amount <- state.chip_amount - 0;
                  chips_after_bets state (return_tail chip_list) t
              | _ -> raise (Unknown_range3 4)))
      | _ -> init_state.chip_amount <- init_state.chip_amount
    end

let to_string (str : string) bet =
  match bet with
  | Color c -> str ^ "color is: " ^ c
  | Range2 (x, y) ->
      if x = 0 then str ^ "range2 is: (0-18)"
      else if x = 19 then str ^ "range2 is: (19-36)"
      else raise (Unknown_range2 3)
  | Range3 (x, y) ->
      if x = 0 then str ^ "range3 is: (0-12)"
      else if x = 13 then str ^ "range3 is: (13-24)"
      else if x = 25 then str ^ "range3 is: (25-36)"
      else raise (Unknown_range3 4)
  | Num n -> str ^ "number is: " ^ string_of_int n
  | Parity p -> str ^ "parity is: " ^ p
  | Row r -> str ^ "row is: " ^ string_of_int r
  | Col c -> str ^ "column is: " ^ string_of_int c

let create_specific_state (amt : int) (bets : bet list) (win : square) =
  let state =
    { chip_amount = amt; bet = bets; win_square = win; bets_won = [] }
  in
  state

let create_specific_bet (choice : int) (val1 : string) (val2 : int) (val3 : int)
    =
  match choice with
  | 1 -> Color val1
  | 2 -> Range3 (val2, val3)
  | 3 -> Range2 (val2, val3)
  | 4 -> Num val2
  | 5 -> Parity val1
  | 6 -> Row val2
  | 7 -> Col val2
  | _ -> raise (Unknown_bet "Could not create specific bet")