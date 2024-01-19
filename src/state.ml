open Blackjack

type t = {
  mutable final_list : hand list;
  mutable temp_hand : hand;
  mutable no_split_hands : hand list;
  mutable split_hands : hand list;
  mutable dealer_cards : Blackjack.card list;
  mutable chip_amount : int;
  mutable player_busted : bool; (* has_blackjack : bool; *)
  mutable dealer_busted : bool;
  mutable dealer_stay : bool;
  mutable player_stay : bool;
}

and hand = card list * int * bool

let init_state (chips : int) : t =
  {
    final_list = [];
    temp_hand = ([], 0, true);
    no_split_hands = [ (Blackjack.deal_cards (), 0, false) ];
    dealer_cards = Blackjack.deal_cards ();
    split_hands = [];
    chip_amount = chips;
    player_busted = false;
    dealer_busted = false;
    dealer_stay = false;
    player_stay = false;
  }

let get_no_split_hands (state : t) : hand list = state.no_split_hands
let get_split_hands (state : t) : hand list = state.split_hands
let get_dealer_cards (state : t) : card list = state.dealer_cards
let get_chips (state : t) : int = state.chip_amount

let get_bet (hand : hand) : int =
  match hand with
  | h, b, bool -> b

let is_split_empty (state : t) : bool =
  if List.length state.split_hands = 0 then true else false

let get_nth_hand (state : t) (index : int) : hand =
  List.nth state.no_split_hands index

let get_nth_hand_final (state : t) (index : int) : hand =
  List.nth state.final_list index

let move_to_split (state : t) : t =
  state.split_hands <- state.no_split_hands;
  state.no_split_hands <- [];
  state

let move_to_no_split (state : t) (hand : hand) : t =
  let hand1 = List.filter (fun x -> x <> hand) state.split_hands in
  state.no_split_hands <- hand :: state.no_split_hands;

  state.split_hands <- hand1;
  state

let get_first_no_split_hand (state : t) : hand = List.hd state.no_split_hands
let get_first_split_hand (state : t) : hand = List.hd state.split_hands
let get_first_final_hand (state : t) : hand = List.hd state.final_list
let no_split_length (state : t) : int = List.length state.no_split_hands - 1
let final_list_length (state : t) : int = List.length state.final_list - 1

let get_first_hand (state : t) : hand =
  let hand_lst = get_no_split_hands state in
  let first_hand = List.hd hand_lst in
  first_hand

let update_chips (state : t) (chips : int) : t =
  state.chip_amount <- chips;
  state

let update_bet (state : t) (bet : int) (hand : hand) : t =
  match hand with
  | x, y, b ->
      let new_hand = (x, bet, b) in
      state.no_split_hands <-
        new_hand :: List.filter (fun x -> x <> hand) state.no_split_hands;
      state

let get_hand (hand : hand) =
  match hand with
  | hand, x, b -> hand

let player_hit (state : t) (start_hand : hand) : t =
  let wanna_hit = ref true in
  let hand = ref (get_hand start_hand) in
  while Blackjack.sum_of_cards 0 !hand < 21 && !wanna_hit = true do
    print_endline "Would you like to hit?";
    match read_line () with
    | "Y" ->
        wanna_hit := true;
        hand := !hand @ [ create_card () ];
        Blackjack.print_cards !hand;
        let sum = sum_of_cards 0 !hand in
        print_endline (string_of_int sum)
    | _ ->
        wanna_hit := false;
        Blackjack.print_cards !hand;
        let sum = sum_of_cards 0 !hand in
        print_endline (string_of_int sum)
  done;
  match start_hand with
  | _, y, b ->
      if Blackjack.sum_of_cards 0 !hand > 21 then
        let temp_hand = (!hand, y, true) in
        let _ = state.final_list <- temp_hand :: state.final_list in
        let _ = print_endline "You have busted" in
        let _ = Unix.sleep 2 in
        state
      else
        let _ = state.final_list <- (!hand, y, false) :: state.final_list in
        let _ = Unix.sleep 2 in
        state

let player_hit_helper (start_hand : hand) : hand =
  match start_hand with
  | x, y, b -> (
      let new_hand = ([ create_card () ] @ x, y, b) in
      match new_hand with
      | x, y, b ->
          if sum_of_cards 0 (get_hand new_hand) > 21 then (x, y, true)
          else (x, y, false))

let hit_once_step (state : t) : card list =
  let lst = get_no_split_hands state in
  let curr_hand = List.hd lst in
  let new_hand = player_hit_helper curr_hand in
  match new_hand with
  | x, y, z -> x

let get_cards_from_list (hand_list : hand list) : card list =
  match hand_list with
  | (x, y, b) :: t -> x

let dealer_hit (state : t) : t =
  if Blackjack.sum_of_cards 0 state.dealer_cards < 21 then
    let new_hand = state.dealer_cards @ [ create_card () ] in
    if Blackjack.sum_of_cards 0 new_hand > 21 then
      let _ = state.dealer_cards <- new_hand in
      let _ = state.dealer_busted <- true in
      state
    else
      let _ = state.dealer_cards <- new_hand in
      state
  else
    let _ = state.dealer_busted <- true in
    state

let double (state : t) (hand : hand) : t =
  let new_hand =
    match hand with
    | x, y, b -> (create_card () :: x, y * 2, b)
  in
  let cards = get_hand new_hand in
  if sum_of_cards 0 cards > 21 then (
    let new_hand1 =
      match new_hand with
      | a, b, c -> (a, b, true)
    in
    state.no_split_hands <-
      new_hand1 :: List.filter (fun x -> x <> hand) state.no_split_hands;
    print_cards (get_hand new_hand1);
    let sum = sum_of_cards 0 (get_hand new_hand1) in
    print_endline (string_of_int sum);
    print_endline
      ("You now have " ^ string_of_int (get_bet new_hand) ^ " chips on the line"))
  else
    state.no_split_hands <-
      new_hand :: List.filter (fun x -> x <> hand) state.no_split_hands;
  print_cards (get_hand new_hand);
  let sum = sum_of_cards 0 (get_hand new_hand) in
  print_endline (string_of_int sum);
  print_endline
    ("You now have " ^ string_of_int (get_bet new_hand) ^ " chips on the line");
  state

let double_helper (hand : hand) : hand =
  if sum_of_cards 0 (get_hand hand) >= 21 then hand
  else
    let new_hand =
      match hand with
      | x, y, b -> (create_card () :: x, y * 2, b)
    in
    let cards = get_hand new_hand in
    if sum_of_cards 0 cards > 21 then
      let new_hand1 =
        match new_hand with
        | a, b, c -> (a, b, true)
      in
      new_hand1
    else new_hand

let check_double (state : t) (hand : hand) : bool =
  match hand with
  | a :: b :: t, x, c -> if state.chip_amount >= 2 * x then true else false
  | _ -> failwith "invalid hand"

let surrender (state : t) (hand : hand) : t =
  match hand with
  | x, y, b ->
      let new_hand = (x, y / 2, true) in
      state.final_list <- new_hand :: state.final_list;
      state

let is_splittable (hand : hand) : bool =
  match hand with
  | a :: b :: t, x, c -> if Blackjack.name_compare a b = 0 then true else false
  | _ -> false

let rec split (state : t) (hand : hand) : t =
  match hand with
  | c1 :: _, x, b -> (
      let cards1 = [ c1 ] @ [ Blackjack.create_card () ] in
      let hand1 = (cards1, x, b) in
      let cards2 = [ c1 ] @ [ Blackjack.create_card () ] in
      let hand2 = (cards2, x, b) in
      state.no_split_hands <- hand1 :: state.no_split_hands;
      state.no_split_hands <- hand2 :: state.no_split_hands;
      if is_splittable hand1 = true then
        state.split_hands <- hand1 :: state.split_hands;
      if is_splittable hand2 = true then
        state.split_hands <- hand2 :: state.split_hands;
      match hand with
      | x, y, b ->
          let _ =
            state.split_hands <-
              List.filter (fun x -> x <> hand) state.split_hands
          in
          state
      | _ -> state)
  | _ -> state

let check_busted (hand : hand) =
  match hand with
  | h, x, b -> if b = true then true else false

let find_winner (state : t) (hand : hand) : string =
  if check_busted hand then "Dealer"
  else if state.dealer_busted then "Player"
  else if
    Blackjack.sum_of_cards 0 (get_hand hand)
    > Blackjack.sum_of_cards 0 state.dealer_cards
  then "Player"
  else if
    Blackjack.sum_of_cards 0 (get_hand hand)
    < Blackjack.sum_of_cards 0 state.dealer_cards
  then "Dealer"
  else "Draw"

let check_blackjack (hand : hand) =
  if sum_of_cards 0 (get_hand hand) = 21 then true else false
