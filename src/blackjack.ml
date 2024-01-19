type card = {
  name : string;
  value : int;
  suit : string;
}

let card_compare (x : card) (y : card) =
  if x.value < y.value then -1 else if x.value > y.value then 1 else 0

let name_compare (x : card) (y : card) = if x.name = y.name then 0 else -1

let val_from_num (i : int) =
  if i > 0 && i <= 9 then i else if i > 9 then 10 else 0

let name_from_num (i : int) =
  if i > 1 && i < 10 then string_of_int i
  else if i = 10 then "Jack"
  else if i = 11 then "Queen"
  else if i = 12 then "King"
  else "Ace"

let suit_from_num (i : int) =
  let suits = [ "Spades"; "Hearts"; "Clubs"; "Diamonds" ] in
  List.nth suits i

let create_card () =
  Random.self_init ();
  let rand1 = Random.int 13 in
  let rand2 = Random.int 3 in
  let card =
    {
      name = name_from_num rand1;
      value = val_from_num rand1;
      suit = suit_from_num rand2;
    }
  in
  card

let create_specific_card (n : string) (v : int) (s : string) =
  { name = n; value = v; suit = s }

let deal_cards () : card list =
  let card1 = create_card () in
  let card2 = create_card () in
  [ card1; card2 ]

let rec sum_of_cards (acc : int) (cards : card list) =
  match List.sort card_compare cards with
  | [] -> acc + 0
  | h :: t ->
      if h.name = "Ace" then
        if acc + 11 > 21 && acc + 1 > 21 then sum_of_cards (acc + 11) t
        else if acc + 11 = 21 then sum_of_cards (acc + 11) t
        else sum_of_cards (acc + 1) t
      else sum_of_cards (acc + h.value) t

let gui_card (card : card) : string =
  let symbol =
    if card.suit = "Spades" then "♤"
    else if card.suit = "Hearts" then "♡"
    else if card.suit = "Clubs" then "♧"
    else "♢"
  in
  let value =
    if card.name = "Jack" then "J"
    else if card.name = "Queen" then "Q"
    else if card.name = "King" then "K"
    else if card.name = "Ace" then "A"
    else card.name
  in
  "  ┌───────────┐\n  │" ^ value
  ^ "          │\n  │           │\n  │           │\n  │     " ^ symbol
  ^ "     │\n  │           │\n  │           │\n  │          " ^ value
  ^ "│\n  └───────────┘"

let rec print_cards (cards : card list) =
  match cards with
  | [] -> ()
  | h :: t -> (
      Random.self_init ();
      let col = Random.int 2 in
      match col with
      | 0 ->
          ANSITerminal.print_string [ ANSITerminal.red ] (gui_card h);

          print_endline "";
          print_cards t
      | 1 ->
          print_endline (gui_card h);
          print_cards t
      | _ -> print_string "error")
