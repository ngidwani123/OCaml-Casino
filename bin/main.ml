open Game
open Blackjack
open State
open Roulette
open RouletteState

(* * [play_game] starts the game. *)
(* let play_game = raise (Failure "Unimplemented: Main.play_game") *)

(** [main ()] prompts for the game to play, then starts it. *)
(* let main () = ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to
   the 3110 BlackJaCK Game.\n"; print_endline "Please enter the name of the game
   file you want to load.\n"; print_string "> "; match read_line () with | _ ->
   read_line *)

(* Execute the game engine. *)

(* let () = main () *)

(* let play_game (chips : string) = print_endline "Your two cards are: "; let
   initial = Game.State.init_state(int_of_string chips) in
   Game.Blackjack.print_cards (Game.State.get_player_cards initial);
   print_endline "You have betted "; print_endline (chips); print_endline
   "chips"; *)

exception Unknown_col of string
exception Unknown_chips of string

type chip_to_check = { mutable chip_list : int list }

let init_chip_list = { chip_list = [] }
let curr_state_roulette = ref RouletteState.init_state

let text_color col =
  match col with
  | "blue" -> ANSITerminal.blue
  | "cyan" -> ANSITerminal.cyan
  | "green" -> ANSITerminal.green
  | "red" -> ANSITerminal.red
  | _ -> raise (Unknown_col col)

let bet_check_color_printer str =
  let print = ANSITerminal.print_string in
  print [ text_color "blue" ] str;
  read_line ()

let bet_check str =
  let print = ANSITerminal.print_string in
  print [ text_color "blue" ] str;
  int_of_string (read_line ())

let bet_check_color () =
  bet_check_color_printer
    "\nWhat color would you like to bet on? (red/black))\n"

let bet_check_num () =
  bet_check "\nWhat number would you like to bet on? (Enter a number 1-36)\n"

let bet_check_parity () =
  bet_check
    "\n\
     Would you like to bet on even or odd (Enter 1 for odd/Enter 2 for even)\n"

let bet_check_range2 () =
  bet_check
    "\n\
     Would you like to bet on the first range (number values 0-18) or the \
     second range (number values 19-36) (Enter 1 for the first range/ Enter 2 \
     for the second range)\n"

let bet_check_range3 () =
  bet_check
    "\n\
     This time, there are three ranges. Would you like to bet on the first \
     range (numers values 0-12), the second range (number values 13-24), or \
     third range (number values 25-36) (Enter 1 for the first range/Enter 2 \
     for the second range/Enter 3 for the third range)\n"

let bet_check_row () =
  bet_check
    "\n\
     What row would you like to bet on? The first row is the first 12 numbers, \
     the second row is the middle 12 numbers and the third row is the last 12 \
     numbers. (Enter 1 for the first row/Enter 2 for the second row/Enter 3 \
     for the third row))\n"

let bet_check_col () =
  bet_check
    "\n\
    \ What column would you like to bet one. Each column represents the three \
     numbers within the number of column you choose. (Enter a number 1-12 \
     inclusive) \n"

let chip_add c =
  let new_chip = c :: init_chip_list.chip_list in
  init_chip_list.chip_list <- new_chip

let update_bet_list str roulette_fun check_fun =
  let print = ANSITerminal.print_string in
  print [ text_color "blue" ] str;
  match read_int_opt () with
  | Some c ->
      if c > 0 then (
        curr_state_roulette := RouletteState.bet_list (roulette_fun check_fun);
        chip_add c)
      else ()
  | None -> raise (Unknown_chips "invalid_chips")

let play_roulette () =
  let print = ANSITerminal.print_string in
  print
    [ text_color "blue" ]
    "\n\n\
    \  /$$$$$$$  /$$                           /$$$$$$$                      \
     /$$             /$$     /$$              \n\
    \  | $$__  $$| $$                          | $$__  $$                    | \
     $$            | $$    | $$              \n\
    \  | $$  \\ $$| $$  /$$$$$$  /$$   /$$      | $$  \\ $$  /$$$$$$  /$$   \
     /$$| $$  /$$$$$$  /$$$$$$ /$$$$$$    /$$$$$$ \n\
    \  | $$$$$$$/| $$ |____  $$| $$  | $$      | $$$$$$$/ /$$__  $$| $$  | $$| \
     $$ /$$__  $$|_  $$_/|_  $$_/   /$$__  $$\n\
    \  | $$____/ | $$  /$$$$$$$| $$  | $$      | $$__  $$| $$  \\ $$| $$  | \
     $$| $$| $$$$$$$$  | $$    | $$    | $$$$$$$$\n\
    \  | $$      | $$ /$$__  $$| $$  | $$      | $$  \\ $$| $$  | $$| $$  | \
     $$| $$| $$_____/  | $$ /$$| $$ /$$| $$_____/\n\
    \  | $$      | $$|  $$$$$$$|  $$$$$$$      | $$  | $$|  $$$$$$/|  $$$$$$/| \
     $$|  $$$$$$$  |  $$$$/|  $$$$/|  $$$$$$$\n\
    \  |__/      |__/ \\_______/ \\____  $$      |__/  |__/ \\______/  \
     \\______/ |__/ \\_______/   \\___/   \\___/   \\_______/\n\
    \                           /$$  | \
     $$                                                                              \n\
    \                          |  \
     $$$$$$/                                                                              \n\
    \                           \\______/           \n\
    \  \n";

  print [ text_color "blue" ] "\nHere is the betting board.\n";
  Game.Roulette.print_square_list ();
  print
    [ text_color "blue" ]
    "\nWe will start off my making allowing you to make your bets. \n";
  update_bet_list
    "\nHow many chips would you like to bet on a color? (0 -> no bet) \n"
    RouletteState.bet_color (bet_check_color ());
  update_bet_list
    "\nHow many chips would you like to bet on a number? (0 -> no bet) \n"
    RouletteState.bet_num (bet_check_num ());
  update_bet_list
    "\nHow many chips would you like to bet on Even or Odd? (0 -> no bet) \n"
    RouletteState.bet_parity (bet_check_parity ());
  update_bet_list
    "\nHow many chips would you like to bet on range2? (0 -> no bet) \n"
    RouletteState.bet_range2 (bet_check_range2 ());
  update_bet_list
    "\nHow many chips would you like to bet on range3? (0 -> no bet) \n"
    RouletteState.bet_range3 (bet_check_range3 ());
  update_bet_list
    "\n How many chips would you like to bet on a row? (0 -> no bet) \n"
    RouletteState.bet_row (bet_check_row ());
  update_bet_list
    "\n How many chips would you like to bet on a column? (0 -> no bet) \n"
    RouletteState.bet_col (bet_check_col ())

let rec print_bets (str : string) bets =
  match bets with
  | [] -> ()
  | h :: t ->
      print_endline (RouletteState.to_string str h);
      print_bets str t

let update_split_bets (curr_state : State.t ref)
    (curr_global_state : GlobalState.t ref) =
  for x = 0 to Game.State.final_list_length !curr_state do
    match State.get_nth_hand_final !curr_state x with
    | h, y, b ->
        let _ = GlobalState.update_bets !curr_global_state y in
        ()
  done

let find_winner_split_hit (starting_chips : int ref) (lost : int ref)
    (won : int ref) (curr_state : State.t ref)
    (curr_global_state : GlobalState.t ref) =
  for x = 0 to Game.State.final_list_length !curr_state do
    match
      State.find_winner !curr_state (State.get_nth_hand_final !curr_state x)
    with
    | "Dealer" ->
        print_endline ("Dealer Wins hand number " ^ string_of_int (x + 1));
        let bet = State.get_bet (State.get_nth_hand_final !curr_state x) in
        let _ = State.update_chips !curr_state (!starting_chips - bet) in
        let _ =
          GlobalState.update_chips !curr_global_state (!starting_chips - bet)
        in
        let _ = lost := !lost + bet in
        let _ = GlobalState.update_money_lost !curr_global_state !lost in
        let _ = starting_chips := !starting_chips - bet in
        print_endline ("You lost " ^ string_of_int bet ^ " chips")
    | "Player" ->
        print_endline ("Player Wins hand number " ^ string_of_int (x + 1));
        let bet = State.get_bet (State.get_nth_hand_final !curr_state x) in
        let _ = State.update_chips !curr_state (!starting_chips + bet) in
        let _ =
          GlobalState.update_chips !curr_global_state (!starting_chips + bet)
        in
        let _ = won := !won + bet in

        let _ = GlobalState.update_money_won !curr_global_state !won in
        let _ = starting_chips := !starting_chips + bet in
        print_endline ("You won " ^ string_of_int bet ^ " chips")
    | _ -> print_endline ("Draw on hand number " ^ string_of_int (x + 1))
  done

let report (lost : int) (won : int) (bjgames : int) (boolean : bool)
    (chips : int) =
  print_endline ("You lost " ^ string_of_int lost ^ " chips");
  print_endline ("You won " ^ string_of_int won ^ " chips");
  print_endline ("You left this game with " ^ string_of_int chips);
  if boolean then
    print_endline ("You played " ^ string_of_int bjgames ^ " game of Blackjack")
  else
    print_endline ("You played " ^ string_of_int bjgames ^ " games of Blackjack")

let play_bj (starting_chips : int ref) (curr_global_state : GlobalState.t ref) =
  print_endline "How many chips would you like in the Casino?";
  let wanna_play = ref true in
  while !starting_chips > 0 && !wanna_play = true do
    let curr_state = ref (Game.State.init_state !starting_chips) in

    ANSITerminal.print_string
      [ text_color "red" ]
      "\n\n\
      \    /$$$$$$$  /$$                           /$$$$$$$  \
       /$$                     /$$                               /$$      \n\
      \    | $$__  $$| $$                          | $$__  $$| \
       $$                    | $$                              | $$      \n\
      \    | $$  \\ $$| $$  /$$$$$$  /$$   /$$      | $$  \\ $$| $$  /$$$$$$   \
       /$$$$$$$| $$   /$$ /$$  /$$$$$$   /$$$$$$$| $$   /$$\n\
      \    | $$$$$$$/| $$ |____  $$| $$  | $$      | $$$$$$$ | $$ |____  $$ \
       /$$_____/| $$  /$$/|__/ |____  $$ /$$_____/| $$  /$$/\n\
      \    | $$____/ | $$  /$$$$$$$| $$  | $$      | $$__  $$| $$  /$$$$$$$| \
       $$      | $$$$$$/  /$$  /$$$$$$$| $$      | $$$$$$/ \n\
      \    | $$      | $$ /$$__  $$| $$  | $$      | $$  \\ $$| $$ /$$__  $$| \
       $$      | $$_  $$ | $$ /$$__  $$| $$      | $$_  $$ \n\
      \    | $$      | $$|  $$$$$$$|  $$$$$$$      | $$$$$$$/| $$|  $$$$$$$|  \
       $$$$$$$| $$ \\  $$| $$|  $$$$$$$|  $$$$$$$| $$ \\  $$\n\
      \    |__/      |__/ \\_______/\\____  $$       |_______/ |__/ \\_______/ \
       \\_______/|__/  \\__/| $$ \\_______/ \\_______/|__/  \\__/\n\
      \                             /$$  | \
       $$                                             /$$  | \
       $$                              \n\
      \                            |  \
       $$$$$$/                                            |  \
       $$$$$$/                              \n\
      \                             \
       \\______/                                              \
       \\______/                \n\
      \    \n\
      \    Are you ready to play? (Y/N)\n";
    print_string "> ";
    match read_line () with
    | "Y" | "y" | "Yes" | "yes" -> (
        print_endline "Yay!";
        let _ =
          GlobalState.update_bj_games !curr_global_state
            (GlobalState.get_bj_games !curr_global_state + 1)
        in
        print_endline "How many chips would you like to bet per hand?";

        let bet_init = read_line () in
        let cards =
          Game.State.get_cards_from_list (State.get_no_split_hands !curr_state)
        in
        Game.Blackjack.print_cards cards;
        let _ =
          curr_state :=
            State.update_bet !curr_state (int_of_string bet_init)
              (List.hd (State.get_no_split_hands !curr_state))
        in
        print_endline ("You have betted " ^ bet_init ^ " chips");
        Random.self_init ();
        print_endline "Would you like to surrender? (Y/N)";
        let lost = ref 0 in
        let won = ref 0 in
        match read_line () with
        | "Y" | "y" | "Yes" | "yes" -> (
            let _ =
              curr_state :=
                State.surrender !curr_state
                  (State.get_first_no_split_hand !curr_state)
            in
            let _ = Unix.sleep 1 in
            print_endline "Dealer wins!";
            let b = State.get_bet (State.get_first_final_hand !curr_state) in
            let _ = State.update_chips !curr_state (!starting_chips - b) in
            let _ =
              GlobalState.update_chips !curr_global_state (!starting_chips - b)
            in
            let _ = lost := !lost + b in

            let _ = GlobalState.update_money_lost !curr_global_state !lost in
            let _ = starting_chips := !starting_chips - b in
            print_endline ("You lost " ^ string_of_int b ^ " chips");
            print_endline
              ("You have "
              ^ string_of_int (State.get_chips !curr_state)
              ^ " chips");
            print_endline "Would you like to play again? (Y/N)";
            match read_line () with
            | "Y" | "y" | "Yes" | "yes" -> wanna_play := true
            | _ ->
                let _ = wanna_play := false in
                ())
        | _ -> (
            print_endline "Would you like to double? (Y/N)";
            let hand = List.hd (State.get_no_split_hands !curr_state) in
            match read_line () with
            | "Y" | "y" | "Yes" | "yes" -> (
                (if Game.State.check_double !curr_state hand then (
                 let _ = curr_state := Game.State.double !curr_state hand in
                 while
                   Blackjack.sum_of_cards 0 (State.get_dealer_cards !curr_state)
                   < 16
                 do
                   print_endline "Dealer Hit";
                   curr_state := State.dealer_hit !curr_state;
                   Blackjack.print_cards (State.get_dealer_cards !curr_state);
                   let sum =
                     Blackjack.sum_of_cards 0
                       (State.get_dealer_cards !curr_state)
                   in
                   print_endline (string_of_int sum);
                   Unix.sleep 1
                 done;
                 match
                   State.find_winner !curr_state
                     (State.get_first_no_split_hand !curr_state)
                 with
                 | "Dealer" ->
                     let b =
                       State.get_bet (State.get_first_no_split_hand !curr_state)
                     in
                     let _ =
                       GlobalState.update_chips !curr_global_state
                         (!starting_chips - b)
                     in
                     print_endline "Dealer Wins!"
                 | "Player" ->
                     let b =
                       State.get_bet (State.get_first_no_split_hand !curr_state)
                     in
                     let _ =
                       GlobalState.update_chips !curr_global_state
                         (!starting_chips + b)
                     in
                     print_endline "Player Wins"
                 | _ -> print_endline "Draw")
                else
                  let _ = print_endline "Not enough chips" in
                  let _ =
                    curr_state := Game.State.player_hit !curr_state hand
                  in
                  let _ = print_endline "Player turnover" in
                  while
                    Blackjack.sum_of_cards 0
                      (State.get_dealer_cards !curr_state)
                    < 16
                  do
                    print_endline "Dealer Hit";
                    curr_state := State.dealer_hit !curr_state;
                    Blackjack.print_cards (State.get_dealer_cards !curr_state);
                    let sum =
                      Blackjack.sum_of_cards 0
                        (State.get_dealer_cards !curr_state)
                    in
                    print_endline (string_of_int sum);
                    Unix.sleep 1
                  done;
                  match
                    State.find_winner !curr_state
                      (State.get_first_no_split_hand !curr_state)
                  with
                  | "Dealer" ->
                      let b =
                        State.get_bet
                          (State.get_first_no_split_hand !curr_state)
                      in
                      let _ =
                        GlobalState.update_chips !curr_global_state
                          (!starting_chips - b)
                      in
                      print_endline "Dealer Wins!"
                  | "Player" ->
                      let b =
                        State.get_bet
                          (State.get_first_no_split_hand !curr_state)
                      in
                      let _ =
                        GlobalState.update_chips !curr_global_state
                          (!starting_chips + b)
                      in
                      print_endline "Player Wins"
                  | _ -> print_endline "Draw");
                print_endline "Would you like to play Blackjack again? (Y/N)";
                match read_line () with
                | "Y" | "y" | "Yes" | "yes" -> wanna_play := true
                | _ -> wanna_play := false)
            | _ -> (
                if Game.State.is_splittable hand then
                  curr_state := Game.State.move_to_split !curr_state
                else ();

                while Game.State.is_split_empty !curr_state <> true do
                  print_endline "Would you like to split?";
                  match read_line () with
                  | "Y" | "y" | "Yes" | "yes" ->
                      curr_state := Game.State.split !curr_state hand
                  | _ ->
                      curr_state := Game.State.move_to_no_split !curr_state hand
                done;

                for x = 0 to Game.State.no_split_length !curr_state do
                  print_endline ("This is hand " ^ string_of_int (x + 1));
                  Blackjack.print_cards
                    (get_hand (State.get_nth_hand !curr_state x));
                  curr_state :=
                    State.player_hit !curr_state
                      (Game.State.get_nth_hand !curr_state x)
                done;
                print_endline "Player turnover";
                Blackjack.print_cards (State.get_dealer_cards !curr_state);
                while
                  Blackjack.sum_of_cards 0 (State.get_dealer_cards !curr_state)
                  < 16
                do
                  print_endline "Dealer Hit";
                  curr_state := State.dealer_hit !curr_state;
                  Blackjack.print_cards (State.get_dealer_cards !curr_state);
                  let sum =
                    Blackjack.sum_of_cards 0
                      (State.get_dealer_cards !curr_state)
                  in

                  print_endline (string_of_int sum);
                  Unix.sleep 1
                done;
                update_split_bets curr_state curr_global_state;
                find_winner_split_hit starting_chips lost won curr_state
                  curr_global_state;
                print_endline
                  ("You have "
                  ^ string_of_int (State.get_chips !curr_state)
                  ^ " chips");
                print_endline "Would you like to play Blackjack again?";
                match read_line () with
                | "Y" | "y" | "Yes" | "yes" -> wanna_play := true
                | _ -> wanna_play := false))
        | _ ->
            print_endline "Aw, shucks! :(";
            exit 0)
    | _ -> ()
  done;
  report
    (GlobalState.get_lost !curr_global_state)
    (GlobalState.get_won !curr_global_state)
    (GlobalState.get_bj_games !curr_global_state)
    (GlobalState.get_bj_games !curr_global_state = 1)
    (GlobalState.get_chips !curr_global_state)

let main () =
  ANSITerminal.print_string
    [ text_color "cyan" ]
    "\n\
    \  \n\
    \  /$$      /$$           \
     /$$                                                     \
     /$$                      /$$$$$$                      \
     /$$                    \n\
    \  | $$  /$ | $$          | \
     $$                                                    | \
     $$                     /$$__  $$                    \
     |__/                    \n\
    \  | $$ /$$$| $$  /$$$$$$ | $$  /$$$$$$$  /$$$$$$  /$$$$$$/$$$$   \
     /$$$$$$        /$$$$$$    /$$$$$$       | $$  \\__/  /$$$$$$   /$$$$$$$ \
     /$$ /$$$$$$$   /$$$$$$ \n\
    \  | $$/$$ $$ $$ /$$__  $$| $$ /$$_____/ /$$__  $$| $$_  $$_  $$ /$$__  \
     $$      |_  $$_/   /$$__  $$      | $$       |____  $$ /$$_____/| $$| \
     $$__  $$ /$$__  $$\n\
    \  | $$$$_  $$$$| $$$$$$$$| $$| $$      | $$  \\ $$| $$ \\ $$ \\ $$| \
     $$$$$$$$        | $$    | $$  \\ $$      | $$        /$$$$$$$|  $$$$$$ | \
     $$| $$  \\ $$| $$  \\ $$\n\
    \  | $$$/ \\  $$$| $$_____/| $$| $$      | $$  | $$| $$ | $$ | $$| \
     $$_____/        | $$ /$$| $$  | $$      | $$    $$ /$$__  $$ \\____  $$| \
     $$| $$  | $$| $$  | $$\n\
    \  | $$/   \\  $$|  $$$$$$$| $$|  $$$$$$$|  $$$$$$/| $$ | $$ | $$|  \
     $$$$$$$        |  $$$$/|  $$$$$$/      |  $$$$$$/|  $$$$$$$ /$$$$$$$/| \
     $$| $$  | $$|  $$$$$$/\n\
    \  |__/     \\__/ \\_______/|__/ \\_______/ \\______/ |__/ |__/ |__/ \
     \\_______/         \\___/   \\______/        \\______/  \
     \\_______/|_______/ |__/|__/  |__/ \\______/ \n\
    \  \n";
  ANSITerminal.print_string [ text_color "cyan" ] "\nWelcome to OCaml Casino!\n";
  print_endline "How many chips would you like in the casino?";
  let starting_chips = ref (int_of_string (read_line ())) in
  let curr_global_state = ref (GlobalState.init_state !starting_chips) in
  ANSITerminal.print_string [ text_color "cyan" ] "Would you like to play ";
  ANSITerminal.print_string [ text_color "green" ] "Blackjack (B)";
  ANSITerminal.print_string [ text_color "cyan" ] " or ";
  ANSITerminal.print_string [ text_color "green" ] "Roulette (R)? ";
  match read_line () with
  | "R" ->
      play_roulette ();
      print_endline "";
      print_endline "Summary of bets made:";
      print_bets "The predicted " (RouletteState.get_bets !curr_state_roulette);
      RouletteState.bets_won_after_bets
        (RouletteState.get_bets !curr_state_roulette);
      print_endline "";
      print_endline "Winning square:";
      print_endline
        (RouletteState.to_string "The actual " RouletteState.win_num_bet);
      print_endline
        (RouletteState.to_string "The actual " RouletteState.win_color_bet);
      print_endline
        (RouletteState.to_string "The actual " RouletteState.win_range2_bet);
      print_endline
        (RouletteState.to_string "The actual " RouletteState.win_range3_bet);
      print_endline
        (RouletteState.to_string "The actual " RouletteState.win_parity_bet);
      print_endline
        (RouletteState.to_string "The actual " RouletteState.win_row_bet);
      print_endline
        (RouletteState.to_string "The actual " RouletteState.win_col_bet);
      print_endline "";
      print_endline "The bets that you won are:";
      print_bets "The winning "
        (RouletteState.get_bets_won !curr_state_roulette);
      RouletteState.chips_after_bets !curr_state_roulette
        init_chip_list.chip_list
        (RouletteState.get_bets !curr_state_roulette);
      print_endline
        (string_of_int (RouletteState.get_chip_amt !curr_state_roulette))
  | "B" -> play_bj starting_chips curr_global_state
  | _ -> ()

let () = main ()
