(* Testing Plan for BlackJack: Our rendition of BlackJack includes many helper
   functions to carryout out the casino rules of the popular casino game. To
   test the blackjack game system we tested two sections, the blackjack rule
   functions as well as the functions that update the game state during the
   rounds of the game

   Blackjack Module and Rule Functions: Hit, Stay, Double, Surrender, Split.
   These functions are used to change cards and bet sizes based on user choice.
   For each of these functions we used glassbox testing to test the different
   possible combinations of inputs on different hands/bet sizes

   State Module Functions: Update Bet, Update Final List, and all other
   functions in state that take in a state and return a state were necessary to
   keep track of game changes within an individual round of the game. To test
   these functions, we created different states and tested the functions using
   blackbox testing. *)

(** Testing Plan for Roulette Game: 1) OUnit is used to test all of the
    functions in Roulette.ml and part of the functions in RouletteState.ml. The
    functions in RouletteState.ml that deal with handling the winning square and
    the betting are manually tested via terminal by calling make play.

    2) Both Roulette and RouletteState modules were tested by OUnit. The test
    cases were developed via black box testing.

    3) Black box testing was the most appropriate for our project given the
    scope and constraints. By looking at the specification of each function, we
    were able to develop test cases that tackle the different forms of possible
    inputs and also test the boundaries. *)

open OUnit2
open Game
open Blackjack
open Roulette
open RouletteState
open State

let string_of_string (s : string) = s
let square1 = Roulette.create_specific_square 1 "red"
let square2 = Roulette.create_specific_square 34 "black"
let list1 = Array.make 1 square1
let list2 = Array.make 10 square1

let add_square_test (name : string) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (let test = Roulette.add_square () in
     test < 37 && test > ~-1)
    ~printer:string_of_bool

let create_list_test (name : string) (list : square array) (index : int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  Roulette.create_list list;
  assert_equal expected_output
    (Roulette.get_value list.(index) < 37
    && Roulette.get_value list.(index) > ~-1)
    ~printer:string_of_bool

let win_sqr_test (name : string) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Roulette.get_value (Roulette.win_sqr ()) < 37
    && Roulette.get_value (Roulette.win_sqr ()) > ~-1)
    ~printer:string_of_bool

let bet1 = RouletteState.create_specific_bet 1 "Black" 1 1
let bet1_1 = RouletteState.create_specific_bet 1 "Red" 1 1
let bet2 = RouletteState.create_specific_bet 2 "Range3" 0 12
let bet2_1 = RouletteState.create_specific_bet 2 "Range3" 13 24
let bet2_2 = RouletteState.create_specific_bet 2 "Range3" 25 36
let bet3 = RouletteState.create_specific_bet 3 "Range2" 0 18
let bet3_1 = RouletteState.create_specific_bet 3 "Range2" 19 36
let bet4 = RouletteState.create_specific_bet 4 "Num" 1 1
let bet5 = RouletteState.create_specific_bet 5 "odd" 1 1
let bet5_1 = RouletteState.create_specific_bet 5 "even" 1 1
let bet6 = RouletteState.create_specific_bet 6 "Row" 1 1
let bet6_1 = RouletteState.create_specific_bet 6 "Row" 2 1
let bet6_2 = RouletteState.create_specific_bet 6 "Row" 3 1
let bet7 = RouletteState.create_specific_bet 7 "Col" 1 1
let bet7_1 = RouletteState.create_specific_bet 7 "Col" 2 1
let bet7_2 = RouletteState.create_specific_bet 7 "Col" 3 1
let bet7_3 = RouletteState.create_specific_bet 7 "Col" 4 1
let bet7_4 = RouletteState.create_specific_bet 7 "Col" 5 1
let bet7_5 = RouletteState.create_specific_bet 7 "Col" 6 1
let bet7_6 = RouletteState.create_specific_bet 7 "Col" 7 1
let bet7_7 = RouletteState.create_specific_bet 7 "Col" 8 1
let bet7_8 = RouletteState.create_specific_bet 7 "Col" 9 1
let bet7_9 = RouletteState.create_specific_bet 7 "Col" 10 1
let bet7_10 = RouletteState.create_specific_bet 7 "Col" 11 1
let bet7_11 = RouletteState.create_specific_bet 7 "Col" 12 1
let state1 = RouletteState.create_specific_state 1 [ bet1 ] square1
let state2 = RouletteState.create_specific_state 1 [ bet1; bet2 ] square1
let state3 = RouletteState.create_specific_state 1 [ bet2 ] square1

let state4 =
  RouletteState.create_specific_state 1
    [ bet1; bet2; bet3; bet4; bet5; bet6; bet7 ]
    square1

let state5 = RouletteState.create_specific_state 1 [] square1
let state6 = RouletteState.create_specific_state 1 [ bet5; bet3; bet1 ] square1

let state7 =
  RouletteState.create_specific_state 1
    [ bet1; bet2; bet3; bet4; bet5; bet6 ]
    square1

let state8 =
  RouletteState.create_specific_state 300
    [ bet3; bet1; bet2; bet5; bet4 ]
    square1

let state9 =
  RouletteState.create_specific_state 300
    [
      bet2;
      bet7_10;
      bet7_9;
      bet6_2;
      bet2_2;
      bet4;
      bet5_1;
      bet7_11;
      bet7_9;
      bet7_8;
      bet7_7;
      bet7_6;
      bet7_5;
      bet7_4;
      bet1;
      bet3;
      bet5;
      bet6;
      bet7_2;
      bet7_1;
      bet6_1;
      bet1_1;
      bet2_1;
    ]
    square1

let init_bet_list_test (name : string) (expected_output : 'a list) : test =
  name >:: fun _ ->
  assert_equal expected_output (RouletteState.init_bet_list ())

let get_bets_test (name : string) (state : RouletteState.t)
    (expected_output : RouletteState.bet list) : test =
  name >:: fun _ -> assert_equal expected_output (RouletteState.get_bets state)

let bet_color_test (name : string) (col : string)
    (expected_output : RouletteState.bet) : test =
  name >:: fun _ -> assert_equal expected_output (RouletteState.bet_color col)

let bet_parity_test (name : string) (par : int)
    (expected_output : RouletteState.bet) : test =
  name >:: fun _ -> assert_equal expected_output (RouletteState.bet_parity par)

let bet_range2_test (name : string) (num : int)
    (expected_output : RouletteState.bet) : test =
  name >:: fun _ -> assert_equal expected_output (RouletteState.bet_range2 num)

let bet_range3_test (name : string) (num : int)
    (expected_output : RouletteState.bet) : test =
  name >:: fun _ -> assert_equal expected_output (RouletteState.bet_range3 num)

let bet_row_test (name : string) (num : int)
    (expected_output : RouletteState.bet) : test =
  name >:: fun _ -> assert_equal expected_output (RouletteState.bet_row num)

let bet_col_test (name : string) (num : int)
    (expected_output : RouletteState.bet) : test =
  name >:: fun _ -> assert_equal expected_output (RouletteState.bet_col num)

let roulette_state_tests =
  [
    init_bet_list_test
      "Init_bet_list test to check if starting value of the bet list is empty"
      [];
    get_bets_test
      "Get_bets test 1 to check the bet of state with bet on Color in the bet \
       list "
      state1 [ bet1 ];
    get_bets_test
      "Get_bets test 2 to check the bet of state with bet on Color and Range3 \
       in the bet list"
      state2 [ bet1; bet2 ];
    get_bets_test
      "Get_bets_test 3 to check the bet of state with bet on Range 3 in the \
       bet list"
      state3 [ bet2 ];
    get_bets_test
      "Get_bets test 4 to check the bet of state with bet on Color, Range3, \
       Range1, Number, Parity, Row, and Column in the bet list"
      state4
      [ bet1; bet2; bet3; bet4; bet5; bet6; bet7 ];
    get_bets_test
      "Get_bets test 5 to check the bet of state with empty bets in the bet \
       list"
      state5 [];
    get_bets_test
      "Get_bets test 6 to check the bet of state with bet on Parity, Range2, \
       and Color in the bet list"
      state6 [ bet5; bet3; bet1 ];
    get_bets_test
      "Get_bets test 4 to check the bet of state with bet on Color, Range3, \
       Range1, Number, Parity, and Row in the bet list"
      state7
      [ bet1; bet2; bet3; bet4; bet5; bet6 ];
    get_bets_test
      "Get_bets test 4 to check the bet of state with bet on Color, Range3, \
       Range1, Number, Parity, and Row in the bet list"
      state8
      [ bet3; bet1; bet2; bet5; bet4 ];
    get_bets_test
      "Get_bets test 4 to check the bet of state with bet on Range3, Column, \
       Row, Number, Range1, Parity, and Row in the bet list"
      state9
      [
        bet2;
        bet7_10;
        bet7_9;
        bet6_2;
        bet2_2;
        bet4;
        bet5_1;
        bet7_11;
        bet7_9;
        bet7_8;
        bet7_7;
        bet7_6;
        bet7_5;
        bet7_4;
        bet1;
        bet3;
        bet5;
        bet6;
        bet7_2;
        bet7_1;
        bet6_1;
        bet1_1;
        bet2_1;
      ];
    bet_color_test
      "Bet_color test 1 to check the color of a bet with Black Color" "black"
      bet1;
    bet_color_test "Bet_color test 2 to check the color of a bet with Red Color"
      "red" bet1_1;
    bet_parity_test
      "Bet_parity test 1 to check the parity of a bet with a parity of Odd" 1
      bet5;
    bet_parity_test
      "Bet_parity test 2 to check the parity of a bet with a parity of Even" 2
      bet5_1;
    bet_range2_test
      "Bet_range2 test 1 to check the range2 of a bet that is within range2 1" 1
      bet3;
    bet_range2_test
      "Bet_range2 test 2 to check the range2 of a bet that is within range2 1" 2
      bet3_1;
    bet_range3_test
      "Bet_range3 test 1 to check the range3 of a bet that is within range3 1" 1
      bet2;
    bet_range3_test
      "Bet_range3 test 2 to check the range3 of a bet that is within range3 1" 2
      bet2_1;
    bet_range3_test
      "Bet_range3 test 3 to check the range3 of a bet that is within range3 1" 3
      bet2_2;
    bet_row_test
      "Bet_row test 1 which checks the row of a bet for roulette numbers in \
       Row 1"
      1 bet6;
    bet_row_test
      "Bet_row test 1 which checks the row of a bet for roulette numbers in \
       Row 2"
      2 bet6_1;
    bet_row_test
      "Bet_row test 1 which checks the row of a bet for roulette numbers in \
       Row 3"
      3 bet6_2;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 1"
      1 bet7;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 2"
      2 bet7_1;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 3"
      3 bet7_2;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 4"
      4 bet7_3;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 5"
      5 bet7_4;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 6"
      6 bet7_5;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 7"
      7 bet7_6;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 8"
      8 bet7_7;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 9"
      9 bet7_8;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 10"
      10 bet7_9;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 11"
      11 bet7_10;
    bet_col_test
      "Bet_col test 1 which checks the column of a bet for roulette numbers in \
       Column 12"
      12 bet7_11;
  ]

let roulette_tests =
  [
    add_square_test
      "Add_square test to check if the value of the added square is within 0 \
       to 36"
      true;
    create_list_test
      "Create_list_test 1 to check if the added square with index 0 value is \
       within 0 to 36"
      list1 0 true;
    create_list_test
      "Create_list_test 2 to check if the added square with index 8 value is \
       within 0 to 36"
      list2 8 true;
    create_list_test
      "Create_list_test 3 to check if added square with index 9 value is \
       within 0 to 36"
      list2 9 true;
    create_list_test
      "Create_list_test 4 to check if added square with index 3 value is \
       within 0 to 36"
      list2 3 true;
    create_list_test
      "Create_list_test 5 to check if added square with index 1 value is \
       within 0 to 36"
      list2 1 true;
    create_list_test
      "Create_list_test 6 to check if added square with index 2 value is \
       within 0 to 36"
      list2 2 true;
    win_sqr_test
      "Win_sqr_test to check if the selected random winning square is within 0 \
       to 36"
      true;
  ]

(* open Game *)
let string_of_string (s : string) = s

let test_demo (name : string) : test =
  name >:: fun _ -> raise (Failure "A Fail")

let val_from_num_test (name : string) (input : int) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (Blackjack.val_from_num input)
    ~printer:string_of_int

let name_from_num_test (name : string) (input : int) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Blackjack.name_from_num input)
    ~printer:string_of_string

let suit_from_num_test (name : string) (input : int) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Blackjack.suit_from_num input)
    ~printer:string_of_string

let sum_of_cards_test (name : string) (input : Blackjack.card list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Blackjack.sum_of_cards 0 input)
    ~printer:string_of_int

let check_busted_test (name : string) (input : State.hand)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.check_busted input)
    ~printer:string_of_bool

let check_blackjack_test (name : string) (input : hand) (expected_output : bool)
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.check_blackjack input)
    ~printer:string_of_bool

let check_initial_state_chips_test (name : string) (state : State.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.get_chips state) ~printer:string_of_int

let check_state_hand_test (name : string) (state : State.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.length (get_hand (State.get_first_no_split_hand state)))
    ~printer:string_of_int

let check_hit_state_test (name : string) (c1 : card list) (c2 : card list)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.length c1 = List.length c2 - 1)
    ~printer:string_of_bool

let double_state_test (name : string) (hand : hand) (expected_output : bool) :
    test =
  name >:: fun _ ->
  let starting_val = get_bet hand in
  let final_val = get_bet (double_helper hand) in
  assert_equal expected_output
    (final_val = 2 * starting_val)
    ~printer:string_of_bool

let double_state_test_fail (name : string) (hand : hand)
    (expected_output : bool) : test =
  name >:: fun _ ->
  let starting_val = get_bet hand in
  let final_val = get_bet (double_helper hand) in
  assert_equal expected_output (final_val = starting_val)
    ~printer:string_of_bool

let is_splittable_test (name : string) (hand : hand) (expected_output : bool) :
    test =
  name >:: fun _ ->
  let splittable_output = is_splittable hand in
  assert_equal expected_output splittable_output ~printer:string_of_bool

let card1 = Blackjack.create_specific_card "Ace" 11 "Spades"
let card2 = Blackjack.create_specific_card "King" 10 "Spades"
let card3 = Blackjack.create_specific_card "9" 9 "Spades"
let card4 = Blackjack.create_specific_card "2" 2 "Hearts"
let card5 = Blackjack.create_specific_card "9" 9 "Spades"
let queen_spade = Blackjack.create_specific_card "Queen" 10 "Spades"
let king_spade = Blackjack.create_specific_card "King" 10 "Spades"
let queen_king_spade_cards = [ queen_spade ] @ [ king_spade ]
let queen_king_spade_hand = (queen_king_spade_cards, 100, false)
let blackjack_cards = [ card1 ] @ [ card2 ]
let blackjack_hand = ([ card1 ] @ [ card2 ], 100, false)
let blackjack_cards_no_ace = [ card2 ] @ [ card3 ] @ [ card4 ]
let blackjack_hand_no_ace = ([ card2 ] @ [ card3 ] @ [ card4 ], 100, false)
let split_cards = [ card3 ] @ [ card5 ]
let split_hand = (split_cards, 100, false)
let hand_unique_cards = ([ card3 ] @ [ card4 ], 100, false)
let hand_20 = [ card1 ] @ [ card2 ]
let busted_cards = card3 :: blackjack_cards_no_ace
let busted_hand = (card3 :: blackjack_cards_no_ace, 100, true)
let state1 = init_state 1000
let state1_init_hand = get_first_hand state1
let init_card_list_after_hit = hit_once_step state1

let blackjack_tests =
  [
    val_from_num_test "testing number 1" 1 1;
    val_from_num_test
      "testing number between 2 and 9 and seeing if value is returned from \
       given integer"
      2 2;
    val_from_num_test
      "testing number between 2 and 9 and seeing if value is returned from \
       given integer"
      3 3;
    val_from_num_test
      "testing number between 2 and 9 and seeing if value is returned from \
       given integer"
      4 4;
    val_from_num_test
      "testing number between 2 and 9 and seeing if value is returned from \
       given integer"
      5 5;
    val_from_num_test
      "testing number between 2 and 9 and seeing if value is returned from \
       given integer"
      6 6;
    val_from_num_test
      "testing number between 2 and 9 and seeing if value is returned from \
       given integer"
      7 7;
    val_from_num_test
      "testing number between 2 and 9 and seeing if value is returned from \
       given integer"
      8 8;
    val_from_num_test
      "testing number between 2 and 9 and seeing if value is returned from \
       given integer"
      9 9;
    val_from_num_test "testing number greater than 9" 11 10;
    val_from_num_test "testing boundary case 0" 0 0;
    val_from_num_test "testing boundary case 9" 9 9;
    val_from_num_test "testing number less than 0" (-1) 0;
    name_from_num_test
      "testing boundary case 1 for extracting name of card from given integer" 1
      "Ace";
    name_from_num_test "testing boundary case 9" 9 "9";
    name_from_num_test
      "testing integer value 10 for extracting name of card from given integer"
      10 "Jack";
    name_from_num_test
      "testing integer value 11 for extracting name of card from given integer"
      11 "Queen";
    name_from_num_test
      "testing integer value 12 for extracting name of card from given integer"
      12 "King";
    name_from_num_test
      "testing to see if value 1 returns corresponding card name (ace)" 1 "Ace";
    name_from_num_test
      "testing to see if any value other than 2-12 returns corresponding card \
       name (ace)"
      0 "Ace";
    suit_from_num_test "testing value zero to find corresponding suit (spade)" 0
      "Spades";
    suit_from_num_test "testing value one to find corresponding suit (heart)" 1
      "Hearts";
    suit_from_num_test "testing value two to find corresponding suit (club)" 2
      "Clubs";
    suit_from_num_test
      "testing value three to find corresponding suit (diamond)" 3 "Diamonds";
    sum_of_cards_test
      "testing blackjack with an Ace to see if sum of cards correctly \
       calculates to 21"
      blackjack_cards 21;
    sum_of_cards_test
      "testing hand that should equal blackjack without an Ace to see if cards \
       sum correctly"
      blackjack_cards_no_ace 21;
    sum_of_cards_test
      "testing boundary case with busted hand to see if sum of cards function \
       accurately calculutes sums over 21"
      busted_cards 30;
    check_blackjack_test
      "testing a blackjack hand in blackjack checker helper function"
      blackjack_hand true;
    check_blackjack_test "testing boundary case blackjack hand on busted hand "
      busted_hand false;
    check_busted_test
      "testing truly busted hand in check_busted helper function" busted_hand
      true;
    check_busted_test "testing non-busted hand in check_busted helper function"
      blackjack_hand false;
    check_initial_state_chips_test
      "testing to see if chips is initialized correctly" state1 1000;
    check_state_hand_test
      "Checking to see if initial hand is properly set to equal a length of 2"
      state1 2;
    check_hit_state_test
      "Checking to see if hit function correctly added a singular card to the \
       player's hand"
      (get_hand state1_init_hand)
      init_card_list_after_hit true;
    double_state_test
      "Testing functionality of double helper in state with hand of different \
       values"
      hand_unique_cards true;
    double_state_test
      "Testing functionality of double helper in state with hand of same values"
      split_hand true;
    double_state_test
      "Testing functionality of double helper in state with hand of same \
       values but different suits"
      hand_unique_cards true;
    double_state_test_fail
      "Testing functionality of double helper in state with hand of different \
       values"
      blackjack_hand true;
    is_splittable_test
      "Testing functionality of the is split helper function in state with \
       hand that is not splittable ( different values for cards)"
      hand_unique_cards false;
    is_splittable_test
      "Testing functionality of the is split helper function in state with \
       hand that is truly spittable ( same values for both cards)"
      split_hand true;
    is_splittable_test
      "Testing functionality of the is split helper function in state with \
       hand that is not truly spittable ( same values different name for both \
       cards)"
      queen_king_spade_hand false;
  ]

let suite =
  "black jack test suite"
  >::: List.flatten [ roulette_tests; roulette_state_tests; blackjack_tests ]

let _ = run_test_tt_main suite