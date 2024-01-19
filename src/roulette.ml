type square = {
  mutable value : int;
  mutable color : string;
}

let init_list = Array.init 37 (fun i -> { value = i; color = "Black" })
let duplicate_list = Array.init 37 (fun i -> ~-i - 1)
let dlist_iter = ref 0

let rec add_square () : int =
  Random.self_init ();
  let num = Random.int 37 in
  if Array.mem num duplicate_list && duplicate_list.(36) = ~-37 then
    add_square ()
  else (
    duplicate_list.(!dlist_iter) <- num;
    dlist_iter := !dlist_iter + 1;
    num)

let create_list (list : square array) =
  for x = 0 to Array.length list - 1 do
    list.(x).value <- add_square ();
    if x mod 2 = 0 then list.(x).color <- "Red"
  done

let win_sqr () =
  Random.self_init ();
  init_list.(Random.int 36)

let print_row (list : square array) (start : int) =
  let magenta = ANSITerminal.magenta in
  let red = ANSITerminal.red in
  let black = ANSITerminal.black in
  for x = start to start + 11 do
    ANSITerminal.print_string [ magenta ] "| ";
    if list.(x).color = "Red" then
      ANSITerminal.print_string [ red ]
        (if list.(x).value < 10 then "0" ^ string_of_int list.(x).value ^ " "
        else string_of_int list.(x).value ^ " ")
    else
      ANSITerminal.print_string [ black ]
        (if list.(x).value < 10 then "0" ^ string_of_int list.(x).value ^ " "
        else string_of_int list.(x).value ^ " ")
  done;
  ANSITerminal.print_string [ magenta ] "|";
  print_endline "";
  ANSITerminal.print_string [ magenta ]
    "+----+----+----+----+----+----+----+----+----+----+----+----+";
  print_endline ""

let print_square_list () =
  let print_magenta = ANSITerminal.magenta in
  let print_yellow = ANSITerminal.yellow in
  create_list init_list;
  ANSITerminal.print_string [ print_magenta ]
    "                         +----+----+";
  print_endline "";
  ANSITerminal.print_string [ print_magenta ] "                         |";
  let num = string_of_int init_list.(36).value in

  ANSITerminal.print_string [ print_yellow ]
    (if init_list.(36).value < 10 then "   0 " ^ num
    else "   " ^ String.sub num 0 1 ^ " " ^ String.sub num 1 1);

  ANSITerminal.print_string [ print_magenta ] "   |";
  print_endline "";
  ANSITerminal.print_string [ print_magenta ]
    "+----+----+----+----+----+----+----+----+----+----+----+----+";
  print_endline "";
  print_row init_list 0;
  print_row init_list 12;
  print_row init_list 24

let create_specific_square (value : int) (col : string) =
  let square = { value; color = col } in
  square

let get_value (square : square) = square.value
let get_color square = square.color