open Minimarket

let is_good_user email pwd = function
  | {Market.email=e;Market.password=p}
    when e=email && p=pwd -> true
  | _ -> false 

let rec ask_login users =
  print_string "Enter your email: ";
  let email = read_line () in
  print_string "Enter your password: ";
  let pwd = read_line () in
  try List.find (is_good_user email pwd) users
  with _ -> (
    print_string "No user found.\n";
    ask_login users
  );
  print_string "Connected.\n"

let () =
  print_string "Market open.\n";
  let users = Market.load_users () in
  let orderbook = Market.load_orderbook () in
  let user = ask_login users in
  ()
