open Minimarket

let is_good_user email pwd = function
  | {Market.email=e;Market.password=p; _}
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
  )

let print_balance (user : Market.user) =
  Printf.printf "Balance: %d\n" user.balance

let rec prompt users (user : Market.user) ob =
  print_string "Type your order ('exit' to quit the program).\n";
  print_string "> ";
  let input = read_line () in
  begin match String.split_on_char ' ' input with
  | ["buy"; "limit"; value; size; asset] ->
      let dt = Datetime.current_datetime () in
      let nvalue = int_of_string value in
      let nsize = int_of_string size in
      if user.balance >= nvalue*nsize then
        let o : Market.order =
        { asset_code = asset
        ; ordtype = Market.Limit nvalue
        ; size = nsize
        ; t = Datetime.datetime_of_string dt 
        ; user_id = user.id
        } in 
        let new_ob = o::ob in
        let new_bal = user.balance-nvalue*nsize in
        Market.save_orderbook new_ob;
        prompt users {user with balance=new_bal} new_ob
      else
        (print_string "Insufficient balance on your account.\n";
        prompt users user ob)
  | ["buy"; "market"; _size; _asset] -> failwith "Not implemented."
  | ["balance"] -> print_balance user; prompt users user ob
  | ["disconnect"] ->
    let new_user = ask_login users in
    prompt users new_user ob 
  | ["exit"] -> print_string "Disconnected.\n"
  | _ -> print_string "Invalid command"; prompt users user ob
  end

let welcome (user : Market.user) =
  Printf.printf "Connected as %s.\n" user.username;
  print_balance user

let () = 
  print_string "Market open.\n";
  let users = Market.load_users () in
  let orderbook = Market.load_orderbook () in
  let user = ask_login users in
  welcome user;
  prompt users user orderbook
