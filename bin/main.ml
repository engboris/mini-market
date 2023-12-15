open Minimarket

let is_good_user email pwd = function
  | {User.email=e;User.password=p; _}
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

let print_balance (user : User.t) =
  Printf.printf "Balance: %d\n" user.balance

let rec prompt users (user : User.t) ob =
  print_string "Type your order ('exit' to quit the program).\n";
  print_string "> ";
  let input = read_line () in
  begin match String.split_on_char ' ' input with
  | ["buy"; "limit"; price; size; asset] ->
      let nprice = int_of_string price in
      let nsize = int_of_string size in
      if user.balance >= nprice*nsize then
        let o =
          Market.new_order
            Order.Buy
            (Order.Limit nprice)
            nsize
            asset
            user.id
        in
        let new_ob = o::ob in
        let new_bal = user.balance-nprice*nsize in
        Market.save_orderbook new_ob;
        prompt users {user with balance=new_bal} new_ob
      else
        (print_string "Insufficient balance on your account.\n";
        prompt users user ob)
  | ["sell"; "limit"; price; size; asset] -> failwith "TODO"
  | ["balance"] -> print_balance user; prompt users user ob
  | ["disconnect"] ->
    print_string "Disconnected.\n";
    let new_user = ask_login users in
    prompt users new_user ob 
  | ["exit"] -> print_string "Disconnected.\n"
  | _ -> print_string "Invalid command"; prompt users user ob
  end

let welcome (user : User.t) =
  Printf.printf "Connected as %s.\n" user.username;
  print_balance user

let () = 
  print_string "Market open.\n";
  let users = Market.load_users () in
  let orderbook = Market.load_orderbook () in
  let user = ask_login users in
  welcome user;
  prompt users user orderbook
