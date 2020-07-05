open! Core
open! Async
open Sentry_lib

module Arg = struct
  type t = {
    length : int;
    must_include_numbers : bool;
    must_include_uppercase_letters : bool;
    must_include_lowercase_letters : bool;
  }

  let parse () =
    let open Command.Let_syntax in
    let%map_open () = return ()
    and length =
      flag "length"
        (optional_with_default 32 int)
        ~doc:"LENGTH randomly generate a password of LENGTH (default to 32)"
    and must_include_numbers =
      flag "include-numbers" no_arg
        ~doc:
          "set to guarantee the generated password would contain at least one \
           number"
    and must_include_lowercase_letters =
      flag "include-lowercase-letters" no_arg ~doc:""
    and must_include_uppercase_letters =
      flag "include-uppercase-letters" no_arg ~doc:""
    in
    {
      length;
      must_include_numbers;
      must_include_lowercase_letters;
      must_include_uppercase_letters;
    }
end

let command =
  Command.async_or_error ~summary:""
    (let%map.Command args = Arg.parse () in
     fun () ->
       let open Deferred.Let_syntax in
       let%bind.Deferred.Or_error () =
         if args.length > 256 || args.length < 1 then
           Deferred.Or_error.errorf "Length cannot exceed 256 or be less than 1"
         else Deferred.Or_error.ok_unit
       in
       let%bind user = Interactive.ask_user "Enter your username:" in
       let%bind master_password =
         Interactive.ask_user "Please type in your master password:"
       in
       let%bind entry =
         Interactive.ask_user "Enter the name of the entry you'd like to add:"
       in
       let%bind.Deferred.Or_error entry_password =
         match%bind
           Async_interactive.ask_yn
             "Do you want a randomly generated password for you?"
         with
         | false ->
             Interactive.ask_user
               (sprintf "Please enter your password for entry %s" entry)
             |> Deferred.ok
         | true ->
             let bool_to_unit_option b = if b then Some () else None in
             Random.random_string ~length:args.length
               ?must_include_numbers:
                 (bool_to_unit_option args.must_include_numbers)
               ?must_include_uppercase_letters:
                 (bool_to_unit_option args.must_include_uppercase_letters)
               ?must_include_lowercase_letters:
                 (bool_to_unit_option args.must_include_lowercase_letters)
               ()
             |> Deferred.return
       in
       printf
         !"Please confirm that you'd like to use this password for entry %s: %s\n"
         entry entry_password;
       match%bind Async_interactive.ask_yn "Confirm?" with
       | false -> Deferred.Or_error.ok_unit
       | true ->
           Sentry_server_connection.with_close ~f:(fun connection ->
               Rpc.Rpc.dispatch_exn Sentry_rpcs.add_password_entry_v1 connection
                 {
                   Sentry_rpcs.Entry_info.user;
                   master_password;
                   entry;
                   entry_password;
                 }))
