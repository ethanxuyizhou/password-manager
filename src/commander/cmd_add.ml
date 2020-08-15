open! Core
open! Async
open Sentry_lib

let command =
  Command.async_or_error ~summary:""
    (let%map_open.Command () = return () in
     fun () ->
       let%bind user = Interactive.ask_user "Choose a username:" in
       let%bind master_password =
         Interactive.ask_user "Please choose and type in your master password:"
       in
       let%bind master_password' =
         Interactive.ask_user "Please retype the password again:"
       in
       match String.equal master_password master_password' with
       | false -> Deferred.Or_error.errorf "Passwords did not match"
       | true -> (
           match%bind Async_interactive.ask_yn "Confirm?" with
           | false -> Deferred.Or_error.ok_unit
           | true ->
               Sentry_server_connection.with_close ~f:(fun connection ->
                   Rpc.Rpc.dispatch_exn Sentry_rpcs.add_user_v1 connection
                     { Sentry_rpcs.User_and_password.user; master_password }) ))
