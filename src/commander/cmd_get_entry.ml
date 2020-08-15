open! Core
open! Async
open Sentry_lib

let command =
  Command.async_or_error ~summary:""
    (let%map_open.Command () = return () in
     fun () ->
       let%bind user = Interactive.ask_user "Enter your username:" in
       let%bind master_password =
         Interactive.ask_user "Please type in your master password:"
       in
       let%bind entry =
         Interactive.ask_user
           "Enter the name of the password entry you'd like to access:"
       in
       match%bind
         Sentry_server_connection.with_close ~f:(fun connection ->
             Rpc.Rpc.dispatch_exn Sentry_rpcs.get_password_entry_v1 connection
               {
                 Sentry_rpcs.Entry_info.user;
                 master_password;
                 entry;
                 entry_password = "";
               })
       with
       | Error _ as err -> return err
       | Ok entry_password ->
           print_endline entry_password;
           Deferred.Or_error.ok_unit)
