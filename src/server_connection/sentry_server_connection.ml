open! Core
open! Async
open Sentry_lib

let where_to_connect =
  Tcp.Where_to_connect.of_host_and_port
    (Host_and_port.of_string "localhost:8080")

let with_close ~f =
  match%bind Secure_connection.connect ~where_to_connect with
  | Error _ as err -> return err
  | Ok connection ->
      Monitor.protect
        (fun () -> f connection)
        ~finally:(fun () -> Rpc.Connection.close connection)
