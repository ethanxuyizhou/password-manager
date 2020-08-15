open! Core
open! Async
open Sentry_kernel
open Sentry_lib
open Sentry_state

type t = { tlog_service : Tlog.Service.t }

let create () = { tlog_service = Tlog.Service.create () }

let add_user_v1 t { Sentry_rpcs.User_and_password.user; master_password } =
  let hashed_master_password = Cryptography.Aes.hash master_password in
  Tlog.write_update t.tlog_service
    (Update.Add_user { user; hashed_master_password })

let remove_user_v1 t { Sentry_rpcs.User_and_password.user; master_password } =
  let hashed_master_password = Cryptography.Aes.hash master_password in
  Tlog.write_update t.tlog_service
    (Update.Remove_user { user; hashed_master_password })

let list_password_entries_v1 t
    { Sentry_rpcs.User_and_password.user; master_password } =
  let hashed_master_password = Cryptography.Aes.hash master_password in
  let state = Tlog.read_state t.tlog_service in
  State.lookup_password_entries state ~user ~hashed_master_password |> return

let add_password_entry_v1 t
    { Sentry_rpcs.Entry_info.user; master_password; entry; entry_password } =
  let hashed_master_password = Cryptography.Aes.hash master_password in
  let encrypted_password =
    Cryptography.Aes.encrypt ~key:master_password ~data:entry_password
  in
  Tlog.write_update t.tlog_service
    (Update.Add_entry
       { user; hashed_master_password; entry; encrypted_password })

let remove_password_entry_v1 t
    { Sentry_rpcs.Entry_info.user; master_password; entry; entry_password = _ }
    =
  let hashed_master_password = Cryptography.Aes.hash master_password in
  Tlog.write_update t.tlog_service
    (Update.Remove_entry { user; hashed_master_password; entry })

let get_password_entry_v1 t
    { Sentry_rpcs.Entry_info.user; master_password; entry; entry_password = _ }
    =
  let hashed_master_password = Cryptography.Aes.hash master_password in
  let state = Tlog.read_state t.tlog_service in
  match State.lookup_password state ~user ~hashed_master_password ~entry with
  | Error _ as err -> return err
  | Ok encrypted_password ->
      let result =
        Cryptography.Aes.decrypt ~key:master_password ~data:encrypted_password
      in
      return (Ok result)

let implementations =
  let implementations =
    [
      Rpc.Rpc.implement Sentry_rpcs.add_user_v1 add_user_v1;
      Rpc.Rpc.implement Sentry_rpcs.remove_user_v1 remove_user_v1;
      Rpc.Rpc.implement Sentry_rpcs.list_password_entries_v1
        list_password_entries_v1;
      Rpc.Rpc.implement Sentry_rpcs.add_password_entry_v1 add_password_entry_v1;
      Rpc.Rpc.implement Sentry_rpcs.remove_password_entry_v1
        remove_password_entry_v1;
      Rpc.Rpc.implement Sentry_rpcs.get_password_entry_v1 get_password_entry_v1;
    ]
  in
  Rpc.Implementations.create_exn ~implementations
    ~on_unknown_rpc:`Close_connection

let start_command =
  Command.async ~summary:"Sentry server"
    (let%map_open.Command () = return ()
     and port =
       flag "port" (required int)
         ~doc:"PORT port number that the server will accept request in"
     in
     fun () ->
       let t = create () in
       let%bind _ =
         let private_key = Cryptography.Rsa.create () in
         Secure_connection.Server.create ~private_key
           ~where_to_listen:(Tcp.Where_to_listen.of_port port) (fun r w ->
             Rpc.Connection.server_with_close r w ~implementations
               ~connection_state:(fun (_ : Rpc.Connection.t) -> t)
               ~on_handshake_error:`Ignore)
       in
       let%bind _ =
         let open Async_rpc_kernel in
         Tcp.Server.create ~on_handler_error:`Ignore
           (Tcp.Where_to_listen.of_port 80) (fun _ reader writer ->
             let app_to_ws, ws_write = Pipe.create () in
             let ws_read, ws_to_app = Pipe.create () in
             don't_wait_for
               (let%bind _ =
                  Websocket_async.server ~reader ~writer ~app_to_ws ~ws_to_app
                    ()
                in
                Deferred.unit);
             let pipe_r, pipe_w =
               let r1, w1 = Pipe.create () in
               let r2, w2 = Pipe.create () in
               upon (Pipe.closed ws_read) (fun () -> Pipe.close_read r1);
               upon (Pipe.closed ws_write) (fun () -> Pipe.close w2);
               don't_wait_for
                 (Pipe.transfer ws_read w1
                    ~f:(fun Websocket.Frame.
                              { opcode; extension; final; content }
                            ->
                      ignore (opcode : Websocket.Frame.Opcode.t);
                      ignore (extension : int);
                      ignore (final : bool);
                      content));
               don't_wait_for
                 (Pipe.iter r2 ~f:(fun content ->
                      Pipe.write_if_open ws_write
                        (Websocket.Frame.create
                           ~opcode:Websocket.Frame.Opcode.Binary ~content ())));
               (r1, w2)
             in
             let transport =
               Pipe_transport.create Pipe_transport.Kind.string pipe_r pipe_w
             in
             Rpc.Connection.server_with_close transport ~implementations
               ~connection_state:(fun (_ : Rpc.Connection.t) -> t)
               ~on_handshake_error:`Ignore)
       in
       Deferred.never ())

let init_command =
  Command.async ~summary:"Initialize tlog state"
    (let%map_open.Command () = return () in
     fun () ->
       let%bind () = Tlog.Service.create () |> Tlog.Service.init in
       Deferred.unit)
