open! Core
open! Async

let make_secure_transport ?(private_key = Cryptography.Rsa.create ()) reader
    writer =
  let info = Info.of_string "" in
  let public_key = Cryptography.Rsa.public_key private_key in
  don't_wait_for
    (Pipe.write_if_open writer
       (sprintf !"%{Cryptography.Rsa.Public}" public_key));
  let%bind.Deferred.Or_error other_side_public_key =
    match%bind Pipe.read reader with
    | `Ok a -> Cryptography.Rsa.Public.of_string a |> Deferred.Or_error.return
    | `Eof ->
        Deferred.Or_error.errorf
          "Did not receive public key from the server side"
  in
  let reader =
    Pipe.map reader ~f:(fun str -> Cryptography.Rsa.decrypt private_key str)
  in
  let writer =
    Pipe.create_writer (fun reader ->
        Pipe.transfer reader writer ~f:(fun str ->
            Cryptography.Rsa.encrypt other_side_public_key str))
  in
  let%bind reader = Reader.of_pipe info reader in
  let%map writer, _ = Writer.of_pipe info writer in
  Or_error.return (reader, writer)

let connect ~where_to_connect =
  let handshake_timeout =
    Time_ns.Span.to_span_float_round_nearest
      Async_rpc_kernel.Async_rpc_kernel_private.default_handshake_timeout
  in
  let finish_handshake_by =
    Time_ns.add (Time_ns.now ())
      (Time_ns.Span.of_span_float_round_nearest handshake_timeout)
  in
  let%bind.Deferred.Or_error sock =
    Monitor.try_with_or_error (fun () ->
        Tcp.connect_sock ~timeout:handshake_timeout where_to_connect)
  in
  let description =
    Info.create "Client connected via TCP" where_to_connect
      [%sexp_of: _ Tcp.Where_to_connect.t]
  in
  let handshake_timeout = Time_ns.diff finish_handshake_by (Time_ns.now ()) in
  let reader = Reader.create (Socket.fd sock) |> Reader.pipe in
  let writer = Writer.create (Socket.fd sock) |> Writer.pipe in
  let%bind.Deferred.Or_error transport =
    let%map.Deferred.Or_error reader, writer =
      make_secure_transport reader writer
    in
    let max_message_size = 100 * 1024 * 1024 in
    Rpc.Transport.of_reader_writer ~max_message_size reader writer
  in
  let {
    Async_rpc_kernel.Rpc.Connection.Client_implementations.connection_state;
    implementations;
  } =
    Async_rpc_kernel.Rpc.Connection.Client_implementations.null ()
  in
  match%bind
    Async_rpc_kernel.Rpc.Connection.create transport ~handshake_timeout
      ~description ~connection_state ~implementations
  with
  | Ok connection -> Deferred.return (Ok connection)
  | Error exn ->
      let%map () = Rpc.Transport.close transport in
      Or_error.of_exn exn

module Server = struct
  let create ?private_key ~where_to_listen f =
    let%bind _ =
      Tcp.Server.create ~on_handler_error:`Ignore where_to_listen (fun _ r w ->
          let reader = Reader.pipe r in
          let writer = Writer.pipe w in
          let%bind reader, writer =
            make_secure_transport ?private_key reader writer >>| ok_exn
          in
          f reader writer)
    in
    Deferred.unit
end
