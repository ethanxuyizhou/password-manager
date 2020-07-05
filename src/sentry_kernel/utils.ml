open! Core_kernel
open! Async_kernel
open! Async_rpc_kernel

let make_secure_transport ?(private_key = Cryptography.Rsa.create ()) reader writer =
  let public_key = Cryptography.Rsa.public_key private_key in
  don't_wait_for
    (Pipe.write_if_open writer
       (sprintf !"%{Cryptography.Rsa.Public}" public_key));
  let%map.Deferred.Or_error other_side_public_key =
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
  reader, writer


