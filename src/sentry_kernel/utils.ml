open! Core_kernel
open! Async_kernel
open! Async_rpc_kernel

let make_secure_transport ?(private_key = Cryptography.Rsa.create ()) reader
    writer =
  let public_key = Cryptography.Rsa.public_key private_key in
  don't_wait_for
    (Pipe.write_if_open writer
       (sprintf !"%{Cryptography.Rsa.Public}" public_key));
  match%bind Pipe.read reader with
  | `Eof ->
      Deferred.Or_error.errorf
        "Did not receive the public key from the other side of the rpc call"
  | `Ok other_side_public_key ->
      let other_side_public_key =
        Cryptography.Rsa.Public.of_string other_side_public_key
      in
      let reader =
        Pipe.map reader ~f:(fun str -> Cryptography.Rsa.decrypt private_key str)
      in
      let writer =
        Pipe.create_writer (fun reader ->
            Pipe.transfer reader writer ~f:(fun str ->
                Cryptography.Rsa.encrypt other_side_public_key str))
      in
      Deferred.Or_error.return (reader, writer)
