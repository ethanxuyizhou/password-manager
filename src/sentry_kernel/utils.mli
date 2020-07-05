open! Core_kernel
open! Async_kernel

val make_secure_transport : ?private_key:Cryptography.Rsa.Private.t -> string Pipe.Reader.t -> string Pipe.Writer.t -> (string Pipe.Reader.t * string Pipe.Writer.t) Or_error.t Deferred.t
