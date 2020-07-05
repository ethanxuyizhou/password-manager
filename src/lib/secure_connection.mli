open! Core
open! Async
open Sentry_kernel

val connect :
  where_to_connect:Tcp.Where_to_connect.inet ->
  Rpc.Connection.t Or_error.t Deferred.t

module Server : sig
  val create :
    ?private_key:Cryptography.Rsa.Private.t ->
    where_to_listen:Tcp.Where_to_listen.inet ->
    (Reader.t -> Writer.t -> unit Deferred.t) ->
    unit Deferred.t
end
