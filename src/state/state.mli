open! Core
open! Async

type t [@@deriving sexp]

include
  Transaction_log.Transaction_log_state
    with type t := t
     and module Update = Update
     and type Update_result.t = unit Or_error.t

val lookup_password_entries :
  t ->
  user:string ->
  hashed_master_password:string ->
  string list Or_error.t

val lookup_password :
  t ->
  user:string ->
  hashed_master_password:string ->
  entry:string ->
  string Or_error.t
