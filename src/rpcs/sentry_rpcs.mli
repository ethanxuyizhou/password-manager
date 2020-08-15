open! Core_kernel
open! Async_kernel
open Async_rpc_kernel

module User_and_password : sig
  type t = { user : string; master_password : string } [@@deriving bin_io, sexp]

  module Stable : sig
    module V1 : sig
      type nonrec t = t
    end
  end
end

module Entry_info : sig
  type t = {
    user : string;
    master_password : string;
    entry : string;
    entry_password : string;
  }
  [@@deriving bin_io, sexp]

  module Stable : sig
    module V1 : sig
      type nonrec t = t
    end
  end
end

val add_user_v1 : (User_and_password.t, unit Or_error.t) Rpc.Rpc.t

val remove_user_v1 : (User_and_password.t, unit Or_error.t) Rpc.Rpc.t

val list_password_entries_v1 :
  (User_and_password.t, string list Or_error.t) Rpc.Rpc.t

val add_password_entry_v1 : (Entry_info.t, unit Or_error.t) Rpc.Rpc.t

val remove_password_entry_v1 : (Entry_info.t, unit Or_error.t) Rpc.Rpc.t

val get_password_entry_v1 : (Entry_info.t, string Or_error.t) Rpc.Rpc.t
