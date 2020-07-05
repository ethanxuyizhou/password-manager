open! Core_kernel
open! Import

(* This module is a wrapper around some of the modules in
 * the Cryptokit library in order to make it more typeful
 * and easier to use *)

module Aes : sig
  val encrypt : key:string -> data:string -> string

  val decrypt : key:string -> data:string -> string

  val hash : string -> string
end

module Rsa : sig
  type 'a t [@@deriving sexp]

  module Public : sig
    type nonrec t = [ `Public ] t [@@deriving sexp, bin_io]

    val to_string : t -> string

    val of_string : string -> t
  end

  module Private : sig
    type nonrec t = [ `Private ] t
  end

  val create : unit -> [ `Private ] t

  val public_key : [ `Private ] t -> [ `Public ] t

  val decrypt : [ `Private ] t -> string -> string

  val encrypt : [ `Public ] t -> string -> string
end
