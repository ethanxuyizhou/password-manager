open! Core_kernel
open! Async_kernel

type t = Login | Home [@@deriving sexp, equal]
