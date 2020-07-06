open! Core_kernel
open! Async_kernel

module Model = struct
  type t = { login_name : string; login_password : string; login_status : bool} [@@deriving sexp, equal]

  let default = { login_name = ""; login_password = ""; login_status = true}
end

module Action = struct
  type t = 
    | Update_login_name of string
    | Update_login_password of string
    | Update_login_status of bool
  [@@deriving sexp]
end
