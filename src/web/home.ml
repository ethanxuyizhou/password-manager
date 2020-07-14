open! Core_kernel
open! Async_kernel
open Bonsai_web.Future

module Model = struct
  type t = unit
  [@@deriving sexp, equal]

  let default = ()
end

module Action = struct
  type t = unit 
  [@@deriving sexp]
end

let apply_action (_model : Model.t) action = 
  match action with
  | () -> ()

let build ((_model : Model.t), _apply_action) =
  Vdom.Node.body [] []
