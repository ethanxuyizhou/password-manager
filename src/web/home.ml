open! Core_kernel
open! Async_kernel
open Bonsai_web.Future

module Model = struct
  type t = {
    user : string;
    master_password : string;
    password_entries : string list;
  }
  [@@deriving sexp, equal, fields]

  let default = { user = ""; master_password = ""; password_entries = [] }
end

module Action = struct
  type t = Update_password_entries of string list [@@deriving sexp]
end

let apply_action (model : Model.t) (action : Action.t) =
  match action with
  | Update_password_entries password_entries -> { model with password_entries }

let build ((model : Model.t), apply_action) ~send_rpc =
  let refresh_password_entries () =
    send_rpc
      ( Sentry_rpcs.list_password_entries_v1,
        {
          Sentry_rpcs.User_and_password.user = model.user;
          master_password = model.master_password;
        } )
    |> Effect.inject ~on_response:(function
         | Error (_ : Error.t) ->
             apply_action (Action.Update_password_entries [])
         | Ok password_entries ->
             apply_action (Action.Update_password_entries password_entries))
  in
  let refresh_button =
    Vdom.Node.button
      [
        Vdom.Attr.on_click (fun _ev -> refresh_password_entries ());
        Vdom.Attr.style (Css_gen.margin_bottom (`Px 10));
      ]
      [ Vdom.Node.text "Refresh" ]
  in
  let o item = Vdom.Node.tr [] [ Vdom.Node.td [] [ Vdom.Node.text item ] ] in
  let password_entries =
    Vdom.Node.table []
      (List.map model.password_entries ~f:(fun password_entry ->
           o password_entry))
  in
  Vdom.Node.div [] [ refresh_button; password_entries ]

let next_step (_ : Model.t) = All.Home
