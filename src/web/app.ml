open! Core_kernel
open! Async_kernel
open Bonsai_web.Future

module Model = struct
  type t = { login_name : string; login_password : string} [@@deriving sexp, equal]

  let default = { login_name = ""; login_password = ""}
end

module Action = struct
  type t = 
    | Update_login_name of string
    | Update_login_password of string
  [@@deriving sexp]
end

let build ((model : Model.t), apply_action) ~send_rpc =
 let login_box =
    Vdom.Node.input
      [
        Vdom.Attr.type_ "text";
        Vdom.Attr.string_property "value" model.login_name;
        Vdom.Attr.on_input (fun _ev text ->
            apply_action (Action.Update_login_name text));
      ]
      []
  in
  let password_box = 
    Vdom.Node.input
      [
        Vdom.Attr.type_ "text";
        Vdom.Attr.string_property "value" model.login_password;
        Vdom.Attr.on_input (fun _ev text ->
            apply_action (Action.Update_login_password text));
      ]
      []
  in 
  let login_or_fail () = 
    send_rpc (Sentry_rpcs.list_password_entries_v1, { Sentry_rpcs.User_and_password.user = model.login_name; master_password = model.login_password})
    |> Effect.inject ~on_response:(function
          | Error _err -> Ui_event.Ignore
          | Ok _entries -> Ui_event.Ignore)
  in
  let login_button =
    Vdom.Node.button
      [ Vdom.Attr.on_click (fun _ev -> login_or_fail ()) ]
      [ Vdom.Node.text "Add" ]
  in
  Vdom.Node.body [] [ login_box; password_box; login_button ]

let component ~send_rpc =
  let open Bonsai.Let_syntax in
  let%sub state =
    Bonsai.state_machine0 [%here]
      (module Model)
      (module Action)
      ~default_model:Model.default
      ~apply_action:(fun ~inject:_ ~schedule_event:_ (model : Model.t) action ->
        match action with
          | Action.Update_login_name login -> { model with login_name = login}
          | Update_login_password password -> {model with login_password = password })
  in
  return
    (let%map state = state in
     build state ~send_rpc)
