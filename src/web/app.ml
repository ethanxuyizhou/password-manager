open! Core_kernel
open! Async_kernel
open Bonsai_web.Future

module All = struct
  type t = 
    | Login [@@deriving sexp, equal]
end

module Model = struct
  type t = { login : Login.Model.t; currently_rendering : All.t } [@@deriving sexp, equal]

  let update_login t login' =
    { t with login = login' }

  let default = { login = Login.Model.default; currently_rendering = All.Login } 
end

module Action = struct
  type t = 
    | Login of Login.Action.t
  [@@deriving sexp]
end

let build_login ((model : Login.Model.t), apply_action) ~send_rpc =
 let login_box =
    Vdom.Node.input
      [
        Vdom.Attr.type_ "text";
        Vdom.Attr.string_property "value" model.login_name;
        Vdom.Attr.on_input (fun _ev text ->
            apply_action (Login.Action.Update_login_name text));
      ]
      []
  in
  let password_box = 
    Vdom.Node.input
      [
        Vdom.Attr.type_ "text";
        Vdom.Attr.string_property "value" model.login_password;
        Vdom.Attr.on_input (fun _ev text ->
            apply_action (Login.Action.Update_login_password text));
      ]
      []
  in 
  let login_or_fail () = 
    send_rpc (Sentry_rpcs.list_password_entries_v1, { Sentry_rpcs.User_and_password.user = model.login_name; master_password = model.login_password})
    |> Effect.inject ~on_response:(function
          | Error _err -> apply_action (Login.Action.Update_login_status false)
          | Ok _entries -> apply_action (Login.Action.Update_login_status true))
  in
  let login_button =
    Vdom.Node.button
      [ Vdom.Attr.on_click (fun _ev -> login_or_fail ()) ]
      [ Vdom.Node.text "Log In" ]
  in
  let error_box =
    if model.login_status
    then []
    else [ Vdom.Node.text "Incorrect username or master password. Please try again." ]
  in 
  Vdom.Node.body [] ([ login_box; password_box; login_button ] @ error_box)

let build ((model : Model.t), apply_action) ~send_rpc =
  match model.currently_rendering with
  | All.Login -> build_login (model.login, (fun action -> apply_action (Action.Login action))) ~send_rpc

let apply_action ~inject:_ ~schedule_event:_ (model : Model.t) action =
  match action with
  | Action.Login (Login.Action.Update_login_name login) -> Model.update_login model { model.login with login_name = login}
  | Login (Login.Action.Update_login_password password) -> Model.update_login model {model.login with login_password = password }
  | Login (Login.Action.Update_login_status status) -> Model.update_login model { model.login with login_status = status }   

let component ~send_rpc =
  let open Bonsai.Let_syntax in
  let%sub state =
    Bonsai.state_machine0 [%here]
      (module Model)
      (module Action)
      ~default_model:Model.default
      ~apply_action
  in
  return
    (let%map state = state in
     build state ~send_rpc)
