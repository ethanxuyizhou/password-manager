open! Core_kernel
open! Async_kernel
open Bonsai_web.Future

module Model = struct
  type t = { login_name : string; login_password : string; login_status : bool option} [@@deriving sexp, equal, fields]

  let default = { login_name = ""; login_password = ""; login_status = None } 
end

module Action = struct
  type t = 
    | Update_login_name of string
    | Update_login_password of string
    | Update_login_status of bool
  [@@deriving sexp]
end

let apply_action (model : Model.t) ( action : Action.t ) = 
  match action with
  |  (Update_login_name login) ->  { model with login_name = login}
  |  (Update_login_password password) -> {model with login_password = password }
  |  (Update_login_status status) -> { model with login_status = Some status }   


let build ((model : Model.t), apply_action) ~send_rpc =
  let login_banner =
    Vdom.Node.div [ Vdom.Attr.id "login-banner"] [ Vdom.Node.text "SENTRY LOGIN" ] 
  in
  let login_box =
   Vdom.Node.div 
     [ Vdom.Attr.id "login-box" ]
     [ Vdom.Node.input
       [
        Vdom.Attr.type_ "text";
        Vdom.Attr.string_property "value" model.login_name;
        Vdom.Attr.on_input (fun _ev text ->
            apply_action (Action.Update_login_name text));
       ]
       []
    ]
  in
  let password_box = 
    Vdom.Node.div 
      [ Vdom.Attr.id "password-box" ]
      [ Vdom.Node.input
        [
          Vdom.Attr.type_ "password";
          Vdom.Attr.string_property "value" model.login_password;
          Vdom.Attr.on_input (fun _ev text ->
              apply_action (Action.Update_login_password text));
        ]
        []
      ]
  in 
  let login_or_fail () = 
    send_rpc (Sentry_rpcs.list_password_entries_v1, { Sentry_rpcs.User_and_password.user = model.login_name; master_password = model.login_password})
    |> Effect.inject ~on_response:(function
          | Error (_ : Error.t) -> apply_action (Action.Update_login_status false)
          | Ok ( _ : string list) -> apply_action (Action.Update_login_status true))
  in
  let login_button =
    Vdom.Node.div
      [ Vdom.Attr.id "login-button" ]
      [ Vdom.Node.button
        [ Vdom.Attr.on_click (fun _ev -> login_or_fail ()) ]
        [ Vdom.Node.text "Log In" ] 
      ]
  in
  let error_box =
    match model.login_status with
    | None | Some true -> []
    | Some false -> [ Vdom.Node.div [ Vdom.Attr.id "login-error" ] [ Vdom.Node.text "Incorrect username or master password. Please try again." ] ]
  in 
  Vdom.Node.div [] ([ login_banner; login_box; password_box; login_button ] @ error_box)

let next_step (model : Model.t) =
  match model.login_status with
  | Some true -> All.Home
  | None | Some false -> All.Login
