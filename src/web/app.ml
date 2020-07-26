open! Core_kernel
open! Async_kernel
open Bonsai_web.Future

module Model = struct
  type t = {
    login : Login.Model.t;
    home : Home.Model.t;
    currently_rendering : All.t;
  }
  [@@deriving sexp, equal]

  let update_login t login' = { t with login = login' }

  let default =
    {
      login = Login.Model.default;
      home = Home.Model.default;
      currently_rendering = All.Login;
    }

  let apply_redirection_if_necessary model =
    match model.currently_rendering with
    | All.Login -> (
        match Login.next_step model.login with
        | Login -> model
        | Home ->
            let home =
              Home.Model.Fields.create
                ~user:(Login.Model.login_name model.login)
                ~master_password:(Login.Model.login_password model.login)
                ~password_entries:[]
            in
            { model with home; currently_rendering = Home } )
    | All.Home -> { model with currently_rendering = Home.next_step model.home }

  let update model ?home ?login () =
    let model =
      Option.value_map home ~default:model ~f:(fun home -> { model with home })
    in
    let model =
      Option.value_map login ~default:model ~f:(fun login ->
          { model with login })
    in
    apply_redirection_if_necessary model
end

module Action = struct
  type t = Login of Login.Action.t | Home of Home.Action.t [@@deriving sexp]
end

let build ((model : Model.t), apply_action) ~send_rpc =
  match model.currently_rendering with
  | All.Login ->
      Login.build
        (model.login, fun action -> apply_action (Action.Login action))
        ~send_rpc
  | Home ->
      Home.build
        (model.home, fun action -> apply_action (Action.Home action))
        ~send_rpc

let apply_action ~inject:_ ~schedule_event:_ (model : Model.t) action =
  match action with
  | Action.Login action ->
      Model.update model ~login:(Login.apply_action model.login action) ()
  | Home action ->
      Model.update model ~home:(Home.apply_action model.home action) ()

let component ~send_rpc =
  let open Bonsai.Let_syntax in
  let%sub state =
    Bonsai.state_machine0 [%here]
      (module Model)
      (module Action)
      ~default_model:Model.default ~apply_action
  in
  return
    (let%map state = state in
     build state ~send_rpc)
