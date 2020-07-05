open! Core
open! Async

module Data = struct
  type t = {
    hashed_master_password : string;
    encrypted_passwords : string String.Map.t;
  }
  [@@deriving sexp, fields]

  let init ~hashed_master_password =
    { hashed_master_password; encrypted_passwords = String.Map.empty }

  let validate_password t password =
    String.equal t.hashed_master_password password

  let add_entry t ~hashed_master_password ~entry ~encrypted_password =
    if validate_password t hashed_master_password then
      match Map.mem t.encrypted_passwords entry with
      | true ->
          Or_error.errorf !"There already exists an password entry: %s" entry
      | false ->
          Ok
            {
              t with
              encrypted_passwords =
                Map.set t.encrypted_passwords ~key:entry
                  ~data:encrypted_password;
            }
    (* TODO: refactor the error code *)
    else Or_error.errorf !"Invalid password"

  let remove_entry t ~hashed_master_password ~entry =
    if validate_password t hashed_master_password then
      Ok { t with encrypted_passwords = Map.remove t.encrypted_passwords entry }
    else Or_error.errorf !"Invalid password"

  let find t ~hashed_master_password ~entry =
    if validate_password t hashed_master_password then
      match Map.find t.encrypted_passwords entry with
      | Some encrypted_password -> Ok encrypted_password
      | None -> Or_error.errorf !"No such entry: %s" entry
    else Or_error.errorf !"Invalid password"

  let all t ~hashed_master_password =
    if validate_password t hashed_master_password then
    Ok t.encrypted_passwords
    else Or_error.errorf !"Incorrect password"
end

module Update = Update

module Update_result = struct
  type t = unit Or_error.t
end

type t = Data.t String.Map.t [@@deriving sexp]

let create () = String.Map.empty

let rundir = "/Users/ethan/sentry"

let no_user_error ~user = Or_error.errorf !"User doesn't exists: %s" user

let apply_update t update =
  match update with
  | Update.Add_user { user; hashed_master_password } -> (
      match Map.mem t user with
      | true -> (t, Or_error.errorf !"User already exists: %s" user)
      | false ->
          let t =
            Map.set t ~key:user ~data:(Data.init ~hashed_master_password)
          in
          (t, Ok ()) )
  | Update.Remove_user { user; hashed_master_password } -> (
      match Map.find t user with
      | Some data ->
          if Data.validate_password data hashed_master_password then
            let t = Map.remove t user in
            (t, Ok ())
          else (t, Or_error.errorf !"Invalid password")
      | None -> (t, no_user_error ~user) )
  | Add_entry { user; hashed_master_password; entry; encrypted_password } -> (
      match Map.find t user with
      | None -> (t, no_user_error ~user)
      | Some data -> (
          match
            Data.add_entry data ~hashed_master_password ~entry
              ~encrypted_password
          with
          | Ok data -> (Map.set t ~key:user ~data, Ok ())
          | Error err -> (t, Error err) ) )
  | Remove_entry { user; hashed_master_password; entry } -> (
      match Map.find t user with
      | None -> (t, no_user_error ~user)
      | Some data -> (
          match Data.remove_entry data ~hashed_master_password ~entry with
          | Ok data -> (Map.set t ~key:user ~data, Ok ())
          | Error err -> (t, Error err) ) )

let lookup_password_entries t ~user ~hashed_master_password =
  match Map.find t user with
  | None -> no_user_error ~user
  | Some data -> 
    let%map.Or_error data = Data.all data ~hashed_master_password
    in
    Map.keys data

let lookup_password t ~user ~hashed_master_password ~entry =
  match Map.find t user with
  | None -> no_user_error ~user
  | Some data -> Data.find data ~hashed_master_password ~entry 
