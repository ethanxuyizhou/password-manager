open! Core_kernel
open! Async_rpc_kernel

module User_and_password = struct
  module Stable = struct
    module V1 = struct
      type t = { user : string; master_password : string }
      [@@deriving bin_io, sexp]
    end
  end

  include Stable.V1
end

module Entry_info = struct
  module Stable = struct
    module V1 = struct
      type t = {
        user : string;
        master_password : string;
        entry : string;
        entry_password : string;
      }
      [@@deriving bin_io, sexp]
    end
  end

  include Stable.V1
end

let add_user_v1 =
  Rpc.Rpc.create ~name:"add-user" ~version:1
    ~bin_query:User_and_password.Stable.V1.bin_t
    ~bin_response:(Or_error.Stable.V2.bin_t Unit.Stable.V1.bin_t)

let remove_user_v1 =
  Rpc.Rpc.create ~name:"remove-user" ~version:1
    ~bin_query:User_and_password.Stable.V1.bin_t
    ~bin_response:(Or_error.Stable.V2.bin_t Unit.Stable.V1.bin_t)

let list_password_entries_v1 =
  Rpc.Rpc.create ~name:"list-password-entries" ~version:1
    ~bin_query:User_and_password.Stable.V1.bin_t
    ~bin_response:(Or_error.Stable.V2.bin_t (List.Stable.V1.bin_t String.Stable.V1.bin_t))

let add_password_entry_v1 =
  Rpc.Rpc.create ~name:"add-password-entry" ~version:1
    ~bin_query:Entry_info.Stable.V1.bin_t
    ~bin_response:(Or_error.Stable.V2.bin_t Unit.Stable.V1.bin_t)

let remove_password_entry_v1 =
  Rpc.Rpc.create ~name:"remove-password-entry" ~version:1
    ~bin_query:Entry_info.Stable.V1.bin_t
    ~bin_response:(Or_error.Stable.V2.bin_t Unit.Stable.V1.bin_t)

let get_password_entry_v1 =
  Rpc.Rpc.create ~name:"get-password-entry" ~version:1
    ~bin_query:Entry_info.Stable.V1.bin_t
    ~bin_response:(Or_error.Stable.V2.bin_t String.Stable.V1.bin_t)
