open! Core
open! Async

let ask_user str =
    Async_interactive.ask_dispatch_gen
      ~f:(fun str ->
        if String.is_empty str then Error "Answer should not be empty" else Ok str)
      str
