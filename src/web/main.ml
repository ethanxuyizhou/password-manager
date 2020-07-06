open! Core_kernel
open! Async_kernel
open! Async_js
open! Bonsai_web.Future

let run () =  
  Async_js.init ();
  let%bind send_rpc =
    let%map connection = Rpc.Connection.client_exn ~uri:(Uri.of_string "ws://localhost") () in
    (fun (protocol, query) -> 
      Rpc.Rpc.dispatch_exn protocol connection query)
    |> Effect.of_deferred_fun
    |> unstage
  in
  let (_: (unit, never_returns) Start.Handle.t) = 
    Start.start
      Start.Result_spec.just_the_view
      ~bind_to_element_with_id:"app"
      (App.component ~send_rpc)
  in
  Deferred.unit

let () = 
  don't_wait_for (run ())
