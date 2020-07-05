open! Core_kernel
open! Async_kernel
include Random

let substitute_one_random_character string ~rand_int ~excluding_indices =
  let rec index_loop () =
    let index = Random.int (String.length string) in
    if Hash_set.mem excluding_indices index then index_loop () else index
  in
  let index = index_loop () in
  Hash_set.add excluding_indices index;
  String.mapi string ~f:(fun i char ->
      if Int.equal i index then rand_int () |> Char.of_int_exn else char)

let random_string' ~length ~rand_int =
  List.map (List.range 0 length) ~f:(fun (_ : int) ->
      rand_int () |> Char.of_int_exn)
  |> String.of_char_list

let random_string ?must_include_numbers ?must_include_uppercase_letters
    ?must_include_lowercase_letters ~length () =
  Random.self_init ();
  let open Or_error.Let_syntax in
  let string =
    random_string' ~length ~rand_int:(fun () -> Random.int_incl 33 122)
  in
  let rand_int_generators =
    [
      (* TODO: don't hardcode 33 and 122 *)
      (must_include_numbers, fun () -> Random.int_incl 48 57);
      (must_include_lowercase_letters, fun () -> Random.int_incl 97 122);
      (must_include_uppercase_letters, fun () -> Random.int_incl 65 90);
    ]
    |> List.filter ~f:(fun (requirement, _) -> Option.is_some requirement)
    |> List.map ~f:snd
  in
  let%map () =
    let number_of_requirements = List.length rand_int_generators in
    if number_of_requirements > length then
      Or_error.errorf
        !"Length (%d) must be greater than the total number of requirements \
          (%d)"
        length number_of_requirements
    else Ok ()
  in
  let excluding_indices = Int.Hash_set.create () in
  List.fold rand_int_generators ~init:string ~f:(fun string rand_int ->
      substitute_one_random_character string ~rand_int ~excluding_indices)

let%expect_test "test ascii ranges are correct" =
  let print_ascii_chars ~lower ~upper =
    List.map
      (List.range lower (upper + 1))
      ~f:(fun ascii_number -> Char.of_int_exn ascii_number |> String.of_char)
    |> String.concat ~sep:"," |> print_string
  in
  print_ascii_chars ~lower:33 ~upper:122;
  let () =
    [%expect
      {| !,",#,$,%,&,',(,),*,+,,,-,.,/,0,1,2,3,4,5,6,7,8,9,:,;,<,=,>,?,@,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,[,\,],^,_,`,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z |}]
  in
  print_ascii_chars ~lower:48 ~upper:57;
  let () = [%expect {| 0,1,2,3,4,5,6,7,8,9 |}] in
  print_ascii_chars ~lower:97 ~upper:122;
  let () =
    [%expect {| a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z |}]
  in
  print_ascii_chars ~lower:65 ~upper:90;
  [%expect {| A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z |}]
