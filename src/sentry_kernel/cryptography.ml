open! Core_kernel
open! Import

(* Use the [escape] char for padding. Consider using a better key *)
let pad_char = Char.of_int_exn 27

let pad ?(char = pad_char) string ~len =
  let pad_string =
    List.map (List.range 0 (len - String.length string)) ~f:(fun _ -> char)
    |> String.of_char_list
  in
  String.concat ~sep:"" [ string; pad_string ]

module Aes = struct
  let cipher ~direction ~key ~data =
    let block_size = 16 in
    let next_fit length =
      match length % block_size with
      | 0 -> length
      | _ -> length - (length % block_size) + block_size
    in
    let trim string =
      let next_fit_length = next_fit (String.length string) in
      match Int.equal (String.length string) next_fit_length with
      | true -> string
      | false -> pad string ~char:' ' ~len:next_fit_length
    in
    let key = trim key in
    let data = trim data in
    let transform = Cipher.aes key direction in
    transform#put_string data;
    transform#finish;
    transform#get_string

  let encrypt ~key ~data = cipher ~direction:Cipher.Encrypt ~key ~data

  let decrypt ~key ~data = cipher ~direction:Cipher.Decrypt ~key ~data

  let hash string = Sha256.string string |> Sha256.to_bin
end

module Rsa = struct
  (* Definition copied from the [cryptokit] library in order to 
   * derive sexp, bin_io *)
  type 'a t = {
    size : int;  (** Size of the modulus [n], in bits *)
    n : string;  (** Modulus [n = p.q] *)
    e : string;  (** Public exponent [e] *)
    d : string;  (** Private exponent [d] *)
    p : string;  (** Prime factor [p] of [n] *)
    q : string;  (** The other prime factor [q] of [n] *)
    dp : string;  (** [dp] is [d mod (p-1)] *)
    dq : string;  (** [dq] is [d mod (q-1)] *)
    qinv : string;  (** [qinv] is a multiplicative inverse of [q] modulo [p] *)
  }
  [@@deriving sexp, bin_io]

  module Public = struct
    type nonrec t = [ `Public ] t [@@deriving sexp, bin_io]

    let to_string t = Sexp.to_string (sexp_of_t t)

    let of_string str = t_of_sexp (Sexp.of_string str)
  end

  module Private = struct
    type nonrec t = [ `Private ] t
  end

  let key_size = 2048

  let msg_size = key_size / 8

  let from_rsa_key { RSA.size; n; e; d; p; q; dp; dq; qinv } =
    { size; n; e; d; p; q; dp; dq; qinv }

  let to_rsa_key { size; n; e; d; p; q; dp; dq; qinv } =
    { RSA.size; n; e; d; p; q; dp; dq; qinv }

  let create () = RSA.new_key key_size |> from_rsa_key

  let public_key t =
    {
      size = t.size;
      n = t.n;
      e = t.e;
      d = "";
      p = "";
      q = "";
      dp = "";
      dq = "";
      qinv = "";
    }

  let break_down str =
    let rec loop chars =
      if List.length chars <= msg_size then
        [ String.of_char_list chars |> pad ~len:msg_size ]
      else
        let lst1, lst2 = List.split_n chars msg_size in
        let string1 = String.of_char_list lst1 |> pad ~len:msg_size in
        string1 :: loop lst2
    in
    loop (String.to_list str)

  let convert str ~f = break_down str |> List.map ~f |> String.concat ~sep:""

  let encrypt t str = convert str ~f:(RSA.encrypt (to_rsa_key t))

  let decrypt t str =
    convert str ~f:(RSA.decrypt (to_rsa_key t))
    |> String.strip ~drop:(Char.equal pad_char)
end
