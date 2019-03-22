open Lwt.Infix

module Main (KV: Mirage_kv_lwt.RO) = struct

  let fname = "secret"

  (* val get: t -> key -> (value, error) result io *)
  let test_get kv =
    KV.get kv (Mirage_kv.Key.v fname) >|= function
    | Error e ->
      Logs.warn (fun f -> f
	"Could not compare the secret against a known constant: %a"
	KV.pp_error e)
    | Ok stored_secret ->
      match String.compare "foo\n" stored_secret with
      | 0 ->
        Logs.info (fun f -> f
	"Contents of extremely secret vital storage confirmed!")
      | _ ->
        Logs.warn (fun f -> f "The secret provided does not match!")

  (* val exists: t -> key -> ([`Value | `Dictionary] option, error) result io *)
  let test_exists kv =
    KV.exists kv (Mirage_kv.Key.v fname) >|= function
    | Error e -> Logs.err (fun f -> f "%a" KV.pp_error e)
    | Ok ok -> match ok with
      | Some `Value -> Logs.info (fun f -> f "%s is `Value" fname)
      | Some `Dictionary -> Logs.info (fun f -> f "%s is `Dictionary" fname)
      | None ->  Logs.info (fun f -> f "%s is None" fname)

  (* val list: t -> key -> ((string * [`Value | `Dictionary]) list, error) result io *)
  let test_list kv =
    KV.list kv (Mirage_kv.Key.v "") >|= function
    | Error e -> Logs.err (fun f -> f "%a" KV.pp_error e)
    | Ok list ->
      List.iter (fun el -> match el with
          | s, `Value -> Logs.info (fun f -> f "%s is `Value" s)
          | s, `Dictionary -> Logs.info (fun f -> f "%s is `Dictionary" s))
        list

  (* val last_modified: t -> key -> (int * int64, error) result io *)
  let test_last_modified kv =
    KV.last_modified kv (Mirage_kv.Key.v fname) >|= function
    | Error e -> Logs.err (fun f -> f "%a" KV.pp_error e)
    | Ok (s, nsec) -> Logs.info (fun f -> f "%d %Ld" s nsec)

  (* val digest: t -> key -> (string, error) result io *)
  let test_digest kv =
    KV.digest kv (Mirage_kv.Key.v fname) >|= function
    | Error e -> Logs.err (fun f -> f "%a" KV.pp_error e)
    | Ok str -> Logs.info (fun f -> f "%s" str)


  let start kv =
    test_get kv >>= fun () ->
    test_exists kv >>= fun () ->
    test_list kv >>= fun () ->
    test_last_modified kv  >>= fun () ->
    test_digest kv >>= fun () ->
    Lwt.return_unit

end
