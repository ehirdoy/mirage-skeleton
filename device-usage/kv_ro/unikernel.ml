open Lwt.Infix

module Main (KV: Mirage_kv_lwt.RO) = struct

  let start kv =
    let our_secret = "foo\n" in
    KV.get kv (Mirage_kv.Key.v "secret") >|= function
    | Error e ->
      Logs.warn (fun f -> f "Could not compare the secret against a known constant: %a"
        KV.pp_error e)
    | Ok stored_secret ->
        Logs.info (fun f -> f "%s" stored_secret)
end
