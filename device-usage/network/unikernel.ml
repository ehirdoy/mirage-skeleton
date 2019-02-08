open Lwt.Infix

let stored_stack_handler : Mirage_stack_lwt.V4.t option ref = ref None

module MyNet = struct

  module S = Mirage_stack_lwt.V4 (* from main.ml *)

  let listen port =
    match !stored_stack_handler with
    | None -> failwith "Uninitialized"
    | Some k ->
      S.listen_tcpv4 k ~port (fun flow ->
          let dst, dst_port = S.TCPV4.dst flow in
          Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                        (Ipaddr.V4.to_string dst) dst_port);
          S.TCPV4.read flow >>= function
          | Ok `Eof -> Logs.info (fun f -> f "Closing connection!"); Lwt.return_unit
          | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
          | Ok (`Data b) ->
            Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b) (Cstruct.to_string b));
            S.TCPV4.close flow
        );

      S.listen k

end

module Main (S: Mirage_stack_lwt.V4) = struct

  let start (s : Mirage_stack_lwt.V4.t) =
    stored_stack_handler := Some s; (* save in global variable *)
    let port = Key_gen.port () in
    MyNet.listen port

end
