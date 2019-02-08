open Mirage

let port =
  let doc = Key.Arg.info ~doc:"The TCP port on which to listen for incoming connections." ["port"] in
  Key.(create "port" Arg.(opt int 8080 doc))

let packages = [
  package "mirage-stack-lwt";
]

let main = foreign ~keys:[Key.abstract port] "Unikernel.Main" (stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "network" [
    main $ stack
  ]
