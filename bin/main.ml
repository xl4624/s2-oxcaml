open Async

let () =
  print_endline "Initializing S2 project with Async and Core...";
  Command.async
    ~summary:"S2 Geometry Port"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       print_endline "Hello from OxCaml S2!";
       Deferred.unit)
  |> Command_unix.run
;;
