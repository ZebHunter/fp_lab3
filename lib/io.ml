open Printf

let print_points points = List.iter (fun (x, y) -> printf "> %.2f %.2f\n" x y) points

let read_point () =
  try
    printf "< ";
    flush stdout;
    let line = read_line () |> String.trim in
    if line = "exit" || line = "EOF"
    then (
      print_endline "Input terminated.";
      raise End_of_file);
    match String.split_on_char ' ' line |> List.filter (fun s -> s <> "") with
    | [ x; y ] ->
      (try float_of_string x, float_of_string y with
       | _ -> failwith "Invalid numbers")
    | _ -> failwith "Expected two numbers"
  with
  | End_of_file ->
    print_endline "End of input detected";
    exit 0
;;
