open Lab3.Interpolation
open Lab3.Runner

let parse_args args =
  let rec aux args cur_step interpolation_types =
    match args with
    | [] ->
      { Lab3.Runner.step = cur_step
      ; points_stream = Seq.empty
      ; interpolation_types
      ; last_interpolated_x = List.map (fun i -> i.name, None) interpolation_types
      }
    | "--linear" :: rest -> aux rest cur_step (linear_interpolation :: interpolation_types)
    | "--lagrange" :: rest -> aux rest cur_step (lagrange_interpolation :: interpolation_types)
    | "-s" :: step_value :: rest -> aux rest (float_of_string step_value) interpolation_types
    | _ :: rest -> aux rest cur_step interpolation_types
  in
  aux args 1.0 []
;;

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let runner = parse_args args in
  init_runner runner
;;
