open Interpolation
open Io

module StateMonad = struct
  type ('a, 's) state = 's -> 'a * 's

  let return x = fun s -> x, s

  let bind m f =
    fun s ->
    let x, s' = m s in
    f x s'
  ;;

  let ( >>= ) = bind
  let get = fun s -> s, s
  let put new_state = fun _ -> (), new_state
  let modify f = fun s -> (), f s
end

open StateMonad

type runner_state =
  { step : float
  ; points_stream : (float * float) Seq.t
  ; interpolation_types : interpolation list
  ; last_interpolated_x : (string * float option) list
  }

let gen_data x_min x_max step =
  let rec aux current =
    if current >= x_max
    then Seq.Nil
    else (
      let next = current +. step in
      Seq.Cons (current, fun () -> if next > x_max then Seq.Nil else aux next))
  in
  fun () -> aux x_min
;;

let take n lst =
  let rec aux n lst acc =
    match lst, n with
    | _, 0 | [], _ -> List.rev acc
    | x :: xs, _ -> aux (n - 1) xs (x :: acc)
  in
  aux n lst []
;;

let sort_interpolations interpolation_types =
  List.sort (fun a b -> compare a.window_size b.window_size) interpolation_types
;;

let compute_x_min last_x relevant_points step =
  match last_x with
  | None -> fst (List.hd relevant_points)
  | Some x ->
    (match x with
     | Some v -> v +. step
     | None -> fst (List.hd relevant_points))
;;

let interpolate_points interpolation relevant_points step last_interpolated =
  let last_x = List.assoc_opt interpolation.name last_interpolated in
  let x_min = compute_x_min last_x relevant_points step in
  let end_point = List.hd (List.rev relevant_points) in
  if x_min < fst end_point
  then (
    let result =
      gen_data x_min (fst end_point) step
      |> Seq.map (fun x -> x, interpolation.interpolate relevant_points x)
      |> List.of_seq
    in
    print_endline interpolation.name;
    print_points result;
    interpolation.name, Some (fst (List.hd (List.rev result))))
  else interpolation.name, last_x |> Option.join
;;

let rec read_init_points n acc =
  if n <= 0
  then acc
  else (
    let point = read_point () in
    read_init_points (n - 1) (Seq.append acc (Seq.return point)))
;;

let read_init_points n = read_init_points n Seq.empty

let apply_interp points interpolation_types step last_interpolated =
  let sorted_types = sort_interpolations interpolation_types in
  List.map
    (fun interpolation ->
       let relevant_points =
         if interpolation.window_size = 2
         then List.rev (List.rev points |> List.rev |> List.rev)
         else List.rev (List.rev points |> List.rev)
       in
       if List.length relevant_points >= interpolation.window_size
       then interpolate_points interpolation relevant_points step last_interpolated
       else interpolation.name, None)
    sorted_types
;;

let read_and_update_runner () =
  try
    let new_point = read_point () in
    modify (fun state -> { state with points_stream = Seq.append state.points_stream (Seq.return new_point) })
  with
  | End_of_file -> return ()
;;

let rec update_runner () =
  bind get (fun state ->
    let points = List.of_seq state.points_stream in
    let last_interpolated =
      if List.length points >= 2
      then apply_interp points state.interpolation_types state.step state.last_interpolated_x
      else state.last_interpolated_x
    in
    bind (read_and_update_runner ()) (fun () ->
      bind get (fun updated_state ->
        let required_points =
          List.fold_left
            (fun acc interpolation -> max acc interpolation.window_size)
            0
            updated_state.interpolation_types
        in
        let updated_points =
          if Seq.length updated_state.points_stream > required_points
          then Seq.drop 1 updated_state.points_stream
          else updated_state.points_stream
        in
        put { updated_state with points_stream = updated_points; last_interpolated_x = last_interpolated }
        >>= update_runner)))
;;

let init_runner runner =
  let min_points = 2 in
  let init_points = read_init_points min_points in
  update_runner
    ()
    { runner with
      points_stream = init_points
    ; last_interpolated_x = List.map (fun i -> i.name, None) runner.interpolation_types
    }
;;
