open OUnit2
open Lab3.Interpolation
open Lab3.Runner

let test_gen_data _ =
  let seq = gen_data 0.0 1.0 0.5 in
  let result =
    List.of_seq
      (Seq.unfold
         (fun s ->
            match s () with
            | Seq.Nil -> None
            | Seq.Cons (x, xs) -> Some (x, xs))
         seq)
  in
  assert_equal [ 0.0; 0.5 ] result ~printer:(fun lst -> String.concat ", " (List.map string_of_float lst))
;;

let test_take _ =
  assert_equal [ 1; 2 ] (take 2 [ 1; 2; 3; 4 ]);
  assert_equal [ 1; 2; 3 ] (take 3 [ 1; 2; 3 ]);
  assert_equal [] (take 2 [])
;;

let test_sort_interpolations _ =
  let interpolations =
    [ { name = "linear"; window_size = 2; interpolate = (fun _ _ -> 0.0) }
    ; { name = "lagrange"; window_size = 3; interpolate = (fun _ _ -> 0.0) }
    ]
  in
  let sorted = sort_interpolations interpolations in
  assert_equal [ "linear"; "lagrange" ] (List.map (fun i -> i.name) sorted)
;;

let test_compute_x_min _ =
  assert_equal 1.0 (compute_x_min None [ 1.0, 2.0 ] 0.5);
  assert_equal 2.5 (compute_x_min (Some (Some 2.0)) [ 1.0, 2.0; 3.0, 4.0 ] 0.5);
  assert_equal 1.0 (compute_x_min (Some None) [ 1.0, 2.0 ] 0.5)
;;

let test_apply_interp _ =
  let interp = { name = "linear"; window_size = 2; interpolate = (fun _ _ -> 1.0) } in
  let result = apply_interp [ 0.0, 0.0; 1.0, 1.0 ] [ interp ] 0.5 [] in
  assert_equal [ "linear", Some 0.5 ] result
;;

let test_linear_interpolation _ =
  let points = [ 1.0, 2.0; 3.0, 4.0 ] in
  let result = linear_interpolation.interpolate points 2.0 in
  assert_equal 3.0 result ~printer:string_of_float
;;

let test_lagrange_interpolation _ =
  let points = [ 1.0, 1.0; 2.0, 4.0; 3.0, 9.0 ] in
  let result = lagrange_interpolation.interpolate points 1.5 in
  assert_equal 2.25 result ~printer:string_of_float
;;

let suite =
  "Lab3 Tests"
  >::: [ "test_gen_data" >:: test_gen_data
       ; "test_take" >:: test_take
       ; "test_sort_interpolations" >:: test_sort_interpolations
       ; "test_compute_x_min" >:: test_compute_x_min
       ; "test_apply_interp" >:: test_apply_interp
       ; "test_linear_interpolation" >:: test_linear_interpolation
       ; "test_lagrange_interpolation" >:: test_lagrange_interpolation
       ]
;;

let () = run_test_tt_main suite
