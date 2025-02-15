type interpolation =
  { name : string
  ; window_size : int
  ; interpolate : (float * float) list -> float -> float
  }

let linear_interpolate points x =
  match points with
  | [ (x0, y0); (x1, y1) ] -> ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0
  | _ -> failwith "Invalid set of points"
;;

let lagrange_interpolate points x =
  List.fold_left
    (fun acc (xi, yi) ->
       acc
       +. (yi
           *. List.fold_left (fun prod (xj, _) -> if xj = xi then prod else prod *. (x -. xj) /. (xi -. xj)) 1. points))
    0.
    points
;;

let linear_interpolation = { name = "Linear"; window_size = 2; interpolate = linear_interpolate }
let lagrange_interpolation = { name = "Lagrange"; window_size = 3; interpolate = lagrange_interpolate }
