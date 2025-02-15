# Лабораторная работа 3. OCaml

- Автор: Рогачев Михаил Сергеевич P34082

- Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

## Условие задания

В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, link);
- настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:

- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

## Реализация

### Алгоритмы интерполяции 

В рамках лабораторной работы реализованы следующие алгоритмы:

- Линейная интерполяция (обязательная)
- Интерполяция по Лагранжу

### Взаимодействие с программой 

Программа поддерживает следующие флаги:

- --linear - работа с линейной интерполяцией
- --lagrange - работа с интерполяцией по Лагранжу
- -s <"шаг"> - величина шага

### Код

- Реализация алгоритмов интерполяции

```Ocaml
let linear_interpolate points x =
  match points with
  | [ (x0, y0); (x1, y1) ] -> ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0
  | _ -> failwith "Invalid set of points"
;;

let lagrange_interpolate points x =
  List.fold_left
    (fun acc (xi, yi) ->
      acc
      +. (yi *. List.fold_left (fun prod (xj, _) -> if xj = xi then prod else prod *. (x -. xj) /. (xi -. xj)) 1. points))
    0.
    points
;;
```

- Тип runner

```Ocaml
type runner =
  { step : float
  ; points_stream : (float * float) Seq.t
  ; interpolation_types : interpolation list
  ; last_interpolated_x : (string * float option) list
  }
```

### Тесты
```OCaml
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

```

### Примеры работы

```bash
dune exec lab3 -- --linear -s 0.7
< 0 0                              
< 1 1
Linear
> 0.00 0.00
> 0.70 0.70
< 2 2
Linear
> 1.40 1.40
< 3 3
Linear
> 2.10 2.10
> 2.80 2.80
< 4 4
Linear
> 3.50 3.50
...
```

## Вывод
В ходе работы я написал программу потоковой обработки и интерполяции данных на языке OCaml.