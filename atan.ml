let rec kernel_atan x =
  let x2 = x *. x in
  let x3 = x *. x2 in
  let x5 = x2 *. x3 in
  let x7 = x2 *. x5 in
  let x9 = x2 *. x7 in
  let x11 = x2 *. x9 in
  let x13 = x2 *. x11 in
  x -. (0.333333 *. x3) +. (0.2 *. x5) -. (0.142857142 *. x7) +. (0.111111104 *. x9) -. (0.08976446 *. x11) +. (0.060035485 *. x13)
in

let rec atan x =
   if and (x >= -0.4375) (x <= 0.4375) then
     kernel_atan x
   else
     let ax = if x < 0.0 then -.x else x in
     if x < 0.0 then
       if ax <= 2.4375 then
         -.((3.141592 /. 4.0) +. kernel_atan ((ax-.1.0) /. (ax +. 1.0)))
       else
         -.((3.141592 /. 2.0) +. kernel_atan (1.0/.ax))
     else
       if ax <= 2.4375 then
         (3.141592 /. 4.0) +. kernel_atan ((ax-.1.0) /. (ax +. 1.0))
       else
         (3.141592 /. 2.0) +. kernel_atan (1.0/.ax)
in

print_float (atan 1.0)
