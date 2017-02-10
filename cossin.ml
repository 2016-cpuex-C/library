let rec sub1_red2pi p x =
     if x >= p then
       sub1_red2pi (2.0*.p) x
     else
       p
in

let rec sub2_red2pi p x =
   if x >= 2.0*.3.141592 then
     if x >= p then
       sub2_red2pi (p/.2.0) (x-.p)
     else
       sub2_red2pi (p/.2.0) x
   else
     x 
in

let rec reduction_to_2pi x =
   let p = 2.0*.3.14159265 in
   let pp = sub1_red2pi p x in
   sub2_red2pi pp x
in


let rec kernel_cos x =
   let x2 = x*.x in
   let x4 = x2*.x2 in
   let x6 = x2*.x4 in

   1.0 -. (0.499998521812 *. x2) +. (0.041668949132*.x4) -. (0.001386642456*.x6)
in

let rec kernel_sin x =
  let x2 = x *. x in
  let x3 = x *. x2 in
  let x5 = x2 *. x3 in
  let x7 = x2 *. x5 in
  x -. (0.16666625976 *. x3) +. (0.008333557129 *. x5) -. (0.000198896484 *. x7)
in

let rec reduction_to_quopi x cs =
   (* cs=true then sin , cs=false then cos *)
  
   if x >= 3.14159265 then
     -. reduction_to_quopi (x -. 3.14159265) cs
   else 
     if x >= (3.141592 /. 2.0) then
       if cs then
         -. reduction_to_quopi (3.141592 -. x) cs
       else 
         reduction_to_quopi (3.141592-.x) cs
     else 
       if x > (3.14159265 /. 4.0) then
         if cs then
           kernel_cos  ((3.14159265 /. 2.0) -. x)
         else 
           kernel_sin  ((3.14159265 /. 2.0) -. x)
       else
         if cs then
           kernel_cos x
         else
           kernel_sin x
in

let rec cos x =
   if x < 0.0 then
     let x_red2pi = reduction_to_2pi (-.x) in
     reduction_to_quopi x_red2pi false
   else
     let x_red2pi = reduction_to_2pi x in
     reduction_to_quopi x_red2pi false
in

let rec sin x =
   if x < 0.0 then
     let x_red2pi = reduction_to_2pi (-.x) in
     -. reduction_to_quopi x_red2pi true
   else 
     let x_red2pi = reduction_to_2pi x in
     reduction_to_quopi x_red2pi true
in

()
