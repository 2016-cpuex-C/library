(let rec iaf_mul x n =
   (*int * float -> float を足し算でやる、nは最大256 *)
  if n=1 then
    x
  else
    iaf_mul (x + 8388608.0) (n - 1)
);

(let rec sub_ftoi ax m =
   (* 最高256回　再帰される *)
   if ax < 8388608.0 then
     m
   else
     ftoi (ax - 8388608.0) (m+1)
);

(let rec ftoi x =
   let ax = if x < 0 then -x else x in
   if ax >= 2147483648.0 then
     2147483647
   else

     if ax < 8388608.0 then
       let xx = ax + 8388608.0 in
     if x < 0 then
       -(f2i xx) + 8388608
     else
       (f2i xx) - 8388608
     else
       let m = sub_ftoi ax 0 in
       if x < 0 then
         -(m*8388608 + ftoi(ax - (iaf_mul 8388608.0 m)) (* よくない *)
       else
         m*8388608 + ftoi(ax - (iaf_mul 8388608.0 m)
                         );















(*
(let rec ftoi x =
   let ax = if x < 0 then -x else x in
   if ax >= 2147483648.0 then
     2147483647
   else
     if ax < 8388608.0 then
       let xx = ax + 8388608.0 in
       if x < 0 then
       -(f2i xx) + 8388608
       else 
         (f2i xx) - 8388608
     else
       let m = sub_ftoi ax 0 in
       if x < 0 then
         -(m*8388608 + ftoi(ax - m * 8388608.0))
       else
         m*8388608 + ftoi(ax - m * 8388608.0)
);
:)
