
(let rec iaf_mul x n =
   (*int * float -> float を足し算でやる、nは最大256 *)
  if n=1 then
    x
  else 
    iaf_mul (x+8388608.0) (n-1)
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

(let rec sub1_mod m x =
     if x >= p then
       sub1_mod (2*m) x
     else
       m
);

(let rec sub2_mod p m x =
   if x >= m then
     if x >= p then
       sub2_mod (p/2) m (x-p)
     else
       sub2_mod (p/2) m  x
   else
     x
);

(let modulo m x =
   let p = sub1_mod m x in
   sub2_mod p m x
);

(let rec itof i =
   let ai = if i < 0 then -i else i in
   if ai < 8388608 then
x     let fi = i2f (ai + 8388608) in
     let ans = fi - 8388608.0 in
     if i < 0 then
       -ans
     else 
       ans
         
   else
     let n = (ai / 8388608) in
     let m = ai%8388608 in
     let ans = (iaf_mul 8388608.0 n) + itof(m) in
     if i < 0 then
       -ans
     else
       ans
);

(let floor x =
   let ax = if x < 0 then -x else x in
   if ax >= 8388608.0 then 
     x
   else
     let xadded = ax + 8388608.0 in
     let xsubed = xadded - 8388608.0 in
     let xx = if x < 0 then -xsubed else xsubed in
     if x < xx then
       xx-1.0
     else
       xx
);
     
