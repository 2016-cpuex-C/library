(let rec sub_itof x n =
   (*int * float -> float を足し算でやる、nが小さいから問題ない *)
  if n=1 then
    x
  else 
    sub_itof (x+8388608.0) (n-1)
);

(let rec itof i =
   let ai = if i < 0 then -i else i in
   if ai < 8388608 then
     let fi = i2f (ai + 8388608) in
     let ans = fi - 8388608.0 in
     if i < 0 then
       -ans
     else 
       ans
   
   else
     let n = (ai / 8388608) in
     let m = ai%8388608 in
     let ans = (sub_itof 8388608.0 n) + itof(m) in (* 暗黙のキャスト *)
     if i < 0 then
       -ans
     else
       ans
);
