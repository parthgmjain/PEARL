// SRL interpreter v2: two steps x += c1 then y += c2

(CurVal1 CurVal2 ConstVal1 ConstVal2) -> (CurVal1 CurVal2 ConstVal1 ConstVal2)

init: entry
      CurVal1 += ConstVal1
      goto step2

step2: from init
       CurVal2 += ConstVal2
       exit