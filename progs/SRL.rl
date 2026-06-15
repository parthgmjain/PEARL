// SRL interpreter v3: four step sequence

(CurVal1 CurVal2 CurVal3 CurVal4 ConstVal1 ConstVal2 ConstVal3 ConstVal4)
-> (CurVal1 CurVal2 CurVal3 CurVal4 ConstVal1 ConstVal2 ConstVal3 ConstVal4)

init: entry
      CurVal1 += ConstVal1
      goto step2

step2: from init
       CurVal2 += ConstVal2
       goto step3

step3: from step2
       CurVal3 += ConstVal3
       goto step4

step4: from step3
       CurVal4 += ConstVal4
       exit