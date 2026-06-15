// SRL interpreter v1: x += c

(CurVal ConstVal) -> (CurVal ConstVal)

init: entry
      CurVal += ConstVal
      goto stop

stop: from init
      exit