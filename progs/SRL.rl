// SRL interpreter: ADD and SUB on variables N and V
// Program: list of (ADDE . (varname . constval)) or (SUBE . (varname . constval))
// Supported variables: N, V

(Program N V) -> (Program N V)
with (ProgRev Step Tag VarName ConstVal)

init: entry
      goto act1

act1: fi !ProgRev from init else act2
      (Step . Program) <- Program
      (Tag . (VarName . ConstVal)) <- Step
      if Tag = 'ADDE goto doAdd else doSub

doAdd: from act1
       if VarName = 'N goto addN else addV

addN: from doAdd
      N += ConstVal
      goto addDone

addV: from doAdd
      V += ConstVal
      goto addDone

addDone: fi VarName = 'N from addN else addV
         goto act2

doSub: from act1
       if VarName = 'N goto subN else subV

subN: from doSub
      N -= ConstVal
      goto subDone

subV: from doSub
      V -= ConstVal
      goto subDone

subDone: fi VarName = 'N from subN else subV
         goto act2

act2: fi Tag = 'ADDE from addDone else subDone
      Step <- (Tag . (VarName . ConstVal))
      ProgRev <- (Step . ProgRev)
      if Program goto act1 else reload

reload: fi Program from reload else act2
        (Step . ProgRev) <- ProgRev
        Program <- (Step . Program)
        if ProgRev goto reload else done

done: from reload
      exit