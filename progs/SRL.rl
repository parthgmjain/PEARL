// SRL interpreter: ADD, SUB, XOR on variables N, V, W
// Program: list of steps:
//   (ADDE . (varname . (CONST . val)))   varname += val
//   (ADDE . (varname . (VAR   . var)))   varname += var
//   (SUBE . (varname . (CONST . val)))   varname -= val
//   (SUBE . (varname . (VAR   . var)))   varname -= var
//   (XORE . (varname . (CONST . val)))   varname ^= val
//   (XORE . (varname . (VAR   . var)))   varname ^= var

(Program N V W) -> (Program N V W)
with (ProgRev Step Tag VarName Expr ExprTag ExprVal)

init: entry
      goto act1

act1: fi !ProgRev from init else act2
      (Step . Program) <- Program
      (Tag . (VarName . Expr)) <- Step
      if Tag = 'ADDE goto doAdd else doNotAdd

doNotAdd: from act1
          if Tag = 'SUBE goto doSub else doXor

doAdd: from act1
       if VarName = 'N goto addN else addNotN

addNotN: from doAdd
         if VarName = 'V goto addV else addW

addN: from doAdd
      (ExprTag . ExprVal) <- Expr
      if ExprTag = 'CONST goto addNconst else addNvar

addV: from addNotN
      (ExprTag . ExprVal) <- Expr
      if ExprTag = 'CONST goto addVconst else addVvar

addW: from addNotN
      (ExprTag . ExprVal) <- Expr
      if ExprTag = 'CONST goto addWconst else addWvar

addNconst: from addN
           N += ExprVal
           goto addNdone

addNvar: from addN
         if ExprVal = 'V goto addNfromV else addNfromW

addNfromV: from addNvar
           N += V
           goto addNvarDone

addNfromW: from addNvar
           N += W
           goto addNvarDone

addNvarDone: fi ExprVal = 'V from addNfromV else addNfromW
             goto addNdone

addNdone: fi ExprTag = 'CONST from addNconst else addNvarDone
          Expr <- (ExprTag . ExprVal)
          goto addMerge1

addVconst: from addV
           V += ExprVal
           goto addVdone

addVvar: from addV
         if ExprVal = 'N goto addVfromN else addVfromW

addVfromN: from addVvar
           V += N
           goto addVvarDone

addVfromW: from addVvar
           V += W
           goto addVvarDone

addVvarDone: fi ExprVal = 'N from addVfromN else addVfromW
             goto addVdone

addVdone: fi ExprTag = 'CONST from addVconst else addVvarDone
          Expr <- (ExprTag . ExprVal)
          goto addMerge1

addMerge1: fi VarName = 'N from addNdone else addVdone
           goto addDone

addWconst: from addW
           W += ExprVal
           goto addWdone

addWvar: from addW
         if ExprVal = 'N goto addWfromN else addWfromV

addWfromN: from addWvar
           W += N
           goto addWvarDone

addWfromV: from addWvar
           W += V
           goto addWvarDone

addWvarDone: fi ExprVal = 'N from addWfromN else addWfromV
             goto addWdone

addWdone: fi ExprTag = 'CONST from addWconst else addWvarDone
          Expr <- (ExprTag . ExprVal)
          goto addDone

addDone: fi VarName = 'W from addWdone else addMerge1
         goto act2

doSub: from doNotAdd
       if VarName = 'N goto subN else subNotN

subNotN: from doSub
         if VarName = 'V goto subV else subW

subN: from doSub
      (ExprTag . ExprVal) <- Expr
      if ExprTag = 'CONST goto subNconst else subNvar

subV: from subNotN
      (ExprTag . ExprVal) <- Expr
      if ExprTag = 'CONST goto subVconst else subVvar

subW: from subNotN
      (ExprTag . ExprVal) <- Expr
      if ExprTag = 'CONST goto subWconst else subWvar

subNconst: from subN
           N -= ExprVal
           goto subNdone

subNvar: from subN
         if ExprVal = 'V goto subNfromV else subNfromW

subNfromV: from subNvar
           N -= V
           goto subNvarDone

subNfromW: from subNvar
           N -= W
           goto subNvarDone

subNvarDone: fi ExprVal = 'V from subNfromV else subNfromW
             goto subNdone

subNdone: fi ExprTag = 'CONST from subNconst else subNvarDone
          Expr <- (ExprTag . ExprVal)
          goto subMerge1

subVconst: from subV
           V -= ExprVal
           goto subVdone

subVvar: from subV
         if ExprVal = 'N goto subVfromN else subVfromW

subVfromN: from subVvar
           V -= N
           goto subVvarDone

subVfromW: from subVvar
           V -= W
           goto subVvarDone

subVvarDone: fi ExprVal = 'N from subVfromN else subVfromW
             goto subVdone

subVdone: fi ExprTag = 'CONST from subVconst else subVvarDone
          Expr <- (ExprTag . ExprVal)
          goto subMerge1

subMerge1: fi VarName = 'N from subNdone else subVdone
           goto subDone

subWconst: from subW
           W -= ExprVal
           goto subWdone

subWvar: from subW
         if ExprVal = 'N goto subWfromN else subWfromV

subWfromN: from subWvar
           W -= N
           goto subWvarDone

subWfromV: from subWvar
           W -= V
           goto subWvarDone

subWvarDone: fi ExprVal = 'N from subWfromN else subWfromV
             goto subWdone

subWdone: fi ExprTag = 'CONST from subWconst else subWvarDone
          Expr <- (ExprTag . ExprVal)
          goto subDone

subDone: fi VarName = 'W from subWdone else subMerge1
         goto opMerge

doXor: from doNotAdd
       if VarName = 'N goto xorN else xorNotN

xorNotN: from doXor
         if VarName = 'V goto xorV else xorW

xorN: from doXor
      (ExprTag . ExprVal) <- Expr
      if ExprTag = 'CONST goto xorNconst else xorNvar

xorV: from xorNotN
      (ExprTag . ExprVal) <- Expr
      if ExprTag = 'CONST goto xorVconst else xorVvar

xorW: from xorNotN
      (ExprTag . ExprVal) <- Expr
      if ExprTag = 'CONST goto xorWconst else xorWvar

xorNconst: from xorN
           N ^= ExprVal
           goto xorNdone

xorNvar: from xorN
         if ExprVal = 'V goto xorNfromV else xorNfromW

xorNfromV: from xorNvar
           N ^= V
           goto xorNvarDone

xorNfromW: from xorNvar
           N ^= W
           goto xorNvarDone

xorNvarDone: fi ExprVal = 'V from xorNfromV else xorNfromW
             goto xorNdone

xorNdone: fi ExprTag = 'CONST from xorNconst else xorNvarDone
          Expr <- (ExprTag . ExprVal)
          goto xorMerge1

xorVconst: from xorV
           V ^= ExprVal
           goto xorVdone

xorVvar: from xorV
         if ExprVal = 'N goto xorVfromN else xorVfromW

xorVfromN: from xorVvar
           V ^= N
           goto xorVvarDone

xorVfromW: from xorVvar
           V ^= W
           goto xorVvarDone

xorVvarDone: fi ExprVal = 'N from xorVfromN else xorVfromW
             goto xorVdone

xorVdone: fi ExprTag = 'CONST from xorVconst else xorVvarDone
          Expr <- (ExprTag . ExprVal)
          goto xorMerge1

xorMerge1: fi VarName = 'N from xorNdone else xorVdone
           goto xorDone

xorWconst: from xorW
           W ^= ExprVal
           goto xorWdone

xorWvar: from xorW
         if ExprVal = 'N goto xorWfromN else xorWfromV

xorWfromN: from xorWvar
           W ^= N
           goto xorWvarDone

xorWfromV: from xorWvar
           W ^= V
           goto xorWvarDone

xorWvarDone: fi ExprVal = 'N from xorWfromN else xorWfromV
             goto xorWdone

xorWdone: fi ExprTag = 'CONST from xorWconst else xorWvarDone
          Expr <- (ExprTag . ExprVal)
          goto xorDone

xorDone: fi VarName = 'W from xorWdone else xorMerge1
         goto opMerge

opMerge: fi Tag = 'SUBE from subDone else xorDone
         goto act2

act2: fi Tag = 'ADDE from addDone else opMerge
      Step <- (Tag . (VarName . Expr))
      ProgRev <- (Step . ProgRev)
      if Program goto act1 else reload

reload: fi Program from reload else act2
        (Step . ProgRev) <- ProgRev
        Program <- (Step . Program)
        if ProgRev goto reload else done

done: from reload
      exit