// SRL Interpreter in RL
// Interprets a subset of SRL: sequences, loops, and step operations.
//
// SRL program representation:
//   (STEP . (ADDE . (x . e)))        -- x += e
//   (STEP . (SUBE . (x . e)))        -- x -= e
//   (STEP . (XORE . (x . e)))        -- x ^= e
//   (STEP . (SKIP . nil))            -- skip
//   (SEQ  . (b1 . b2))               -- sequence b1 then b2
//   (LOOP . (e1 . (b1 . (b2 . e2)))) -- from e1 do b1 loop b2 until e2
//
// Expressions: (CONST . v) or (VAR . name)
// Store: list of (name . value) pairs e.g. '((n . 5) . ((v . 0) . ((w . 1) . nil)))
// Continuation stack entries:
//   b                                          -- any block to execute next
//   (LOOPBACK . (e1 . (b1 . (b2 . e2))))      -- check exit condition

(Program Store) -> (Program Store)
with (Block Cont Tag B1 B2 Op Var E1 E2
      StoreRev CurName CurVal Val EntryName)

init: entry
      Cont <- (Program . nil)
      goto fetch

// ============================================================
// FETCH: pop next block from continuation stack
// ============================================================
fetch: fi Cont from done else fetch
       (Block . Cont) <- Cont
       if hd Block = 'SEQ goto doSeq
       else if hd Block = 'STEP goto doStep
       else if hd Block = 'LOOP goto doLoopEntry
       else doLoopBack

done: fi !Cont from fetch else done
      (Program . nil) <- Cont
      exit

// ============================================================
// SEQ: push b2 as continuation, then execute b1
// ============================================================
doSeq: fi hd Block = 'SEQ from fetch else doStep
       (Tag . (B1 . B2)) <- Block
       Cont <- (B2 . Cont)
       Block <- B1
       goto fetch

// ============================================================
// STEP: unpack and dispatch on operation tag
// ============================================================
doStep: fi hd Block = 'STEP from doSeq else doLoopEntry
        (Tag . Op) <- Block
        if hd Op = 'SKIP goto doSkip
        else if hd Op = 'ADDE goto doAdd
        else if hd Op = 'SUBE goto doSub
        else doXor

doSkip: fi hd Op = 'SKIP from doStep else doAdd
        (Tag . Op) -> Block
        goto fetch

// ============================================================
// HELPER: evaluate expression e into Val
// For VAR: iterates store with StoreRev pattern
// After: Val holds result, store is unchanged
// ============================================================

// ============================================================
// ADD: x += e
// ============================================================
doAdd: fi hd Op = 'ADDE from doSkip else doSub
       (Tag . (Var . E1)) <- Block
       if hd E1 = 'CONST goto addConst else addVar

addConst: fi hd E1 = 'CONST from doAdd else addVar
          (Tag . Val) <- E1
          goto addStore

addVar: fi hd E1 = 'VAR from addConst else addStore
        (Tag . EntryName) <- E1
        StoreRev <- nil
        goto addVarLoop

addVarLoop: fi !StoreRev && EntryName = hd (tl E1) from addVar else addVarNext
            ((CurName . CurVal) . Store) <- Store
            if CurName = EntryName goto addVarHit else addVarNext

addVarNext: fi CurName = EntryName from addVarHit else addVarLoop
            StoreRev <- ((CurName . CurVal) . StoreRev)
            goto addVarLoop

addVarHit: fi CurName = EntryName from addVarLoop else addVarNext
           Val ^= CurVal
           StoreRev <- ((CurName . CurVal) . StoreRev)
           goto addVarReload

addVarReload: fi StoreRev from addVarHit else addVarReload
              ((CurName . CurVal) . StoreRev) <- StoreRev
              Store <- ((CurName . CurVal) . Store)
              if StoreRev goto addVarReload else addVarDone

addVarDone: fi !StoreRev from addVarReload else addVarDone
            (Tag . EntryName) -> E1
            goto addStore

addStore: fi hd E1 = 'VAR || hd E1 = 'CONST from addConst else addVarDone
          StoreRev <- nil
          goto addStoreLoop

addStoreLoop: fi !StoreRev from addStore else addStoreNext
              ((CurName . CurVal) . Store) <- Store
              if CurName = Var goto addStoreHit else addStoreNext

addStoreNext: fi CurName = Var from addStoreHit else addStoreLoop
              StoreRev <- ((CurName . CurVal) . StoreRev)
              goto addStoreLoop

addStoreHit: fi CurName = Var from addStoreLoop else addStoreNext
             CurVal += Val
             StoreRev <- ((CurName . CurVal) . StoreRev)
             goto addStoreReload

addStoreReload: fi StoreRev from addStoreHit else addStoreReload
                ((CurName . CurVal) . StoreRev) <- StoreRev
                Store <- ((CurName . CurVal) . Store)
                if StoreRev goto addStoreReload else addDone

addDone: fi !StoreRev from addStoreReload else addDone
         Val ^= Val
         (Tag . (Var . E1)) -> Block
         goto fetch

// ============================================================
// SUB: x -= e
// ============================================================
doSub: fi hd Op = 'SUBE from doAdd else doXor
       (Tag . (Var . E1)) <- Block
       if hd E1 = 'CONST goto subConst else subVar

subConst: fi hd E1 = 'CONST from doSub else subVar
          (Tag . Val) <- E1
          goto subStore

subVar: fi hd E1 = 'VAR from subConst else subStore
        (Tag . EntryName) <- E1
        StoreRev <- nil
        goto subVarLoop

subVarLoop: fi !StoreRev && EntryName = hd (tl E1) from subVar else subVarNext
            ((CurName . CurVal) . Store) <- Store
            if CurName = EntryName goto subVarHit else subVarNext

subVarNext: fi CurName = EntryName from subVarHit else subVarLoop
            StoreRev <- ((CurName . CurVal) . StoreRev)
            goto subVarLoop

subVarHit: fi CurName = EntryName from subVarLoop else subVarNext
           Val ^= CurVal
           StoreRev <- ((CurName . CurVal) . StoreRev)
           goto subVarReload

subVarReload: fi StoreRev from subVarHit else subVarReload
              ((CurName . CurVal) . StoreRev) <- StoreRev
              Store <- ((CurName . CurVal) . Store)
              if StoreRev goto subVarReload else subVarDone

subVarDone: fi !StoreRev from subVarReload else subVarDone
            (Tag . EntryName) -> E1
            goto subStore

subStore: fi hd E1 = 'VAR || hd E1 = 'CONST from subConst else subVarDone
          StoreRev <- nil
          goto subStoreLoop

subStoreLoop: fi !StoreRev from subStore else subStoreNext
              ((CurName . CurVal) . Store) <- Store
              if CurName = Var goto subStoreHit else subStoreNext

subStoreNext: fi CurName = Var from subStoreHit else subStoreLoop
              StoreRev <- ((CurName . CurVal) . StoreRev)
              goto subStoreLoop

subStoreHit: fi CurName = Var from subStoreLoop else subStoreNext
             CurVal -= Val
             StoreRev <- ((CurName . CurVal) . StoreRev)
             goto subStoreReload

subStoreReload: fi StoreRev from subStoreHit else subStoreReload
                ((CurName . CurVal) . StoreRev) <- StoreRev
                Store <- ((CurName . CurVal) . Store)
                if StoreRev goto subStoreReload else subDone

subDone: fi !StoreRev from subStoreReload else subDone
         Val ^= Val
         (Tag . (Var . E1)) -> Block
         goto fetch

// ============================================================
// XOR: x ^= e
// ============================================================
doXor: fi hd Op = 'XORE from doSub else doSkip
       (Tag . (Var . E1)) <- Block
       if hd E1 = 'CONST goto xorConst else xorVar

xorConst: fi hd E1 = 'CONST from doXor else xorVar
          (Tag . Val) <- E1
          goto xorStore

xorVar: fi hd E1 = 'VAR from xorConst else xorStore
        (Tag . EntryName) <- E1
        StoreRev <- nil
        goto xorVarLoop

xorVarLoop: fi !StoreRev && EntryName = hd (tl E1) from xorVar else xorVarNext
            ((CurName . CurVal) . Store) <- Store
            if CurName = EntryName goto xorVarHit else xorVarNext

xorVarNext: fi CurName = EntryName from xorVarHit else xorVarLoop
            StoreRev <- ((CurName . CurVal) . StoreRev)
            goto xorVarLoop

xorVarHit: fi CurName = EntryName from xorVarLoop else xorVarNext
           Val ^= CurVal
           StoreRev <- ((CurName . CurVal) . StoreRev)
           goto xorVarReload

xorVarReload: fi StoreRev from xorVarHit else xorVarReload
              ((CurName . CurVal) . StoreRev) <- StoreRev
              Store <- ((CurName . CurVal) . Store)
              if StoreRev goto xorVarReload else xorVarDone

xorVarDone: fi !StoreRev from xorVarReload else xorVarDone
            (Tag . EntryName) -> E1
            goto xorStore

xorStore: fi hd E1 = 'VAR || hd E1 = 'CONST from xorConst else xorVarDone
          StoreRev <- nil
          goto xorStoreLoop

xorStoreLoop: fi !StoreRev from xorStore else xorStoreNext
              ((CurName . CurVal) . Store) <- Store
              if CurName = Var goto xorStoreHit else xorStoreNext

xorStoreNext: fi CurName = Var from xorStoreHit else xorStoreLoop
              StoreRev <- ((CurName . CurVal) . StoreRev)
              goto xorStoreLoop

xorStoreHit: fi CurName = Var from xorStoreLoop else xorStoreNext
             CurVal ^= Val
             StoreRev <- ((CurName . CurVal) . StoreRev)
             goto xorStoreReload

xorStoreReload: fi StoreRev from xorStoreHit else xorStoreReload
                ((CurName . CurVal) . StoreRev) <- StoreRev
                Store <- ((CurName . CurVal) . Store)
                if StoreRev goto xorStoreReload else xorDone

xorDone: fi !StoreRev from xorStoreReload else xorDone
         Val ^= Val
         (Tag . (Var . E1)) -> Block
         goto fetch

// ============================================================
// LOOP ENTRY: check e1 (assertion), push LOOPBACK, execute b1
// ============================================================
doLoopEntry: fi hd Block = 'LOOP from fetch else doLoopBack
             (Tag . (E1 . (B1 . (B2 . E2)))) <- Block
             if hd E1 = 'CONST goto loopEntConst else loopEntVar

loopEntConst: fi hd E1 = 'CONST from doLoopEntry else loopEntVar
              (Tag . Val) <- E1
              goto loopEntCheck

loopEntVar: fi hd E1 = 'VAR from loopEntConst else loopEntCheck
            (Tag . EntryName) <- E1
            StoreRev <- nil
            goto loopEntVarLoop

loopEntVarLoop: fi !StoreRev && EntryName = hd (tl E1) from loopEntVar else loopEntVarNext
                ((CurName . CurVal) . Store) <- Store
                if CurName = EntryName goto loopEntVarHit else loopEntVarNext

loopEntVarNext: fi CurName = EntryName from loopEntVarHit else loopEntVarLoop
                StoreRev <- ((CurName . CurVal) . StoreRev)
                goto loopEntVarLoop

loopEntVarHit: fi CurName = EntryName from loopEntVarLoop else loopEntVarNext
               Val ^= CurVal
               StoreRev <- ((CurName . CurVal) . StoreRev)
               goto loopEntVarReload

loopEntVarReload: fi StoreRev from loopEntVarHit else loopEntVarReload
                  ((CurName . CurVal) . StoreRev) <- StoreRev
                  Store <- ((CurName . CurVal) . Store)
                  if StoreRev goto loopEntVarReload else loopEntVarDone

loopEntVarDone: fi !StoreRev from loopEntVarReload else loopEntVarDone
                (Tag . EntryName) -> E1
                goto loopEntCheck

loopEntCheck: fi hd E1 = 'CONST || hd E1 = 'VAR from loopEntConst else loopEntVarDone
              assert(Val)
              Val ^= Val
              (Tag . (E1 . (B1 . (B2 . E2)))) -> Block
              Cont <- ((LOOPBACK . (E1 . (B1 . (B2 . E2)))) . Cont)
              Block <- B1
              goto fetch

// ============================================================
// LOOPBACK: after b1, evaluate e2
// e2 true  -> exit loop (assert e2)
// e2 false -> push b2 and new LOOPBACK, continue (assert !e2)
// ============================================================
doLoopBack: fi hd Block = 'LOOPBACK from fetch else doLoopEntry
            (Tag . (E1 . (B1 . (B2 . E2)))) <- Block
            if hd E2 = 'CONST goto loopBackConst else loopBackVar

loopBackConst: fi hd E2 = 'CONST from doLoopBack else loopBackVar
               (Tag . Val) <- E2
               goto loopBackCheck

loopBackVar: fi hd E2 = 'VAR from loopBackConst else loopBackCheck
             (Tag . EntryName) <- E2
             StoreRev <- nil
             goto loopBackVarLoop

loopBackVarLoop: fi !StoreRev && EntryName = hd (tl E2) from loopBackVar else loopBackVarNext
                 ((CurName . CurVal) . Store) <- Store
                 if CurName = EntryName goto loopBackVarHit else loopBackVarNext

loopBackVarNext: fi CurName = EntryName from loopBackVarHit else loopBackVarLoop
                 StoreRev <- ((CurName . CurVal) . StoreRev)
                 goto loopBackVarLoop

loopBackVarHit: fi CurName = EntryName from loopBackVarLoop else loopBackVarNext
                Val ^= CurVal
                StoreRev <- ((CurName . CurVal) . StoreRev)
                goto loopBackVarReload

loopBackVarReload: fi StoreRev from loopBackVarHit else loopBackVarReload
                   ((CurName . CurVal) . StoreRev) <- StoreRev
                   Store <- ((CurName . CurVal) . Store)
                   if StoreRev goto loopBackVarReload else loopBackVarDone

loopBackVarDone: fi !StoreRev from loopBackVarReload else loopBackVarDone
                 (Tag . EntryName) -> E2
                 goto loopBackCheck

loopBackCheck: fi hd E2 = 'CONST || hd E2 = 'VAR from loopBackConst else loopBackVarDone
               if Val goto loopExit else loopAgain

loopExit: fi Val from loopBackCheck else loopAgain
          assert(Val)
          Val ^= Val
          (Tag . (E1 . (B1 . (B2 . E2)))) -> Block
          goto fetch

loopAgain: fi !Val from loopBackCheck else loopExit
           assert(!Val)
           Val ^= Val
           (Tag . (E1 . (B1 . (B2 . E2)))) -> Block
           Cont <- (B2 . ((LOOPBACK . (E1 . (B1 . (B2 . E2)))) . Cont))
           goto fetch
