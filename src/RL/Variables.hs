module RL.Variables where

import RL.AST
import RL.Values

import Data.List (union)

nonInput :: VariableDecl -> [Name]
nonInput decl = filter (`notElem` input decl) $ allVars decl

nonOutput :: VariableDecl -> [Name]
nonOutput decl = filter (`notElem` output decl) $ allVars decl

allVars :: VariableDecl -> [Name]
allVars decl = input decl `union` output decl `union` temp decl

getVarsPat :: Pattern -> [Name]
getVarsPat (QConst _) = []
getVarsPat (QVar n) = [n]
getVarsPat (QPair q1 q2) = getVarsPat q1 `union` getVarsPat q2
getVarsPat (QIndex n e) = n : getVarsExp e

getNonIndexedVars :: Pattern -> [Name]
getNonIndexedVars (QConst _) = []
getNonIndexedVars (QVar n) = [n]
getNonIndexedVars (QPair q1 q2) = getNonIndexedVars q1 `union` getNonIndexedVars q2
getNonIndexedVars (QIndex _ _) = []

getNonExprVars :: Pattern -> [Name]
getNonExprVars (QConst _) = []
getNonExprVars (QVar n) = [n]
getNonExprVars (QPair q1 q2) = getNonExprVars q1 `union` getNonExprVars q2
getNonExprVars (QIndex n _) = [n]

getVarsExp :: Expr -> [Name]
getVarsExp (Const _) = []
getVarsExp (Var n) = [n]
getVarsExp (Op _ e1 e2) = getVarsExp e1 `union` getVarsExp e2
getVarsExp (UOp _ e) = getVarsExp e
