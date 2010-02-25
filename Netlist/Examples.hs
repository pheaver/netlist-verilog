-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
-- -----------------------------------------------------------------------------

{-# LANGUAGE ParallelListComp #-}

module Netlist.Examples where

import Netlist.AST
import Netlist.Util

-- -----------------------------------------------------------------------------

t :: Module
t = Module "foo" (f ins) (f outs) ds
  where
    f xs = [ (x, makeRange Down sz) | (x, sz) <- xs ]
    ins = [("clk", 1), ("reset", 1), ("enable", 1), ("x", 16)]
    outs = [("z", 16)]

ds :: [Decl]
ds = [ NetDecl "a" (makeRange Down 16) (Just (ExprVar "x"))
     , NetDecl "b" (makeRange Down 16) (Just (ExprLit 16 10))
     , MemDecl "c" Nothing (makeRange Down 16)
     , ProcessDecl
       [ (Event (ExprVar "reset") PosEdge, Assign (ExprVar "c") (ExprLit 16 0))
       , (Event (ExprVar "clk") PosEdge, If (ExprVar "enable")
                                  (Assign (ExprVar "c") (ExprVar "x"))
                                  Nothing)
       ]
     ]

var_exprs :: [Expr]
var_exprs = [ ExprVar [x] | x <- "abcdefghijklmnopqrstuvwxyz" ]

stmts :: [Stmt]
stmts = [ Assign x (ExprNum i) | x <- var_exprs | i <- [0..] ]

if0 :: Stmt
if0 = If e0 s0 $ Just $
      If e1 s1' $ Just $
      If e2 s2' $ Just s3'
  where
    s1' = Seq [s1, s2, s3]
    s2' = Seq [s4, s5, s6]
    s3' = s7
    (e0:e1:e2:_) = var_exprs
    (s0:s1:s2:s3:s4:s5:s6:s7:_) = stmts

if1 :: Stmt
if1 = If e0 (If e1 s1 Nothing) (Just (If e2 s2 Nothing))
  where
    (e0:e1:e2:_) = var_exprs
    (_:s1:s2:_) = stmts

-- -----------------------------------------------------------------------------
