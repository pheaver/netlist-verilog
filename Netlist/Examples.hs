-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
-- -----------------------------------------------------------------------------

module Netlist.Examples where

import Netlist.AST
--import Operators

-- -----------------------------------------------------------------------------

t :: Module
t = Module "foo" [("clk", 1), ("reset", 1), ("enable", 1), ("x", 16)] [("z", 16)] ds

ds :: [Decl]
ds = [ NetDecl "a" 16 (Just (ExprVar "x"))
     , NetDecl "b" 16 (Just (ExprNum (Just 16) 10))
     , MemDecl "c" 16
     , ProcessDecl
       [ (Event "reset" PosEdge, Assign (ExprVar "c") (ExprNum (Just 16) 0))
       , (Event "clk" PosEdge, If (ExprVar "enable")
                                  (Assign (ExprVar "c") (ExprVar "x"))
                                  Nothing)
       ]
     ]

-- -----------------------------------------------------------------------------
