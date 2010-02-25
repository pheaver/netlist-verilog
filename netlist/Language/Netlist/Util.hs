-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
--
-- Utility functions for Netlist, particularly for generating AST elements.
-- -----------------------------------------------------------------------------

module Language.Netlist.Util where

import Language.Netlist.AST

-- -----------------------------------------------------------------------------

data Direction = Up | Down

makeRange :: Direction -> Size -> Maybe Range
makeRange _ 1 = Nothing
makeRange d sz
  | sz > 1
  = let upper = ExprNum (fromIntegral (sz - 1))
        lower = ExprNum 0
    in Just $ case d of
                Up    -> Range lower upper
                Down  -> Range upper lower

  | otherwise
  = error ("makeRange: invalid size: " ++ show sz)

-- a couple useful functions
exprConcat :: [Expr] -> Expr
exprConcat [e] = e
exprConcat es  = ExprConcat es

statements :: [Stmt] -> Stmt
statements [x] = x
statements xs  = Seq xs

-- -----------------------------------------------------------------------------

-- generate a process declaration for a generic register based on the following:
--  * the register name (as an expression)
--  * clock expression
--  * width of the register
--  * optional asynchronous reset and initial value
--  * optional clock enable
--  * optional synchronous restart and initial value
--  * optional load enable
--  * when enabled, the expression to assign to the identifier
-- You can implement a shift register by passing in a concatenation for the
-- register expression and the input expression, though that is not compatible
-- with VHDL.
-- TODO
--  * support negative-edge triggered clock/reset, active-low reset/restart
--  * support true clock enable (as opposed to load enable)?

generateReg :: Expr -> Expr -> Maybe (Expr, Expr) -> Maybe (Expr, Expr) ->
               Maybe Expr -> Expr -> Decl
generateReg x clk mb_reset mb_restart mb_enable expr
  = ProcessDecl as
  where
    as = case mb_reset of
           Just (reset, initial)
             -> [ (Event reset PosEdge, Assign x initial), a0]
           Nothing
             -> [a0]

    a0 = (Event clk PosEdge, stmt2)

    stmt2 = case mb_restart of
              Just (restart, initial)
                -> If restart (Assign x initial) (Just stmt1)
              Nothing
                -> stmt1

    stmt1 = case mb_enable of
              Just enable  -> If enable stmt0 Nothing
              Nothing      -> stmt0

    stmt0 = Assign x expr

-- -----------------------------------------------------------------------------
